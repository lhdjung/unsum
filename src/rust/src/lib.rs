use closure_core::modality::{ModalityShapes, ShapeClass};
use closure_core::{
    closure_count, closure_parallel, closure_parallel_streaming, sprite_parallel,
    sprite_parallel_streaming, FrequencyDist, ModalityCounts, ModalityPairs, OutputFormat,
    RestrictionsMinimum, RestrictionsOption, ResultListFromMeanSdN, StreamingConfig,
};
/// This is part of unsum, an R package that uses extendr for Rust integration
use extendr_api::prelude::*;
use extendr_api::{Result, Robj};
use std::collections::HashMap;

/// Local wrapper for StreamingConfig to allow TryFrom<Robj> implementation.
/// This wrapper type is necessary because of Rust's orphan rule - we can only
/// implement traits for types if we own either the trait or the type.
pub struct StreamingConfigR(pub StreamingConfig);

impl TryFrom<Robj> for StreamingConfigR {
    type Error = Error;

    fn try_from(robj: Robj) -> Result<Self> {
        // Extract the fields from the R list/object
        let file_path = robj
            .dollar("file_path")?
            .as_str()
            .ok_or_else(|| Error::Other("file_path must be a string".into()))?
            .to_string();

        let batch_size = robj
            .dollar("batch_size")?
            .as_real()
            .ok_or_else(|| Error::Other("batch_size must be numeric".into()))?
            as usize;

        // Optional show_progress field, defaults to true for better user experience
        let show_progress = robj
            .dollar("show_progress")
            .ok()
            .and_then(|r| r.as_bool())
            .unwrap_or(true); // Default to true to show progress by default

        Ok(StreamingConfigR(StreamingConfig {
            file_path,
            batch_size,
            show_progress,
            // counts.parquet: one count column per scale value, then horns
            format: OutputFormat::Counts,
        }))
    }
}

/// Give a list of equal-length columns the attributes of a data frame with
/// `n_rows` rows, using R's compact row names instead of storing 1..n.
fn as_data_frame(mut df: Robj, n_rows: usize) -> Robj {
    df.set_attrib("class", "data.frame").unwrap();
    let row_names: Vec<i32> = if n_rows == 0 {
        Vec::new()
    } else {
        // `c(NA_integer_, -n)`; i32::MIN is R's `NA_integer_`
        vec![i32::MIN, -(n_rows as i32)]
    };
    df.set_attrib("row.names", row_names).unwrap();
    df
}

/// Helper function to convert FrequencyDist to R data frame
fn frequency_dist_to_robj(freq_dist: &FrequencyDist) -> Robj {
    let n_samples_i32: Vec<i32> = freq_dist.n_samples.iter().map(|&x| x as i32).collect();
    let df = data_frame!(
        value = freq_dist.value.clone(),
        count = freq_dist.count.clone(),
        n_samples = n_samples_i32
    );
    df.into()
}

/// (value, count_lo, count_hi) — one row per scale value
fn modality_counts_to_robj(mc: &ModalityCounts) -> Robj {
    data_frame!(
        value = mc.value.clone(),
        count_lo = mc.count_lo.clone(),
        count_hi = mc.count_hi.clone()
    )
    .into()
}

/// (value_a, value_b, resolved, a_greater) — one row per adjacent pair
fn modality_pairs_to_robj(mp: &ModalityPairs) -> Robj {
    data_frame!(
        value_a = mp.value_a.clone(),
        value_b = mp.value_b.clone(),
        resolved = mp.resolved.clone(),
        a_greater = mp.a_greater.clone()
    )
    .into()
}

/// `None` ("not ruled out, but the search was partial") becomes `NA`.
fn to_rbool(x: Option<bool>) -> Rbool {
    x.map_or(Rbool::na(), Rbool::from)
}

/// (can_be_unimodal, can_be_bimodal, j_shape_low, j_shape_high) — exactly one
/// row. Each is `NA` if no such sample was found in a partial search.
fn modality_conclusion_to_robj(ms: &ModalityShapes) -> Robj {
    data_frame!(
        can_be_unimodal = vec![to_rbool(ms.can_be_unimodal())],
        can_be_bimodal  = vec![to_rbool(ms.can_be_multimodal())],
        j_shape_low     = vec![to_rbool(ms.can_be_j_shape_low())],
        j_shape_high    = vec![to_rbool(ms.can_be_j_shape_high())]
    )
    .into()
}

/// Long format, one row per (class, grid value), like `modality_shapes.parquet`.
fn modality_shapes_to_robj(ms: &ModalityShapes, values: &[f64]) -> Robj {
    let (mut class, mut n_samples, mut value, mut count_lo, mut count_hi) =
        (Vec::new(), Vec::new(), Vec::new(), Vec::new(), Vec::new());
    for b in &ms.bounds {
        for (i, (&lo, &hi)) in b.count_lo.iter().zip(&b.count_hi).enumerate() {
            class.push(b.class.as_str());
            n_samples.push(b.n_samples as f64);
            value.push(values[i]);
            count_lo.push(lo);
            count_hi.push(hi);
        }
    }
    data_frame!(
        class = class,
        n_samples = n_samples,
        value = value,
        count_lo = count_lo,
        count_hi = count_hi
    )
    .into()
}

/// One row, same columns as `modality_summary.parquet`.
fn modality_summary_to_robj(ms: &ModalityShapes) -> Robj {
    let mut pairs: Vec<(String, Robj)> = vec![
        ("exhaustive".into(), ms.exhaustive.into()),
        ("n_scanned".into(), (ms.n_scanned as f64).into()),
        ("min_prominence".into(), ms.min_prominence.into()),
        ("deficit_min".into(), (ms.deficit_min as i32).into()),
        ("deficit_mean".into(), ms.deficit_mean.into()),
        ("deficit_max".into(), (ms.deficit_max as i32).into()),
    ];
    for class in ShapeClass::all() {
        pairs.push((format!("n_{}", class.as_str()), (ms.n_of(class) as f64).into()));
    }
    for class in ShapeClass::all() {
        let n = ms.band_n_per_class[class as usize] as f64;
        pairs.push((format!("band_n_{}", class.as_str()), n.into()));
    }
    as_data_frame(List::from_pairs(pairs).into(), 1)
}

/// One row per (rung, class), same columns as `modality_prominence.parquet`.
fn modality_prominence_to_robj(ms: &ModalityShapes) -> Robj {
    let (mut prom, mut counts, mut primary, mut class, mut n_samples) =
        (Vec::new(), Vec::new(), Vec::new(), Vec::new(), Vec::new());
    for rung in &ms.ladder {
        for c in ShapeClass::all() {
            prom.push(rung.min_prominence);
            counts.push(rung.min_prominence_counts as i32);
            primary.push(rung.primary);
            class.push(c.as_str());
            n_samples.push(rung.n_of(c) as f64);
        }
    }
    data_frame!(
        min_prominence = prom,
        min_prominence_counts = counts,
        primary = primary,
        class = class,
        n_samples = n_samples
    )
    .into()
}

/// Helper function to convert FrequencyTable to R data frame
/// The frequency table includes a 'samples' column as the first column
fn frequency_table_to_robj(freq_table: &closure_core::FrequencyTable) -> Robj {
    let df = data_frame!(
        samples = freq_table.samples_group().to_vec(),
        value = freq_table.value().to_vec(),
        f_expected = freq_table.f_expected().to_vec(),
        f_representative = freq_table.f_representative().to_vec(),
        f_relative = freq_table.f_relative().to_vec()
    );

    df.into()
}

/// Helper function to parse restrict_exact from R object
/// Expects NULL or a named numeric vector/list
fn parse_restrict_exact(robj: &Robj) -> Result<Option<HashMap<i32, usize>>> {
    if robj.is_null() {
        return Ok(None);
    }

    let mut map = HashMap::new();

    // Get keys (names) and values
    let names = robj
        .get_attrib("names")
        .ok_or_else(|| Error::Other("restrict_exact must have names".into()))?;
    let names_vec: Vec<&str> = names
        .as_str_vector()
        .ok_or_else(|| Error::Other("restrict_exact names must be strings".into()))?;

    for (i, name) in names_vec.iter().enumerate() {
        let key: i32 = name
            .parse()
            .map_err(|_| Error::Other("restrict_exact names must be integers".into()))?;

        let value = robj
            .index(i + 1)?
            .as_real()
            .ok_or_else(|| Error::Other("restrict_exact values must be numeric".into()))?
            as usize;

        map.insert(key, value);
    }

    Ok(Some(map))
}

/// Helper function to parse restrict_min from R object
/// Expects NULL or a named numeric vector/list
fn parse_restrict_min(robj: &Robj) -> Result<RestrictionsOption> {
    if robj.is_null() {
        return Ok(RestrictionsOption::Null);
    }

    // Check if it's a special string "default"
    if let Some(s) = robj.as_str() {
        if s == "default" {
            return Ok(RestrictionsOption::Default);
        }
    }

    let mut map = HashMap::new();

    // Get keys (names) and values
    let names = robj
        .get_attrib("names")
        .ok_or_else(|| Error::Other("restrict_min must have names".into()))?;
    let names_vec: Vec<&str> = names
        .as_str_vector()
        .ok_or_else(|| Error::Other("restrict_min names must be strings".into()))?;

    for (i, name) in names_vec.iter().enumerate() {
        let key: i32 = name
            .parse()
            .map_err(|_| Error::Other("restrict_min names must be integers".into()))?;

        let value = robj
            .index(i + 1)?
            .as_real()
            .ok_or_else(|| Error::Other("restrict_min values must be numeric".into()))?
            as usize;

        map.insert(key, value);
    }

    Ok(RestrictionsOption::Opt(Some(RestrictionsMinimum::new(map))))
}

/// Convert ResultListFromMeanSdN to named R list pairs.
/// Callers can extend the returned vector before building the final list.
fn result_list_to_pairs(rl: &ResultListFromMeanSdN<i32>) -> Vec<(&'static str, Robj)> {
    let metrics_main: Robj = data_frame!(
        samples_all = rl.metrics_main.samples_all,
        values_all = rl.metrics_main.values_all
    )
    .into();

    let metrics_horns: Robj = data_frame!(
        mean = rl.metrics_horns.mean,
        uniform = rl.metrics_horns.uniform,
        sd = rl.metrics_horns.sd,
        cv = rl.metrics_horns.cv,
        mad = rl.metrics_horns.mad,
        min = rl.metrics_horns.min,
        median = rl.metrics_horns.median,
        max = rl.metrics_horns.max,
        range = rl.metrics_horns.range
    )
    .into();

    let ms = &rl.modality_shapes;

    vec![
        ("metrics_main", metrics_main),
        ("metrics_horns", metrics_horns),
        ("frequency", frequency_table_to_robj(&rl.frequency)),
        ("frequency_dist", frequency_dist_to_robj(&rl.frequency_dist)),
        ("modality_counts", modality_counts_to_robj(&rl.modality_counts)),
        ("modality_pairs", modality_pairs_to_robj(&rl.modality_pairs)),
        ("modality_conclusion", modality_conclusion_to_robj(ms)),
        (
            "modality_shapes",
            modality_shapes_to_robj(ms, rl.results.counts.grid().values()),
        ),
        ("modality_summary", modality_summary_to_robj(ms)),
        ("modality_prominence", modality_prominence_to_robj(ms)),
        ("results", results_table_to_robj(&rl.results)),
    ]
}

/// Convert ResultsTable to an R data frame with `id`, one integer count column
/// per scale value (named like `v1`, `v1_5`, `vn2`, as in counts.parquet), and
/// `horns`. The counts encode each sample losslessly because samples are
/// sorted, so R holds k + 2 vectors instead of one vector per sample.
fn results_table_to_robj(results_table: &closure_core::ResultsTable<i32>) -> Robj {
    let counts = &results_table.counts;
    let k = counts.k();
    let flat = counts.as_flat();

    let mut pairs: Vec<(&str, Robj)> = Vec::with_capacity(k + 2);
    pairs.push(("id", Robj::from(&results_table.id)));
    for (col, name) in counts.grid().column_names().iter().enumerate() {
        let column: Robj = flat
            .iter()
            .skip(col)
            .step_by(k)
            .map(|&c| c as i32)
            .collect_robj();
        pairs.push((name.as_str(), column));
    }
    pairs.push(("horns", Robj::from(&results_table.horns)));

    as_data_frame(List::from_pairs(pairs).into(), results_table.len())
}

#[extendr]
fn count_closure_combinations(
    mean: f64,
    sd: f64,
    n: i32,
    scale_min: i32,
    scale_max: i32,
    rounding_error_mean: f64,
    rounding_error_sd: f64,
) -> Robj {
    // Invalid input is an error, not a count of 0; report it the way
    // `create_combinations()` does.
    match closure_count(
        mean,
        sd,
        n,
        scale_min,
        scale_max,
        rounding_error_mean,
        rounding_error_sd,
    ) {
        Ok(count) => Robj::from(count),
        Err(e) => Robj::from(format!("CLOSURE error: {}", e)),
    }
}

#[extendr]
fn create_combinations(
    mean: f64,
    sd: f64,
    n: i32,
    scale_min: i32,
    scale_max: i32,
    technique: &str,
    rounding_error_mean: f64,
    rounding_error_sd: f64,
    items: Option<u32>,
    restrict_exact: Robj,
    restrict_min: Robj,
    write: Robj,
    stop_after: Option<usize>,
) -> Robj {
    let technique_upper = technique.to_uppercase();

    if technique_upper != "CLOSURE" && technique_upper != "SPRITE" {
        return Robj::from(format!(
            "Unknown technique: {}. Must be 'CLOSURE' or 'SPRITE'",
            technique
        ));
    }

    // Validate and parse SPRITE-specific parameters
    let (items_val, restrict_exact_parsed, restrict_min_parsed) = if technique_upper == "SPRITE"
    {
        let Some(items_val) = items else {
            return Robj::from("Error: items is required for SPRITE technique");
        };

        let exact = match parse_restrict_exact(&restrict_exact) {
            Ok(e) => e,
            Err(e) => return Robj::from(format!("Error parsing restrict_exact: {}", e)),
        };

        let minimum = match parse_restrict_min(&restrict_min) {
            Ok(m) => m,
            Err(e) => return Robj::from(format!("Error parsing restrict_min: {}", e)),
        };

        (items_val, exact, minimum)
    } else {
        (1, None, RestrictionsOption::Null)
    };

    // Writing mode
    if !write.is_null() {
        // Parse the write parameter as StreamingConfig for streaming mode
        let streaming_config = match StreamingConfigR::try_from(write) {
            Ok(config_wrapper) => config_wrapper.0,
            Err(e) => {
                return Robj::from(format!("Error parsing write configuration: {}", e));
            }
        };

        // Use streaming mode - writes directly to disk without keeping results in memory
        let result = if technique_upper == "CLOSURE" {
            closure_parallel_streaming(
                mean,
                sd,
                n,
                scale_min,
                scale_max,
                rounding_error_mean,
                rounding_error_sd,
                items_val,
                streaming_config,
                stop_after,
            )
        } else {
            sprite_parallel_streaming(
                mean,
                sd,
                n,
                scale_min,
                scale_max,
                rounding_error_mean,
                rounding_error_sd,
                items_val,
                restrict_exact_parsed,
                restrict_min_parsed,
                streaming_config,
                stop_after,
            )
        };

        // Return information about the streaming operation as an R list
        let result = match result {
            Ok(r) => r,
            Err(e) => {
                return Robj::from(format!("Streaming error: {}", e));
            }
        };
        let result_list = list!(
            total_combinations = result.total_combinations,
            file_path = result.file_path,
            streaming_mode = true
        );

        return result_list.into();
    }

    // Default mode: use parallel without writing to disk
    let results = if technique_upper == "CLOSURE" {
        closure_parallel(
            mean,
            sd,
            n,
            scale_min,
            scale_max,
            rounding_error_mean,
            rounding_error_sd,
            items_val,
            None, // No parquet config - just return results in memory
            stop_after,
        )
    } else {
        sprite_parallel(
            mean,
            sd,
            n,
            scale_min,
            scale_max,
            rounding_error_mean,
            rounding_error_sd,
            items_val,
            restrict_exact_parsed,
            restrict_min_parsed,
            None, // No parquet config - just return results in memory
            stop_after,
        )
    };

    let results = match results {
        Ok(results) => results,
        Err(e) => {
            return Robj::from(format!("{} error: {}", technique_upper, e));
        }
    };

    let mut pairs = result_list_to_pairs(&results);
    pairs.push(("streaming_mode", Robj::from(false)));
    Robj::from(List::from_pairs(pairs))
}

// Macro to generate exports.
// This ensures exported functions are registered with R.
// See corresponding C code in `entrypoint.c`.
extendr_module! {
    mod unsum;
    fn create_combinations;
    fn count_closure_combinations;
}
