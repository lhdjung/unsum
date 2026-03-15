use closure_core::{
    closure_count, closure_parallel, closure_parallel_streaming, sprite_parallel,
    sprite_parallel_streaming, ModalityAnalysis, ParquetConfig, RestrictionsMinimum,
    RestrictionsOption, ResultListFromMeanSdN, StreamingConfig, FrequencyDist,
};
/// This is part of unsum, an R package that uses extendr for Rust integration
use extendr_api::prelude::*;
use extendr_api::Robj;
use std::collections::HashMap;

/// Local wrapper for ParquetConfig to allow TryFrom<Robj> implementation.
/// This wrapper type is necessary because of Rust's orphan rule - we can only
/// implement traits for types if we own either the trait or the type.
pub struct ParquetConfigR(pub ParquetConfig);

impl TryFrom<Robj> for ParquetConfigR {
    type Error = Error;

    fn try_from(robj: Robj) -> Result<Self> {
        // Extract the fields from the R list/object
        // Assuming the R side passes a list with 'file_path' and 'batch_size' fields
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

        Ok(ParquetConfigR(ParquetConfig {
            file_path,
            batch_size,
        }))
    }
}

/// Local wrapper for StreamingConfig to allow TryFrom<Robj> implementation.
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
        }))
    }
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

/// Helper function to convert ModalityAnalysis to a named R list of three
/// data frames:
///   - `count_ranges`:   (value, count_lo, count_hi) — one row per scale value
///   - `pair_orderings`: (value_a, value_b, resolved, a_greater) — one row per adjacent pair
///   - `conclusion`:     (unimodal, j_shape_low, j_shape_high) — exactly one row
fn modality_analysis_to_robj(ma: &ModalityAnalysis) -> Robj {
    let count_ranges: Robj = data_frame!(
        value    = ma.value.clone(),
        count_lo = ma.count_lo.clone(),
        count_hi = ma.count_hi.clone()
    )
    .into();

    let pair_orderings: Robj = data_frame!(
        value_a   = ma.pair_value_a.clone(),
        value_b   = ma.pair_value_b.clone(),
        resolved  = ma.pair_resolved.clone(),
        a_greater = ma.pair_a_greater.clone()
    )
    .into();

    let conclusion: Robj = data_frame!(
        unimodal    = vec![ma.unimodal],
        j_shape_low  = vec![ma.j_shape_low],
        j_shape_high = vec![ma.j_shape_high]
    )
    .into();

    list!(
        count_ranges   = count_ranges,
        pair_orderings = pair_orderings,
        conclusion     = conclusion
    )
    .into()
}

/// Helper function to convert FrequencyTable to R data frame
/// The frequency table now includes a 'samples' column as the first column
fn frequency_table_to_robj(freq_table: &closure_core::FrequencyTable) -> Robj {
    // Create a data frame with columns: samples, value, f_average, f_absolute, f_relative
    let df = data_frame!(
        samples = freq_table.samples_group().to_vec(),
        value = freq_table.value().to_vec(),
        f_average = freq_table.f_average().to_vec(),
        f_absolute = freq_table.f_absolute().to_vec(),
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
        samples_all = rl.metrics_main.samples_all as f64,
        values_all = rl.metrics_main.values_all as f64
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

    let frequency = frequency_table_to_robj(&rl.frequency);
    let frequency_dist = frequency_dist_to_robj(&rl.frequency_dist);
    let modality_analysis = modality_analysis_to_robj(&rl.modality_analysis);
    let results = results_table_to_robj(&rl.results);

    vec![
        ("metrics_main",      metrics_main),
        ("metrics_horns",     metrics_horns),
        ("frequency",         frequency),
        ("frequency_dist",    frequency_dist),
        ("modality_analysis", modality_analysis),
        ("results",           results),
    ]
}

/// Helper function to convert ResultsTable to R list
/// Returns a simple list that can be converted to a data frame on the R side
fn results_table_to_robj(results_table: &closure_core::ResultsTable<i32>) -> Robj {
    // Clone the id vector (Vec<f64>) for R compatibility
    let id_vec: Vec<f64> = results_table.id.clone();

    // Convert each sample to an R integer vector and collect into a list
    let samples_robjs: Vec<Robj> = results_table
        .sample
        .iter()
        .map(|sample| {
            // Each sample becomes an R integer vector
            let sample_clone: Vec<i32> = sample.clone();
            sample_clone.into_robj()
        })
        .collect();

    // Create a list of samples (each element is a vector)
    let samples_list: Robj = samples_robjs.into_robj();

    // Get the horns values as a numeric vector
    let horns_vec = results_table.horns.clone();

    // Build a data frame with id, sample (list-column), and horns
    let n_rows = id_vec.len();
    let mut df: Robj = list!(id = id_vec, sample = samples_list, horns = horns_vec).into();
    df.set_attrib("class", "data.frame").unwrap();
    df.set_attrib(
        "row.names",
        (1..=n_rows as i32).collect::<Vec<i32>>(),
    )
    .unwrap();

    df
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
    let count = closure_count(
        mean,
        sd,
        n,
        scale_min,
        scale_max,
        rounding_error_mean,
        rounding_error_sd,
    );

    Robj::from(count)
}

#[extendr]
fn create_empty_results(scale_min: i32, scale_max: i32) -> Robj {
    let empty = ResultListFromMeanSdN::empty(scale_min, scale_max);
    let pairs = result_list_to_pairs(&empty);
    Robj::from(List::from_pairs(pairs))
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
    // Validate and parse SPRITE-specific parameters
    let technique_upper = technique.to_uppercase();

    let (restrict_exact_parsed, restrict_min_parsed) = if technique_upper == "SPRITE" {
        // Parse restrictions for SPRITE
        let exact = match parse_restrict_exact(&restrict_exact) {
            Ok(e) => e,
            Err(e) => return Robj::from(format!("Error parsing restrict_exact: {}", e)),
        };

        let minimum = match parse_restrict_min(&restrict_min) {
            Ok(m) => m,
            Err(e) => return Robj::from(format!("Error parsing restrict_min: {}", e)),
        };

        (exact, minimum)
    } else {
        (None, RestrictionsOption::Null)
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
        let result = match technique_upper.as_str() {
            "CLOSURE" => closure_parallel_streaming(
                mean,
                sd,
                n,
                scale_min,
                scale_max,
                rounding_error_mean,
                rounding_error_sd,
                1,
                streaming_config,
                stop_after,
            ),
            "SPRITE" => {
                let items_val = items
                    .ok_or_else(|| {
                        return format!("items is required for SPRITE technique");
                    })
                    .unwrap_or_else(|_| {
                        // Return error if items is missing
                        return 2; // Default fallback
                    });

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
            }
            _ => {
                return Robj::from(format!(
                    "Unknown technique: {}. Must be 'CLOSURE' or 'SPRITE'",
                    technique
                ));
            }
        };

        // Return information about the streaming operation as an R list
        let result = match result {
            Ok(r) => r,
            Err(e) => {
                return Robj::from(format!("Streaming error: {:?}", e));
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
    let closure_results = match technique_upper.as_str() {
        "CLOSURE" => {
            match closure_parallel(
                mean,
                sd,
                n,
                scale_min,
                scale_max,
                rounding_error_mean,
                rounding_error_sd,
                1,
                None, // No parquet config - just return results in memory
                stop_after,
            ) {
                Ok(results) => results,
                Err(e) => {
                    return Robj::from(format!("CLOSURE error: {:?}", e));
                }
            }
        }
        "SPRITE" => {
            let items_val = match items {
                Some(val) => val,
                None => {
                    return Robj::from("Error: items is required for SPRITE technique");
                }
            };

            match sprite_parallel(
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
            ) {
                Ok(results) => results,
                Err(e) => {
                    return Robj::from(format!("SPRITE error: {:?}", e));
                }
            }
        }
        _ => {
            return Robj::from(format!(
                "Unknown technique: {}. Must be 'CLOSURE' or 'SPRITE'",
                technique
            ));
        }
    };

    let mut pairs = result_list_to_pairs(&closure_results);
    pairs.push(("streaming_mode", Robj::from(false)));
    Robj::from(List::from_pairs(pairs))
}

// Macro to generate exports.
// This ensures exported functions are registered with R.
// See corresponding C code in `entrypoint.c`.
extendr_module! {
    mod unsum;
    fn create_combinations;
    fn create_empty_results;
    fn count_closure_combinations;
}
