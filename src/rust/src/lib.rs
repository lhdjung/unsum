/// This is part of unsum, an R package that uses extendr for Rust integration

use extendr_api::prelude::*;
use extendr_api::Robj;
use closure_core::{
    closure_parallel,
    closure_parallel_streaming,
    sprite_parallel,
    sprite_parallel_streaming,
    ParquetConfig,
    StreamingConfig,
    RestrictionsMinimum,
    RestrictionsOption,
};
use std::collections::HashMap;
use rand::rng;

/// Local wrapper for ParquetConfig to allow TryFrom<Robj> implementation.
/// This wrapper type is necessary because of Rust's orphan rule - we can only
/// implement traits for types if we own either the trait or the type.
pub struct ParquetConfigR(pub ParquetConfig);

impl TryFrom<Robj> for ParquetConfigR {
    type Error = Error;

    fn try_from(robj: Robj) -> Result<Self> {
        // Extract the fields from the R list/object
        // Assuming the R side passes a list with 'file_path' and 'batch_size' fields
        let file_path = robj.dollar("file_path")?
            .as_str()
            .ok_or_else(|| Error::Other("file_path must be a string".into()))?
            .to_string();

        let batch_size = robj.dollar("batch_size")?
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
        let file_path = robj.dollar("file_path")?
            .as_str()
            .ok_or_else(|| Error::Other("file_path must be a string".into()))?
            .to_string();

        let batch_size = robj.dollar("batch_size")?
            .as_real()
            .ok_or_else(|| Error::Other("batch_size must be numeric".into()))?
            as usize;

        // Optional show_progress field, defaults to true for better user experience
        let show_progress = robj.dollar("show_progress")
            .ok()
            .and_then(|r| r.as_bool())
            .unwrap_or(true);  // Default to true to show progress by default

        Ok(StreamingConfigR(StreamingConfig {
            file_path,
            batch_size,
            show_progress,
        }))
    }
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
    let names = robj.get_attrib("names").ok_or_else(|| {
        Error::Other("restrict_exact must have names".into())
    })?;
    let names_vec: Vec<&str> = names.as_str_vector().ok_or_else(|| {
        Error::Other("restrict_exact names must be strings".into())
    })?;

    for (i, name) in names_vec.iter().enumerate() {
        let key: i32 = name.parse().map_err(|_| {
            Error::Other("restrict_exact names must be integers".into())
        })?;

        let value = robj.index(i + 1)?.as_real().ok_or_else(|| {
            Error::Other("restrict_exact values must be numeric".into())
        })? as usize;

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
    let names = robj.get_attrib("names").ok_or_else(|| {
        Error::Other("restrict_min must have names".into())
    })?;
    let names_vec: Vec<&str> = names.as_str_vector().ok_or_else(|| {
        Error::Other("restrict_min names must be strings".into())
    })?;

    for (i, name) in names_vec.iter().enumerate() {
        let key: i32 = name.parse().map_err(|_| {
            Error::Other("restrict_min names must be integers".into())
        })?;

        let value = robj.index(i + 1)?.as_real().ok_or_else(|| {
            Error::Other("restrict_min values must be numeric".into())
        })? as usize;

        map.insert(key, value);
    }

    Ok(RestrictionsOption::Opt(Some(RestrictionsMinimum::new(map))))
}

/// Helper function to convert ResultsTable to R list
/// Returns a simple list that can be converted to a data frame on the R side
fn results_table_to_robj(results_table: &closure_core::ResultsTable<i32>) -> Robj {
    // Convert id from usize to i32 for R compatibility
    let id_vec: Vec<i32> = results_table.id
        .iter()
        .map(|&id| id as i32)
        .collect();

    // Convert each sample to an R integer vector and collect into a list
    let samples_robjs: Vec<Robj> = results_table.sample
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
    let horns_vec = results_table.horns_values.clone();

    // Return as a simple list with named elements
    // R users can convert this to a data frame using:
    // df <- data.frame(
    //   id = results$id,
    //   samples = I(results$samples),  # I() preserves the list structure
    //   horns = results$horns
    // )
    let results_list = list!(
        id = id_vec,
        samples = samples_list,
        horns = horns_vec
    );

    results_list.into()
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
    n_items: Option<u32>,
    restrict_exact: Robj,
    restrict_min: Robj,
    dont_test: bool,
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

    let need_to_write = !write.is_null();

    if need_to_write {
        // Parse the write parameter as StreamingConfig for streaming mode
        let streaming_config = match StreamingConfigR::try_from(write) {
            Ok(config_wrapper) => config_wrapper.0,
            Err(e) => {
                return Robj::from(format!("Error parsing write configuration: {}", e));
            }
        };

        // Use streaming mode - writes directly to disk without keeping results in memory
        let result = match technique_upper.as_str() {
            "CLOSURE" => {
                closure_parallel_streaming(
                    mean,
                    sd,
                    n,
                    scale_min,
                    scale_max,
                    rounding_error_mean,
                    rounding_error_sd,
                    streaming_config,
                    stop_after,
                )
            }
            "SPRITE" => {
                let n_items_val = n_items.ok_or_else(|| {
                    return format!("n_items is required for SPRITE technique");
                }).unwrap_or_else(|_| {
                    // Return error if n_items is missing
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
                    n_items_val,
                    restrict_exact_parsed,
                    restrict_min_parsed,
                    dont_test,
                    streaming_config,
                    stop_after,
                )
            }
            _ => {
                return Robj::from(format!("Unknown technique: {}. Must be 'CLOSURE' or 'SPRITE'", technique));
            }
        };

        // Return information about the streaming operation as an R list
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
            closure_parallel(
                mean,
                sd,
                n,
                scale_min,
                scale_max,
                rounding_error_mean,
                rounding_error_sd,
                None, // No parquet config - just return results in memory
                stop_after,
            )
        }
        "SPRITE" => {
            let n_items_val = match n_items {
                Some(val) => val,
                None => {
                    return Robj::from("Error: n_items is required for SPRITE technique");
                }
            };

            let mut rng = rng();
            match sprite_parallel(
                mean,
                sd,
                n,
                scale_min,
                scale_max,
                rounding_error_mean,
                rounding_error_sd,
                n_items_val,
                restrict_exact_parsed,
                restrict_min_parsed,
                dont_test,
                None, // No parquet config - just return results in memory
                stop_after,
                &mut rng,
            ) {
                Ok(results) => results,
                Err(e) => {
                    return Robj::from(format!("SPRITE error: {:?}", e));
                }
            }
        }
        _ => {
            return Robj::from(format!("Unknown technique: {}. Must be 'CLOSURE' or 'SPRITE'", technique));
        }
    };

    // Create the main metrics list
    let metrics_main = list!(
        samples_initial = closure_results.metrics_main.samples_initial,
        samples_all = closure_results.metrics_main.samples_all,
        values_all = closure_results.metrics_main.values_all
    );

    // Create the horns metrics list
    let metrics_horns = list!(
        mean = closure_results.metrics_horns.mean,
        uniform = closure_results.metrics_horns.uniform,
        sd = closure_results.metrics_horns.sd,
        cv = closure_results.metrics_horns.cv,
        mad = closure_results.metrics_horns.mad,
        min = closure_results.metrics_horns.min,
        median = closure_results.metrics_horns.median,
        max = closure_results.metrics_horns.max,
        range = closure_results.metrics_horns.range
    );

    // Convert frequency table to R data frame
    let frequency = frequency_table_to_robj(&closure_results.frequency);

    // Convert results table to R data frame
    let results = results_table_to_robj(&closure_results.results);

    // Create the comprehensive result list following the new API structure
    // The order matches the ClosureResults struct: metrics_main, metrics_horns, frequency, results
    let result_list = list!(
        metrics_main = metrics_main,
        metrics_horns = metrics_horns,
        frequency = frequency,
        results = results,
        streaming_mode = false
    );

    result_list.into()
}

// Macro to generate exports.
// This ensures exported functions are registered with R.
// See corresponding C code in `entrypoint.c`.
extendr_module! {
    mod unsum;
    fn create_combinations;
}
