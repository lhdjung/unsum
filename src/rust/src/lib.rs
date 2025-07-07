use extendr_api::prelude::*;
use extendr_api::Robj;
use closure_core::{
    dfs_parallel,
    dfs_parallel_streaming,
    ParquetConfig,
    StreamingConfig,
};

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

        // Optional show_progress field, defaults to false if not provided
        let show_progress = robj.dollar("show_progress")
            .ok()
            .and_then(|r| r.as_bool())
            .unwrap_or(false);

        Ok(StreamingConfigR(StreamingConfig {
            file_path,
            batch_size,
            show_progress,
        }))
    }
}

/// Helper function to convert FrequencyTable to R data frame
fn frequency_table_to_robj(freq_table: &closure_core::FrequencyTable) -> Robj {
    // Create a data frame with columns: value, f_average, f_absolute, f_relative
    let df = data_frame!(
        value = freq_table.value.clone(),
        f_average = freq_table.f_average.clone(),
        f_absolute = freq_table.f_absolute.clone(),
        f_relative = freq_table.f_relative.clone()
    );

    df.into()
}

#[extendr]
fn create_combinations(
    mean: f64,
    sd: f64,
    n: i32,
    scale_min: i32,
    scale_max: i32,
    rounding_error_mean: f64,
    rounding_error_sd: f64,
    write: Robj, // Accept Robj directly
) -> Robj {
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
        let result = dfs_parallel_streaming(
            mean,
            sd,
            n,
            scale_min,
            scale_max,
            rounding_error_mean,
            rounding_error_sd,
            streaming_config,
        );

        // Return information about the streaming operation as an R list
        let result_list = list!(
            total_combinations = result.total_combinations,
            file_path = result.file_path,
            streaming_mode = true
        );

        return result_list.into();
    }

    // Default mode: use dfs_parallel without writing to disk
    let results = dfs_parallel(
        mean,
        sd,
        n,
        scale_min,
        scale_max,
        rounding_error_mean,
        rounding_error_sd,
        None, // No parquet config - just return results in memory
    );

    // Convert the samples to R format
    // Each inner Vec<i32> becomes an R integer vector
    let samples_r: Vec<Robj> = results.samples
        .into_iter()
        .map(|vec| vec.into_robj())
        .collect();

    // Create the main metrics list
    let metrics_main = list!(
        samples_initial = results.metrics_main.samples_initial,
        samples_all = results.metrics_main.samples_all,
        values_all = results.metrics_main.values_all
    );

    // Create the horns metrics list
    let metrics_horns = list!(
        mean = results.metrics_horns.mean,
        uniform = results.metrics_horns.uniform,
        sd = results.metrics_horns.sd,
        cv = results.metrics_horns.cv,
        mad = results.metrics_horns.mad,
        min = results.metrics_horns.min,
        median = results.metrics_horns.median,
        max = results.metrics_horns.max,
        range = results.metrics_horns.range
    );

    // Convert frequency tables to R data frames
    let frequency_all = frequency_table_to_robj(&results.frequency_all);
    let frequency_horns_min = frequency_table_to_robj(&results.frequency_horns_min);
    let frequency_horns_max = frequency_table_to_robj(&results.frequency_horns_max);

    // Create the comprehensive result list
    // This structure provides R users with all the rich information from CLOSURE
    let result_list = list!(
        samples = samples_r,
        horns_values = results.horns_values,
        metrics_main = metrics_main,
        metrics_horns = metrics_horns,
        frequency_all = frequency_all,
        frequency_horns_min = frequency_horns_min,
        frequency_horns_max = frequency_horns_max,
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
