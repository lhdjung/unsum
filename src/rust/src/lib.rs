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

    // Convert the results back to R objects
    results
        .into_iter()
        .map(|vec| vec.into_robj())
        .collect::<Vec<Robj>>()
        .into_robj()
}

// Macro to generate exports.
// This ensures exported functions are registered with R.
// See corresponding C code in `entrypoint.c`.
extendr_module! {
    mod unsum;
    fn create_combinations;
}
