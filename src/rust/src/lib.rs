use extendr_api::prelude::*;
use extendr_api::Robj;
use closure_core::{
    dfs_parallel,
    ParquetConfig,
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
    // Convert the write parameter to Option<ParquetConfig>
    let write_config = if write.is_null() {
        None
    } else {
        // Try to convert the R object to our wrapper type, then extract the inner ParquetConfig
        match ParquetConfigR::try_from(write) {
            Ok(config_wrapper) => Some(config_wrapper.0),
            Err(e) => {
                // You might want to handle this error differently
                panic!("Invalid ParquetConfig: {}", e);
            }
        }
    };

    // Call the core function with the converted parameters
    let results = dfs_parallel(
        mean,
        sd,
        n,
        scale_min,
        scale_max,
        rounding_error_mean,
        rounding_error_sd,
        write_config,
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
