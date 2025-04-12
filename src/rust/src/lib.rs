use extendr_api::prelude::*;
use closure_core::dfs_parallel;

#[extendr]
fn create_combinations(
    mean: f64,
    sd: f64,
    n: i32,
    scale_min: i32,
    scale_max: i32,
    rounding_error_mean: f64,
    rounding_error_sd: f64
) -> Robj {
    // Call dfs_parallel with explicit type parameters
    let result: Vec<Vec<i32>> = dfs_parallel::<f64, i32>(
        mean,
        sd,
        n,
        scale_min,
        scale_max,
        rounding_error_mean,
        rounding_error_sd
    );
    
    // Since result is already Vec<Vec<i32>>, this should convert directly to an R object
    result
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