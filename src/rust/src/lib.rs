use extendr_api::prelude::*;
use closure_core::dfs_parallel;

/// Core CLOSURE implementation, processed for R
#[extendr]
fn create_combinations(
    mean: f64,
    sd: f64,
    n: usize,
    scale_min: i32,
    scale_max: i32,
    rounding_error_mean: f64,
    rounding_error_sd: f64
) -> Robj {
    dfs_parallel(    // let r_list: Vec<Robj> = 
        mean,
        sd,
        n,
        scale_min,
        scale_max,
        rounding_error_mean,
        rounding_error_sd
    )
    .combinations
    .into_iter()
    .map(|vec| vec.into_robj())
    .collect::<Vec<Robj>>()
    .into_robj()
}


/// Convert strings such as "n1" and "n2" to integers such as 1 and 2
#[extendr]
fn n_to_integer(strings: Vec<String>) -> Integers {
    let rust_ints: Vec<i32> = strings
        .into_iter()
        .map(|s| s.trim_start_matches('n').parse().unwrap())
        .collect();
    
    Integers::from_values(rust_ints)
}

// Macro to generate exports.
// This ensures exported functions are registered with R.
// See corresponding C code in `entrypoint.c`.
extendr_module! {
    mod closure;
    fn create_combinations;
    fn n_to_integer;
}
