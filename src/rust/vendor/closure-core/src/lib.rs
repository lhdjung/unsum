//! CLOSURE: complete listing of original samples of underlying raw evidence
//! 
//! Crate closure-core implements the CLOSURE technique for efficiently reconstructing
//! all possible distributions of raw data from summary statistics. It is not
//! about the Rust feature called closure.
//! 
//! The only API users are likely to need is `dfs_parallel()`. This function applies
//! the lower-level `dfs_branch()` in parallel and writes results to disk (currently
//! into a CSV file, but this may change in the future.)
//! 
//! Most of the code was written by Claude 3.5, translating Python code by Nathanael Larigaldie.


use num::{Float, FromPrimitive, Integer, NumCast, ToPrimitive};
use std::collections::VecDeque;
use rayon::prelude::*;

/// Implements range over Rint-friendly generic integer type U
struct IntegerRange<U>
where
    U: Integer + Copy
{
    current: U,
    end: U,
}

impl<U> Iterator for IntegerRange<U>
where 
    U: Integer + Copy
{
    type Item = U;

    /// Increment over U type integers
    fn next(&mut self) -> Option<U> {
        if self.current < self.end {
            let next = self.current;
            self.current = self.current + U::one();
            Some(next) 
        } else {
            None
        }
    }
}

/// Creates an iterator over the space of U type integers 
fn range_u<U: Integer + Copy>(start: U, end: U) -> IntegerRange<U> {
    IntegerRange {current: start, end}
}

// Define the Combination struct
#[derive(Clone)]
struct Combination<T, U> {
    values: Vec<U>,
    running_sum: T,
    running_m2: T,
}

pub fn count_initial_combinations(scale_min: i32, scale_max: i32) -> i32 {
    let range_size = scale_max - scale_min + 1;
    (range_size * (range_size + 1)) / 2
}

// takes in summary statistics, top level function
pub fn dfs_parallel<T, U>(
    mean: T,
    sd: T,
    n: U,
    scale_min: U,
    scale_max: U,
    rounding_error_mean: T,
    rounding_error_sd: T,
) -> Vec<Vec<U>>
where
    T: Float + FromPrimitive + Send + Sync, // suggest renaming to F to indicate float type?
    U: Integer + NumCast + ToPrimitive + Copy + Send + Sync,
{
    // Convert integer `n` to float to enable multiplication with other floats
    let n_float = T::from(U::to_i32(&n).unwrap()).unwrap();
    
    // Target sum calculations
    let target_sum = mean * n_float;
    let rounding_error_sum = rounding_error_mean * n_float;
    
    let target_sum_upper = target_sum + rounding_error_sum;
    let target_sum_lower = target_sum - rounding_error_sum;
    let sd_upper = sd + rounding_error_sd;
    let sd_lower = sd - rounding_error_sd;

    // Convert to usize for range operations
    let n_usize = U::to_usize(&n).unwrap();
    
    // Precomputing scale sums directly on T types 
    let scale_min_sum_t: Vec<T> = (0..n_usize)
        .map(|x| T::from(scale_min).unwrap() * T::from(x).unwrap())
        .collect();
    
    let scale_max_sum_t: Vec<T> = (0..n_usize)
        .map(|x| T::from(scale_max).unwrap() * T::from(x).unwrap())
        .collect();
    
    let n_minus_1 = n - U::one();
    let scale_max_plus_1 = scale_max + U::one();

   // instead of generating the initial combinations using concrete types, we're keeping them in U
    // and T using the iterator for U 
    let combinations = range_u(scale_min, scale_max_plus_1)
    .flat_map(|i| {
        range_u(i, scale_max_plus_1).map(move |j| {
            let initial_combination = vec![i, j];

            // turn the integer type into the float type
            // again, might be good for readability to rename T to F
            let i_float = T::from(i).unwrap();
            let j_float = T::from(j).unwrap();
            let sum = i_float + j_float;
            let current_mean = sum / T::from(2).unwrap();

            let diff_i = i_float - current_mean;
            let diff_j = j_float - current_mean;
            let current_m2 = diff_i * diff_i + diff_j * diff_j;

            (initial_combination, sum, current_m2)
        })
    })
    .collect::<Vec<_>>();

    // Process combinations in parallel
    combinations.par_iter()
        .flat_map(|(combo, running_sum, running_m2)| {
            dfs_branch(
                combo.clone(),
                *running_sum,
                *running_m2,
                n_usize,
                target_sum_upper,
                target_sum_lower,
                sd_upper,
                sd_lower,
                &scale_min_sum_t,
                &scale_max_sum_t,
                n_minus_1,
                scale_max_plus_1,
            )
        })
        .collect()
}

// Collect all valid combinations from a starting point
#[inline]
#[allow(clippy::too_many_arguments)]
fn dfs_branch<T, U>(
    start_combination: Vec<U>,
    running_sum_init: T,
    running_m2_init: T,
    n: usize,  // Use usize for the length
    target_sum_upper: T,
    target_sum_lower: T,
    sd_upper: T,
    sd_lower: T,
    scale_min_sum_t: &[T],
    scale_max_sum_t: &[T],
    _n_minus_1: U,
    scale_max_plus_1: U,
) -> Vec<Vec<U>>
where
    T: Float + FromPrimitive + Send + Sync,
    U: Integer + NumCast + ToPrimitive + Copy + Send + Sync,
{
    let mut stack = VecDeque::with_capacity(n * 2); // Preallocate with reasonable capacity
    let mut results = Vec::new();
    
    stack.push_back(Combination {
        values: start_combination.clone(),
        running_sum: running_sum_init,
        running_m2: running_m2_init,
    });
    
    while let Some(current) = stack.pop_back() {
        if current.values.len() >= n {
            let n_minus_1_float = T::from(n - 1).unwrap();
            let current_std = (current.running_m2 / n_minus_1_float).sqrt();
            if current_std >= sd_lower {
                results.push(current.values);
            }
            continue;
        }

        // Calculate remaining items to add
        let current_len = current.values.len();
        let n_left = n - current_len - 1; // How many more items after the next one
        let next_n = current_len + 1;

        // Get current mean
        let current_mean = current.running_sum / T::from(current_len).unwrap();

        // Get the last value        
        let last_value = current.values[current_len - 1];

        for next_value in range_u(last_value, scale_max_plus_1) {
            let next_value_as_t = T::from(next_value).unwrap();
            let next_sum = current.running_sum + next_value_as_t;
            
            // Safe indexing with bounds check (using usize for indexing)
            if n_left < scale_min_sum_t.len() {
                let minmean = next_sum + scale_min_sum_t[n_left];
                if minmean > target_sum_upper {
                    break; // Early termination - better than take_while!
                }
                
                // Safe indexing with bounds check (using usize for indexing)
                if n_left < scale_max_sum_t.len() {
                    let maxmean = next_sum + scale_max_sum_t[n_left];
                    if maxmean < target_sum_lower {
                        continue;
                    }
                    
                    let next_mean = next_sum / T::from(next_n).unwrap();
                    let delta = next_value_as_t - current_mean;
                    let delta2 = next_value_as_t - next_mean;
                    let next_m2 = current.running_m2 + delta * delta2;
                    
                    let min_sd = (next_m2 / T::from(n - 1).unwrap()).sqrt();
                    if min_sd <= sd_upper {
                        let mut new_values = current.values.clone();
                        new_values.push(next_value);
                        stack.push_back(Combination {
                            values: new_values,
                            running_sum: next_sum,
                            running_m2: next_m2,
                        });
                    }
                }
            }
        }
    }
    results
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_count_initial_combinations() {
        assert_eq!(count_initial_combinations(1, 3), 6);
        assert_eq!(count_initial_combinations(1, 4), 10);
    }
}

