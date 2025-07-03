//! CLOSURE: complete listing of original samples of underlying raw evidence
//! 
//! Crate closure-core implements the CLOSURE technique for efficiently reconstructing
//! all possible distributions of raw data from summary statistics. It is not
//! about the Rust feature called closure.
//! 
//! The crate is mostly meant to serve as a backend for the R package [unsum](https://lhdjung.github.io/unsum/).
//! The main APIs users need are `dfs_parallel()` for in-memory results and 
//! `dfs_parallel_streaming()` for memory-efficient file output.
//! 
//! Most of the code was written by Claude 3.5, translating Python code by Nathanael Larigaldie.


use num::{Float, FromPrimitive, Integer, NumCast, ToPrimitive};
use std::collections::VecDeque;
use rayon::prelude::*;
use arrow::array::{Int32Array, ArrayRef};
use arrow::datatypes::{Schema, Field, DataType};
use arrow::record_batch::RecordBatch;
use parquet::arrow::ArrowWriter;
use parquet::file::properties::WriterProperties;
use std::fs::File;
use std::sync::Arc;
use std::sync::mpsc::channel;
use std::sync::atomic::{AtomicUsize, Ordering};
use std::thread;

/// Configuration for Parquet output in memory mode
/// Used with `dfs_parallel()` to optionally save results while returning them
pub struct ParquetConfig {
    pub file_path: String,
    pub batch_size: usize,
}

/// Configuration for streaming mode
/// Used with `dfs_parallel_streaming()` for memory-efficient processing
pub struct StreamingConfig {
    pub file_path: String,
    pub batch_size: usize,
    pub show_progress: bool,
}

/// Result of streaming operation
pub struct StreamingResult {
    pub total_combinations: usize,
    pub file_path: String,
}

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

/// Count first set of integers
/// 
/// The first set of integers that can be formed
/// given a range defined by `scale_min` and `scale_max`.
/// This function calculates the number of unique pairs (i, j) where i and j are integers
/// within the specified range, and i <= j.
/// # Arguments
/// * `scale_min` - The minimum value of the scale.
/// * `scale_max` - The maximum value of the scale.
/// # Returns
/// The total number of unique combinations of integers within the specified range.
pub fn count_initial_combinations(scale_min: i32, scale_max: i32) -> i32 {
    let range_size = scale_max - scale_min + 1;
    (range_size * (range_size + 1)) / 2
}

/// Create a Parquet writer with appropriate schema
fn create_parquet_writer<U>(file_path: &str, n: U) -> Result<ArrowWriter<File>, Box<dyn std::error::Error>>
where
    U: Integer + ToPrimitive + Copy,
{
    let n_usize = U::to_usize(&n).unwrap();
    
    // Create schema with n columns named n1, n2, ..., n{n}
    let fields: Vec<Field> = (1..=n_usize)
        .map(|i| Field::new(&format!("n{}", i), DataType::Int32, false))
        .collect();
    
    let schema = Arc::new(Schema::new(fields));
    
    let file = File::create(file_path)?;
    let props = WriterProperties::builder().build();
    let writer = ArrowWriter::try_new(file, schema, Some(props))?;
    
    Ok(writer)
}

/// Convert combinations to a RecordBatch for Parquet writing
fn combinations_to_record_batch<U>(combinations: &[Vec<U>], n: usize) -> Result<RecordBatch, Box<dyn std::error::Error>>
where
    U: Integer + ToPrimitive + Copy,
{
    // Create arrays for each column
    let mut arrays: Vec<ArrayRef> = Vec::new();
    
    for col_idx in 0..n {
        let column_data: Vec<i32> = combinations
            .iter()
            .map(|combo| U::to_i32(&combo[col_idx]).unwrap())
            .collect();
        
        arrays.push(Arc::new(Int32Array::from(column_data)));
    }
    
    // Create schema
    let fields: Vec<Field> = (1..=n)
        .map(|i| Field::new(&format!("n{}", i), DataType::Int32, false))
        .collect();
    
    let schema = Arc::new(Schema::new(fields));
    
    RecordBatch::try_new(schema, arrays).map_err(|e| e.into())
}

/// Internal function to prepare computation parameters
fn prepare_computation<T, U>(
    mean: T,
    sd: T,
    n: U,
    scale_min: U,
    scale_max: U,
    rounding_error_mean: T,
    rounding_error_sd: T,
) -> (T, T, T, T, usize, Vec<Vec<T>>, Vec<T>, U, U)
where
    T: Float + FromPrimitive + Send + Sync,
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
    
    // Create 2D array for min_scale_sum like in Python
    // min_scale_sum[value][n_left] = value * n_left
    let scale_range = U::to_usize(&(scale_max - scale_min + U::one())).unwrap();
    let mut min_scale_sum_t: Vec<Vec<T>> = Vec::with_capacity(scale_range);
    
    // For each possible value from scale_min to scale_max
    for value in range_u(scale_min, scale_max + U::one()) {
        let value_float = T::from(value).unwrap();
        let row: Vec<T> = (0..n_usize)
            .map(|n_left| value_float * T::from(n_left).unwrap())
            .collect();
        min_scale_sum_t.push(row);
    }
    
    // max_scale_sum remains 1D as in the original
    let scale_max_sum_t: Vec<T> = (0..n_usize)
        .map(|x| T::from(scale_max).unwrap() * T::from(x).unwrap())
        .collect();
    
    let n_minus_1 = n - U::one();
    let scale_max_plus_1 = scale_max + U::one();
    
    (target_sum_upper, target_sum_lower, sd_upper, sd_lower, n_usize, 
     min_scale_sum_t, scale_max_sum_t, n_minus_1, scale_max_plus_1)
}

/// Generate initial combinations for parallel processing
fn generate_initial_combinations<T, U>(
    scale_min: U,
    scale_max_plus_1: U,
) -> Vec<(Vec<U>, T, T)>
where
    T: Float + FromPrimitive + Send + Sync,
    U: Integer + NumCast + ToPrimitive + Copy + Send + Sync,
{
    range_u(scale_min, scale_max_plus_1)
        .flat_map(|i| {
            range_u(i, scale_max_plus_1).map(move |j| {
                let initial_combination = vec![i, j];

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
        .collect()
}

/// Generate all valid combinations (memory mode)
/// 
/// This function computes all valid combinations and returns them in memory.
/// Optionally writes to a Parquet file if config is provided.
/// 
/// Use this when:
/// - Result sets are reasonably sized (< 1GB)
/// - You need to process results in memory after generation
/// - You want both file output and in-memory access
/// 
/// For large result sets, use `dfs_parallel_streaming()` instead.
pub fn dfs_parallel<T, U>(
    mean: T,
    sd: T,
    n: U,
    scale_min: U,
    scale_max: U,
    rounding_error_mean: T,
    rounding_error_sd: T,
    parquet_config: Option<ParquetConfig>,
) -> Vec<Vec<U>>
where
    T: Float + FromPrimitive + Send + Sync,
    U: Integer + NumCast + ToPrimitive + Copy + Send + Sync + 'static,
{
    let (target_sum_upper, target_sum_lower, sd_upper, sd_lower, n_usize, 
         min_scale_sum_t, scale_max_sum_t, n_minus_1, scale_max_plus_1) = 
        prepare_computation(mean, sd, n, scale_min, scale_max, rounding_error_mean, rounding_error_sd);

    // Setup channel for sending results to writer thread if configured
    let (tx, rx) = if parquet_config.is_some() {
        let (tx, rx) = channel::<Vec<Vec<U>>>();
        (Some(tx), Some(rx))
    } else {
        (None, None)
    };
    
    // Spawn dedicated writer thread if we have a parquet config
    let writer_handle = if let Some(config) = parquet_config {
        let rx = rx.unwrap();
        let n_for_writer = n;
        
        Some(thread::spawn(move || {
            let mut writer = create_parquet_writer::<U>(&config.file_path, n_for_writer)
                .expect("Failed to create Parquet writer");
            
            let mut buffer: Vec<Vec<U>> = Vec::with_capacity(config.batch_size * 2);
            let mut total_written = 0;
            
            // Process incoming results
            while let Ok(batch) = rx.recv() {
                buffer.extend(batch);
                
                // Write when buffer reaches threshold
                if buffer.len() >= config.batch_size {
                    if let Ok(record_batch) = combinations_to_record_batch(&buffer, U::to_usize(&n_for_writer).unwrap()) {
                        let _ = writer.write(&record_batch);
                        total_written += buffer.len();
                    }
                    buffer.clear();
                }
            }
            
            // Write any remaining results
            if !buffer.is_empty() {
                if let Ok(record_batch) = combinations_to_record_batch(&buffer, U::to_usize(&n_for_writer).unwrap()) {
                    let _ = writer.write(&record_batch);
                    total_written += buffer.len();
                }
            }
            
            let _ = writer.close();
            total_written
        }))
    } else {
        None
    };

    // Generate initial combinations
    let combinations = generate_initial_combinations(scale_min, scale_max_plus_1);

    // Process combinations in parallel
    let results: Vec<Vec<U>> = combinations.par_iter()
        .flat_map(|(combo, running_sum, running_m2)| {
            let branch_results = dfs_branch(
                combo.clone(),
                *running_sum,
                *running_m2,
                n_usize,
                target_sum_upper,
                target_sum_lower,
                sd_upper,
                sd_lower,
                &min_scale_sum_t,
                &scale_max_sum_t,
                n_minus_1,
                scale_max_plus_1,
                scale_min,
            );
            
            // Send results to writer thread if configured
            if let Some(ref sender) = tx {
                // Clone the results to send to writer
                let results_for_writer = branch_results.clone();
                // Send can fail if receiver is dropped, but we ignore it
                let _ = sender.send(results_for_writer);
            }
            
            branch_results
        })
        .collect();
    
    // Close the channel to signal writer thread to finish
    drop(tx);
    
    // Wait for writer thread to complete if it exists
    if let Some(handle) = writer_handle {
        if let Ok(total_written) = handle.join() {
            eprintln!("Total combinations written to Parquet: {}", total_written);
        }
    }
    
    results
}

/// Generate all valid combinations (streaming mode)
/// 
/// This function computes all valid combinations and streams them directly to a Parquet file
/// without keeping them in memory. This is essential for large result sets.
/// 
/// Use this when:
/// - Result sets are very large (> 1GB)
/// - You only need file output, not in-memory processing
/// - Memory efficiency is critical
/// 
/// Returns a StreamingResult with the total count and file path.
pub fn dfs_parallel_streaming<T, U>(
    mean: T,
    sd: T,
    n: U,
    scale_min: U,
    scale_max: U,
    rounding_error_mean: T,
    rounding_error_sd: T,
    config: StreamingConfig,
) -> StreamingResult
where
    T: Float + FromPrimitive + Send + Sync,
    U: Integer + NumCast + ToPrimitive + Copy + Send + Sync + 'static,
{
    let (target_sum_upper, target_sum_lower, sd_upper, sd_lower, n_usize, 
         min_scale_sum_t, scale_max_sum_t, n_minus_1, scale_max_plus_1) = 
        prepare_computation(mean, sd, n, scale_min, scale_max, rounding_error_mean, rounding_error_sd);

    // Setup channel for streaming results
    let (tx, rx) = channel::<Vec<Vec<U>>>();
    
    // Counter for total combinations found
    let total_counter = Arc::new(AtomicUsize::new(0));
    let counter_for_thread = total_counter.clone();
    
    // Spawn dedicated writer thread
    let file_path_clone = config.file_path.clone();
    let n_for_writer = n;
    
    let writer_handle = thread::spawn(move || {
        let mut writer = create_parquet_writer::<U>(&file_path_clone, n_for_writer)
            .expect("Failed to create Parquet writer");
        
        let mut buffer: Vec<Vec<U>> = Vec::with_capacity(config.batch_size * 2);
        let mut total_written = 0;
        let mut last_progress_report = 0;
        
        // Process incoming results
        while let Ok(batch) = rx.recv() {
            let batch_size = batch.len();
            buffer.extend(batch);
            
            // Update counter
            counter_for_thread.fetch_add(batch_size, Ordering::Relaxed);
            
            // Write when buffer reaches threshold
            if buffer.len() >= config.batch_size {
                if let Ok(record_batch) = combinations_to_record_batch(&buffer, U::to_usize(&n_for_writer).unwrap()) {
                    let _ = writer.write(&record_batch);
                    total_written += buffer.len();
                    
                    // Progress reporting
                    if config.show_progress && total_written - last_progress_report >= 100_000 {
                        eprintln!("Progress: {} combinations written...", total_written);
                        last_progress_report = total_written;
                    }
                }
                buffer.clear();
            }
        }
        
        // Write any remaining results
        if !buffer.is_empty() {
            if let Ok(record_batch) = combinations_to_record_batch(&buffer, U::to_usize(&n_for_writer).unwrap()) {
                let _ = writer.write(&record_batch);
                total_written += buffer.len();
            }
        }
        
        let _ = writer.close();
        
        if config.show_progress {
            eprintln!("Streaming complete: {} total combinations written", total_written);
        }
        
        total_written
    });

    // Generate initial combinations
    let combinations = generate_initial_combinations(scale_min, scale_max_plus_1);

    // Process combinations in parallel (without collecting results)
    combinations.par_iter()
        .for_each(|(combo, running_sum, running_m2)| {
            let branch_results = dfs_branch(
                combo.clone(),
                *running_sum,
                *running_m2,
                n_usize,
                target_sum_upper,
                target_sum_lower,
                sd_upper,
                sd_lower,
                &min_scale_sum_t,
                &scale_max_sum_t,
                n_minus_1,
                scale_max_plus_1,
                scale_min,
            );
            
            // Send results directly to writer thread
            // No cloning needed since we're not keeping them
            if !branch_results.is_empty() {
                let _ = tx.send(branch_results);
            }
        });
    
    // Close the channel to signal writer thread to finish
    drop(tx);
    
    // Wait for writer thread to complete
    let total_written = writer_handle.join()
        .expect("Writer thread panicked");
    
    StreamingResult {
        total_combinations: total_written,
        file_path: config.file_path,
    }
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
    min_scale_sum_t: &[Vec<T>],  // Now 2D array
    scale_max_sum_t: &[T],
    _n_minus_1: U,
    scale_max_plus_1: U,
    scale_min: U,  // Need this to calculate indices
) -> Vec<Vec<U>>
where
    T: Float + FromPrimitive + Send + Sync,
    U: Integer + NumCast + ToPrimitive + Copy + Send + Sync,
{
    let mut stack = VecDeque::with_capacity(n * 2);
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
            
            // Calculate index for 2D array access
            // Index is (next_value - scale_min) to map to 0-based array
            let value_index = U::to_usize(&(next_value - scale_min)).unwrap();
            
            // Safe indexing with bounds check
            if value_index < min_scale_sum_t.len() && n_left < min_scale_sum_t[value_index].len() {
                // Access as min_scale_sum_t[next_value-scale_min][n_left]
                let minmean = next_sum + min_scale_sum_t[value_index][n_left];
                if minmean > target_sum_upper {
                    break; // Early termination
                }
                
                // Safe indexing with bounds check
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
    
    #[test]
    fn test_dfs_parallel_without_file() {
        // Test that the function works without file output
        let results = dfs_parallel::<f64, i32>(
            3.0,  // mean
            1.0,  // sd
            5,    // n
            1,    // scale_min
            5,    // scale_max
            0.05, // rounding_error_mean
            0.05, // rounding_error_sd
            None, // no parquet config
        );
        
        // Should return some valid combinations
        assert!(!results.is_empty());
    }
    
    #[test]
    fn test_dfs_parallel_with_file() {
        // Test with Parquet output
        let config = ParquetConfig {
            file_path: "test_output.parquet".to_string(),
            batch_size: 100,
        };
        
        let results = dfs_parallel::<f64, i32>(
            3.0,  // mean
            1.0,  // sd
            5,    // n
            1,    // scale_min
            5,    // scale_max
            0.05, // rounding_error_mean
            0.05, // rounding_error_sd
            Some(config),
        );
        
        assert!(!results.is_empty());
        
        // Clean up test file
        let _ = std::fs::remove_file("test_output.parquet");
    }
    
    #[test]
    fn test_dfs_parallel_streaming() {
        // Test streaming mode
        let config = StreamingConfig {
            file_path: "test_streaming.parquet".to_string(),
            batch_size: 100,
            show_progress: false,
        };
        
        let result = dfs_parallel_streaming::<f64, i32>(
            3.0,  // mean
            1.0,  // sd
            5,    // n
            1,    // scale_min
            5,    // scale_max
            0.05, // rounding_error_mean
            0.05, // rounding_error_sd
            config,
        );
        
        assert!(result.total_combinations > 0);
        assert_eq!(result.file_path, "test_streaming.parquet");
        
        // Clean up test file
        let _ = std::fs::remove_file("test_streaming.parquet");
    }
}