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
use std::collections::{VecDeque, HashMap};
use rayon::prelude::*;
use arrow::array::{Int32Array, Int64Array, Float64Array, StringArray, ArrayRef, ListBuilder, Int32Builder};
use arrow::datatypes::{Schema, Field, DataType};
use arrow::record_batch::RecordBatch;
use parquet::arrow::ArrowWriter;
use parquet::file::properties::WriterProperties;
use std::fs::File;
use std::sync::Arc;
use std::sync::mpsc::channel;
use std::sync::atomic::{AtomicUsize, Ordering};
use std::sync::Mutex;
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

/// Combined frequency data for a set of samples
/// Each row represents frequency data for a specific value in a specific sample group
#[derive(Clone, Debug)]
pub struct FrequencyTable {
    pub samples: Vec<String>,      // "all", "horns_min", or "horns_max"
    pub value: Vec<i32>,
    pub f_average: Vec<f64>,
    pub f_absolute: Vec<i64>,      // Changed to i64 to maintain integer type
    pub f_relative: Vec<f64>,
}

/// Main metrics about the CLOSURE results
#[derive(Clone, Debug)]
pub struct MetricsMain {
    pub samples_initial: i32,
    pub samples_all: usize,
    pub values_all: usize,
}

/// Horns-specific metrics
#[derive(Clone, Debug)]
pub struct MetricsHorns {
    pub mean: f64,
    pub uniform: f64,
    pub sd: f64,
    pub cv: f64,
    pub mad: f64,
    pub min: f64,
    pub median: f64,
    pub max: f64,
    pub range: f64,
}

/// Results table combining samples and their horns values
#[derive(Clone, Debug)]
pub struct ResultsTable<U> {
    pub id: Vec<usize>,
    pub samples: Vec<Vec<U>>,
    pub horns_values: Vec<f64>,
}

/// Complete CLOSURE results with all statistics
#[derive(Clone, Debug)]
pub struct ClosureResults<U> {
    pub metrics_main: MetricsMain,
    pub metrics_horns: MetricsHorns,
    pub frequency: FrequencyTable,
    pub results: ResultsTable<U>,
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

/// Calculate horns index for a frequency distribution
fn calculate_horns(freqs: &[f64], scale_min: i32, scale_max: i32) -> f64 {
    let scale_values: Vec<f64> = (scale_min..=scale_max)
        .map(|v| v as f64)
        .collect();
    
    let total: f64 = freqs.iter().sum();
    if total == 0.0 {
        return 0.0;
    }
    
    let freqs_relative: Vec<f64> = freqs.iter()
        .map(|f| f / total)
        .collect();
    
    // Calculate mean
    let mean: f64 = scale_values.iter()
        .zip(freqs_relative.iter())
        .map(|(v, f)| v * f)
        .sum();
    
    // Calculate weighted sum of squared deviations
    let numerator: f64 = scale_values.iter()
        .zip(freqs_relative.iter())
        .map(|(v, f)| f * (v - mean).powi(2))
        .sum();
    
    // Maximum possible variance given scale limits
    let denominator = ((scale_max - scale_min) as f64).powi(2) / 4.0;
    
    numerator / denominator
}

/// Calculate horns index for a uniform distribution
fn calculate_horns_uniform(scale_min: i32, scale_max: i32) -> f64 {
    let n_values = (scale_max - scale_min + 1) as usize;
    let uniform_freqs = vec![1.0; n_values];
    calculate_horns(&uniform_freqs, scale_min, scale_max)
}

/// Calculate frequency data for a specific set of samples
fn calculate_frequency_rows<U>(
    samples: &[Vec<U>],
    scale_min: U,
    scale_max: U,
    label: &str,
) -> (Vec<String>, Vec<i32>, Vec<f64>, Vec<i64>, Vec<f64>)
where
    U: Integer + ToPrimitive + Copy,
{
    let scale_min_i32 = U::to_i32(&scale_min).unwrap();
    let scale_max_i32 = U::to_i32(&scale_max).unwrap();
    let n_values = (scale_max_i32 - scale_min_i32 + 1) as usize;
    
    let mut f_absolute = vec![0i64; n_values];
    let n_samples = samples.len() as f64;
    
    // Count frequencies
    for sample in samples {
        for &value in sample {
            let idx = (U::to_i32(&value).unwrap() - scale_min_i32) as usize;
            f_absolute[idx] += 1;
        }
    }
    
    // Calculate derived values
    let total_values: i64 = f_absolute.iter().sum();
    let value: Vec<i32> = (scale_min_i32..=scale_max_i32).collect();
    let f_average: Vec<f64> = f_absolute.iter()
        .map(|&f| f as f64 / n_samples)
        .collect();
    let f_relative: Vec<f64> = f_absolute.iter()
        .map(|&f| f as f64 / total_values as f64)
        .collect();
    
    // Create label column
    let samples_col = vec![label.to_string(); n_values];
    
    (samples_col, value, f_average, f_absolute, f_relative)
}

/// Calculate median of a sorted vector
fn median(sorted: &[f64]) -> f64 {
    let len = sorted.len();
    if len % 2 == 0 {
        (sorted[len / 2 - 1] + sorted[len / 2]) / 2.0
    } else {
        sorted[len / 2]
    }
}

/// Calculate median absolute deviation
fn mad(values: &[f64], median_val: f64) -> f64 {
    let mut deviations: Vec<f64> = values.iter()
        .map(|&v| (v - median_val).abs())
        .collect();
    deviations.sort_by(|a, b| a.partial_cmp(b).unwrap());
    median(&deviations)
}

/// Calculate all statistics for the samples
fn calculate_all_statistics<U>(
    samples: Vec<Vec<U>>,
    scale_min: U,
    scale_max: U,
) -> ClosureResults<U>
where
    U: Integer + ToPrimitive + Copy,
{
    let scale_min_i32 = U::to_i32(&scale_min).unwrap();
    let scale_max_i32 = U::to_i32(&scale_max).unwrap();
    let n = samples[0].len();
    let samples_all = samples.len();
    let values_all = samples_all * n;
    
    // Calculate horns for each sample
    let mut horns_values = Vec::with_capacity(samples_all);
    for sample in &samples {
        let mut freqs = vec![0.0; (scale_max_i32 - scale_min_i32 + 1) as usize];
        for &value in sample {
            let idx = (U::to_i32(&value).unwrap() - scale_min_i32) as usize;
            freqs[idx] += 1.0;
        }
        horns_values.push(calculate_horns(&freqs, scale_min_i32, scale_max_i32));
    }
    
    // Calculate horns statistics
    let horns_mean = horns_values.iter().sum::<f64>() / samples_all as f64;
    let horns_sd = {
        let variance = horns_values.iter()
            .map(|&h| (h - horns_mean).powi(2))
            .sum::<f64>() / samples_all as f64;
        variance.sqrt()
    };
    
    let mut horns_sorted = horns_values.clone();
    horns_sorted.sort_by(|a, b| a.partial_cmp(b).unwrap());
    let horns_min = horns_sorted[0];
    let horns_max = horns_sorted[samples_all - 1];
    let horns_median = median(&horns_sorted);
    let horns_mad = mad(&horns_values, horns_median);
    
    // Find indices of samples with min/max horns
    let min_indices: Vec<usize> = horns_values.iter()
        .enumerate()
        .filter(|(_, &h)| (h - horns_min).abs() < 1e-10)
        .map(|(i, _)| i)
        .collect();
    
    let max_indices: Vec<usize> = horns_values.iter()
        .enumerate()
        .filter(|(_, &h)| (h - horns_max).abs() < 1e-10)
        .map(|(i, _)| i)
        .collect();
    
    let min_samples: Vec<Vec<U>> = min_indices.iter()
        .map(|&i| samples[i].clone())
        .collect();
    
    let max_samples: Vec<Vec<U>> = max_indices.iter()
        .map(|&i| samples[i].clone())
        .collect();
    
    // Calculate frequency data for all three groups
    let (all_samples_col, all_value, all_f_average, all_f_absolute, all_f_relative) = 
        calculate_frequency_rows(&samples, scale_min, scale_max, "all");
    
    let (min_samples_col, min_value, min_f_average, min_f_absolute, min_f_relative) = 
        calculate_frequency_rows(&min_samples, scale_min, scale_max, "horns_min");
    
    let (max_samples_col, max_value, max_f_average, max_f_absolute, max_f_relative) = 
        calculate_frequency_rows(&max_samples, scale_min, scale_max, "horns_max");
    
    // Combine all frequency data into a single table
    let mut combined_samples = all_samples_col;
    combined_samples.extend(min_samples_col);
    combined_samples.extend(max_samples_col);
    
    let mut combined_value = all_value;
    combined_value.extend(min_value);
    combined_value.extend(max_value);
    
    let mut combined_f_average = all_f_average;
    combined_f_average.extend(min_f_average);
    combined_f_average.extend(max_f_average);
    
    let mut combined_f_absolute = all_f_absolute;
    combined_f_absolute.extend(min_f_absolute);
    combined_f_absolute.extend(max_f_absolute);
    
    let mut combined_f_relative = all_f_relative;
    combined_f_relative.extend(min_f_relative);
    combined_f_relative.extend(max_f_relative);
    
    // Create ID column for results table
    let id: Vec<usize> = (1..=samples_all).collect();
    
    ClosureResults {
        metrics_main: MetricsMain {
            samples_initial: count_initial_combinations(scale_min_i32, scale_max_i32),
            samples_all,
            values_all,
        },
        metrics_horns: MetricsHorns {
            mean: horns_mean,
            uniform: calculate_horns_uniform(scale_min_i32, scale_max_i32),
            sd: horns_sd,
            cv: horns_sd / horns_mean,
            mad: horns_mad,
            min: horns_min,
            median: horns_median,
            max: horns_max,
            range: horns_max - horns_min,
        },
        frequency: FrequencyTable {
            samples: combined_samples,
            value: combined_value,
            f_average: combined_f_average,
            f_absolute: combined_f_absolute,
            f_relative: combined_f_relative,
        },
        results: ResultsTable {
            id,
            samples,
            horns_values,
        },
    }
}

/// Create a Parquet writer for the results table with appropriate schema
/// Now stores samples as a list column instead of expanding them
fn create_results_writer(file_path: &str) -> Result<ArrowWriter<File>, Box<dyn std::error::Error>>
{
    // Create schema with id column, samples as list column, plus horns column
    // Note: List items are marked as nullable to match what Arrow's ListBuilder produces
    let fields = vec![
        Field::new("id", DataType::Int32, false),
        Field::new("samples", DataType::List(Arc::new(Field::new("item", DataType::Int32, true))), false), // true for nullable items
        Field::new("horns", DataType::Float64, false),
    ];
    
    let schema = Arc::new(Schema::new(fields));
    
    let file = File::create(file_path)?;
    let props = WriterProperties::builder().build();
    let writer = ArrowWriter::try_new(file, schema, Some(props))?;
    
    Ok(writer)
}

/// Create writers for statistics tables
fn create_stats_writers(base_path: &str) -> Result<(
    ArrowWriter<File>, 
    ArrowWriter<File>, 
    ArrowWriter<File>,
    Arc<Schema>,
    Arc<Schema>,
    Arc<Schema>
), Box<dyn std::error::Error>> {
    // Metrics main writer
    let metrics_main_schema = Arc::new(Schema::new(vec![
        Field::new("samples_initial", DataType::Int32, false),
        Field::new("samples_all", DataType::Int32, false),
        Field::new("values_all", DataType::Int32, false),
    ]));
    let metrics_main_file = File::create(format!("{}metrics_main.parquet", base_path))?;
    let metrics_main_writer = ArrowWriter::try_new(metrics_main_file, metrics_main_schema.clone(), None)?;
    
    // Metrics horns writer
    let metrics_horns_schema = Arc::new(Schema::new(vec![
        Field::new("mean", DataType::Float64, false),
        Field::new("uniform", DataType::Float64, false),
        Field::new("sd", DataType::Float64, false),
        Field::new("cv", DataType::Float64, false),
        Field::new("mad", DataType::Float64, false),
        Field::new("min", DataType::Float64, false),
        Field::new("median", DataType::Float64, false),
        Field::new("max", DataType::Float64, false),
        Field::new("range", DataType::Float64, false),
    ]));
    let metrics_horns_file = File::create(format!("{}metrics_horns.parquet", base_path))?;
    let metrics_horns_writer = ArrowWriter::try_new(metrics_horns_file, metrics_horns_schema.clone(), None)?;
    
    // Frequency writer with samples column first
    let frequency_schema = Arc::new(Schema::new(vec![
        Field::new("samples", DataType::Utf8, false),
        Field::new("value", DataType::Int32, false),
        Field::new("f_average", DataType::Float64, false),
        Field::new("f_absolute", DataType::Int64, false),
        Field::new("f_relative", DataType::Float64, false),
    ]));
    let frequency_file = File::create(format!("{}frequency.parquet", base_path))?;
    let frequency_writer = ArrowWriter::try_new(frequency_file, frequency_schema.clone(), None)?;
    
    Ok((
        metrics_main_writer, 
        metrics_horns_writer, 
        frequency_writer,
        metrics_main_schema,
        metrics_horns_schema,
        frequency_schema
    ))
}

/// Convert results table to a RecordBatch for Parquet writing
/// Now properly handles samples as a list column
fn results_to_record_batch<U>(
    results: &ResultsTable<U>,
    start_idx: usize,
    end_idx: usize,
) -> Result<RecordBatch, Box<dyn std::error::Error>>
where
    U: Integer + ToPrimitive + Copy,
{
    // Create arrays for each column
    let mut arrays: Vec<ArrayRef> = Vec::new();
    
    // Add ID column
    let id_data: Vec<i32> = results.id[start_idx..end_idx]
        .iter()
        .map(|&id| id as i32)
        .collect();
    arrays.push(Arc::new(Int32Array::from(id_data)));
    
    // Add samples column as a list using the standard ListBuilder
    let mut list_builder = ListBuilder::new(Int32Builder::new());
    
    for sample in &results.samples[start_idx..end_idx] {
        // Append all values for this sample
        for &val in sample {
            list_builder.values().append_value(U::to_i32(&val).unwrap());
        }
        // Mark the end of this list
        list_builder.append(true);
    }
    
    arrays.push(Arc::new(list_builder.finish()));
    
    // Add horns column
    let horns_data: Vec<f64> = results.horns_values[start_idx..end_idx].to_vec();
    arrays.push(Arc::new(Float64Array::from(horns_data)));
    
    // Create schema - matching the schema from create_results_writer
    let fields = vec![
        Field::new("id", DataType::Int32, false),
        Field::new("samples", DataType::List(Arc::new(Field::new("item", DataType::Int32, true))), false), // true for nullable items
        Field::new("horns", DataType::Float64, false),
    ];
    
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

/// Generate all valid combinations (memory mode) with summary statistics
/// 
/// This function computes all valid combinations and returns them in memory
/// along with comprehensive statistics.
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
) -> ClosureResults<U>
where
    T: Float + FromPrimitive + Send + Sync,
    U: Integer + NumCast + ToPrimitive + Copy + Send + Sync + 'static,
{
    let (target_sum_upper, target_sum_lower, sd_upper, sd_lower, n_usize, 
         min_scale_sum_t, scale_max_sum_t, n_minus_1, scale_max_plus_1) = 
        prepare_computation(mean, sd, n, scale_min, scale_max, rounding_error_mean, rounding_error_sd);

    // Generate initial combinations
    let combinations = generate_initial_combinations(scale_min, scale_max_plus_1);

    // Process combinations in parallel
    let results: Vec<Vec<U>> = combinations.par_iter()
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
                &min_scale_sum_t,
                &scale_max_sum_t,
                n_minus_1,
                scale_max_plus_1,
                scale_min,
            )
        })
        .collect();

    // Calculate all statistics
    let closure_results = calculate_all_statistics(results, scale_min, scale_max);
    
    // Write to Parquet if configured
    if let Some(config) = parquet_config {
        // Ensure base_path ends with / for consistent file naming
        let base_path = if config.file_path.ends_with('/') {
            config.file_path.clone()
        } else {
            format!("{}/", config.file_path)
        };
        
        // Write results table
        let results_path = format!("{}results.parquet", base_path);
        if let Ok(mut writer) = create_results_writer(&results_path) {
            let batch_size = config.batch_size;
            let total_samples = closure_results.results.samples.len();
            
            for start in (0..total_samples).step_by(batch_size) {
                let end = (start + batch_size).min(total_samples);
                
                if let Ok(record_batch) = results_to_record_batch(
                    &closure_results.results,
                    start,
                    end,
                ) {
                    let _ = writer.write(&record_batch);
                }
            }
            let _ = writer.close();
        }
        
        // Write statistics tables
        if let Ok((mut mm_writer, mut mh_writer, mut freq_writer, mm_schema, mh_schema, freq_schema)) = create_stats_writers(&base_path) {
            // Write metrics_main
            let mm_batch = RecordBatch::try_new(
                mm_schema,
                vec![
                    Arc::new(Int32Array::from(vec![closure_results.metrics_main.samples_initial])),
                    Arc::new(Int32Array::from(vec![closure_results.metrics_main.samples_all as i32])),
                    Arc::new(Int32Array::from(vec![closure_results.metrics_main.values_all as i32])),
                ],
            );
            if let Ok(batch) = mm_batch {
                let _ = mm_writer.write(&batch);
            }
            let _ = mm_writer.close();
            
            // Write metrics_horns
            let mh_batch = RecordBatch::try_new(
                mh_schema,
                vec![
                    Arc::new(Float64Array::from(vec![closure_results.metrics_horns.mean])),
                    Arc::new(Float64Array::from(vec![closure_results.metrics_horns.uniform])),
                    Arc::new(Float64Array::from(vec![closure_results.metrics_horns.sd])),
                    Arc::new(Float64Array::from(vec![closure_results.metrics_horns.cv])),
                    Arc::new(Float64Array::from(vec![closure_results.metrics_horns.mad])),
                    Arc::new(Float64Array::from(vec![closure_results.metrics_horns.min])),
                    Arc::new(Float64Array::from(vec![closure_results.metrics_horns.median])),
                    Arc::new(Float64Array::from(vec![closure_results.metrics_horns.max])),
                    Arc::new(Float64Array::from(vec![closure_results.metrics_horns.range])),
                ],
            );
            if let Ok(batch) = mh_batch {
                let _ = mh_writer.write(&batch);
            }
            let _ = mh_writer.close();
            
            // Write frequency table
            let freq_batch = RecordBatch::try_new(
                freq_schema,
                vec![
                    Arc::new(StringArray::from(closure_results.frequency.samples.clone())),
                    Arc::new(Int32Array::from(closure_results.frequency.value.clone())),
                    Arc::new(Float64Array::from(closure_results.frequency.f_average.clone())),
                    Arc::new(Int64Array::from(closure_results.frequency.f_absolute.clone())),
                    Arc::new(Float64Array::from(closure_results.frequency.f_relative.clone())),
                ],
            );
            if let Ok(batch) = freq_batch {
                let _ = freq_writer.write(&batch);
            }
            let _ = freq_writer.close();
        }
    }
    
    closure_results
}

/// Structure to hold streaming frequency state
struct StreamingFrequencyState {
    current_min_horns: f64,
    current_max_horns: f64,
    all_freq: HashMap<i32, i64>,
    min_freq: HashMap<i32, i64>,
    max_freq: HashMap<i32, i64>,
    min_count: usize,
    max_count: usize,
}

/// Generate all valid combinations (streaming mode) with summary statistics
/// 
/// This function computes all valid combinations and streams them directly to Parquet files
/// without keeping them in memory. Statistics are computed incrementally.
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

    // Setup channels for streaming results
    let (tx_samples, rx_samples) = channel::<Vec<(Vec<U>, f64, usize)>>();
    let (tx_stats, rx_stats) = channel::<(Vec<f64>, HashMap<i32, i64>)>();
    
    // Add a flag to track writer thread status
    let writer_failed = Arc::new(AtomicUsize::new(0)); // 0 = ok, 1 = failed
    let writer_failed_for_compute = writer_failed.clone();
    let writer_failed_for_thread = writer_failed.clone();
    
    // Counter for total combinations found
    let total_counter = Arc::new(AtomicUsize::new(0));
    let counter_for_thread = total_counter.clone();
    
    // Shared state for tracking min/max horns frequencies
    let freq_state = Arc::new(Mutex::new(StreamingFrequencyState {
        current_min_horns: f64::INFINITY,
        current_max_horns: f64::NEG_INFINITY,
        all_freq: HashMap::new(),
        min_freq: HashMap::new(),
        max_freq: HashMap::new(),
        min_count: 0,
        max_count: 0,
    }));
    let freq_state_for_thread = freq_state.clone();
    
    // Handle file paths more carefully
    // If the path ends with .parquet, treat it as a file prefix
    // Otherwise, treat it as a directory
    let (base_path, ensure_dir) = if config.file_path.ends_with(".parquet") {
        // User provided a file name like "output.parquet"
        // Strip the .parquet and use as prefix
        let base = config.file_path.trim_end_matches(".parquet");
        (format!("{}_", base), false)
    } else if config.file_path.ends_with('/') {
        // User provided a directory with trailing slash
        (config.file_path.clone(), true)
    } else {
        // User provided either a directory without trailing slash or a file prefix
        // Check if it looks like a directory (exists and is a directory)
        if std::path::Path::new(&config.file_path).is_dir() {
            (format!("{}/", config.file_path), false)
        } else {
            // Treat as a file prefix
            (format!("{}_", config.file_path), false)
        }
    };
    
    // Create parent directory if needed
    if let Some(parent) = std::path::Path::new(&base_path).parent() {
        if !parent.to_str().unwrap_or("").is_empty() {
            if let Err(e) = std::fs::create_dir_all(parent) {
                eprintln!("Warning: Could not create directory {:?}: {}", parent, e);
            }
        }
    }
    
    // If we determined this should be a directory, create it
    if ensure_dir {
        if let Err(e) = std::fs::create_dir_all(&base_path.trim_end_matches('/')) {
            eprintln!("Warning: Could not create directory {}: {}", base_path, e);
        }
    }
    
    // Spawn dedicated writer thread
    let file_path_clone = format!("{}results.parquet", base_path);
    
    let writer_handle = thread::spawn(move || {
        // Try to create the writer, with better error handling
        let mut writer = match create_results_writer(&file_path_clone) {
            Ok(w) => w,
            Err(e) => {
                eprintln!("ERROR: Failed to create Parquet writer for file '{}': {}", file_path_clone, e);
                eprintln!("Please check that:");
                eprintln!("  1. The parent directory exists or can be created");
                eprintln!("  2. You have write permissions for this location");
                eprintln!("  3. There is sufficient disk space");
                
                // Mark writer as failed
                writer_failed_for_thread.store(1, Ordering::Relaxed);
                
                // Return 0 to indicate failure
                return 0;
            }
        };
        
        let mut buffer_results = ResultsTable {
            id: Vec::with_capacity(config.batch_size * 2),
            samples: Vec::with_capacity(config.batch_size * 2),
            horns_values: Vec::with_capacity(config.batch_size * 2),
        };
        
        let mut total_written = 0;
        let mut last_progress_report = 0;
        let mut current_id = 1usize;
        let mut channel_closed = false;
        
        // Process incoming results
        loop {
            match rx_samples.recv() {
                Ok(batch) => {
                    for (sample, horns, _) in batch {
                        buffer_results.id.push(current_id);
                        buffer_results.samples.push(sample);
                        buffer_results.horns_values.push(horns);
                        current_id += 1;
                    }
                    
                    // Write when buffer reaches threshold
                    if buffer_results.samples.len() >= config.batch_size {
                        let batch_len = buffer_results.samples.len();
                        match results_to_record_batch(
                            &buffer_results,
                            0,
                            batch_len,
                        ) {
                            Ok(record_batch) => {
                                if let Err(e) = writer.write(&record_batch) {
                                    eprintln!("ERROR: Failed to write batch to Parquet file: {}", e);
                                    return total_written;
                                }
                                total_written += batch_len;
                                
                                // Progress reporting
                                if config.show_progress && total_written - last_progress_report >= 100_000 {
                                    eprintln!("Progress: {} combinations written...", total_written);
                                    last_progress_report = total_written;
                                }
                            },
                            Err(e) => {
                                eprintln!("ERROR: Failed to create record batch: {}", e);
                                return total_written;
                            }
                        }
                        buffer_results.id.clear();
                        buffer_results.samples.clear();
                        buffer_results.horns_values.clear();
                    }
                },
                Err(_) => {
                    // Channel has been closed, this is normal when processing is complete
                    channel_closed = true;
                    break;
                }
            }
        }
        
        // Write any remaining results
        if !buffer_results.samples.is_empty() {
            let batch_len = buffer_results.samples.len();
            match results_to_record_batch(
                &buffer_results,
                0,
                batch_len,
            ) {
                Ok(record_batch) => {
                    if let Err(e) = writer.write(&record_batch) {
                        eprintln!("ERROR: Failed to write final batch to Parquet file: {}", e);
                    } else {
                        total_written += batch_len;
                    }
                },
                Err(e) => {
                    eprintln!("ERROR: Failed to create final record batch: {}", e);
                }
            }
        }
        
        if let Err(e) = writer.close() {
            eprintln!("ERROR: Failed to properly close Parquet file: {}", e);
        }
        
        if config.show_progress {
            if channel_closed && total_written > 0 {
                eprintln!("Streaming complete: {} total combinations written", total_written);
            } else if total_written == 0 {
                eprintln!("WARNING: No combinations were written. This might indicate:");
                eprintln!("  - The search parameters are too restrictive");
                eprintln!("  - There was an error in file creation");
                eprintln!("  - The computation found no valid combinations");
            }
        }
        
        total_written
    });
    
    // Spawn statistics collector thread
    let freq_state_for_stats = freq_state.clone();
    let stats_handle = thread::spawn(move || {
        let mut all_horns = Vec::new();
        
        while let Ok((horns_batch, _)) = rx_stats.recv() {
            all_horns.extend(horns_batch);
        }
        
        (all_horns, freq_state_for_stats)
    });

    // Generate initial combinations
    let combinations = generate_initial_combinations(scale_min, scale_max_plus_1);
    
    let scale_min_i32 = U::to_i32(&scale_min).unwrap();
    let scale_max_i32 = U::to_i32(&scale_max).unwrap();

    // Process combinations in parallel
    combinations.par_iter()
        .for_each(|(combo, running_sum, running_m2)| {
            // Check if writer has failed before doing expensive computation
            if writer_failed_for_compute.load(Ordering::Relaxed) == 1 {
                return;
            }
            
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
            
            if !branch_results.is_empty() {
                // Check again before processing results
                if writer_failed_for_compute.load(Ordering::Relaxed) == 1 {
                    return;
                }
                
                let mut samples_with_horns = Vec::with_capacity(branch_results.len());
                let mut horns_batch = Vec::with_capacity(branch_results.len());
                
                // Calculate horns for each sample and update frequency state
                for (idx, sample) in branch_results.into_iter().enumerate() {
                    let mut freqs = vec![0.0; (scale_max_i32 - scale_min_i32 + 1) as usize];
                    let mut sample_freq: HashMap<i32, i64> = HashMap::new();
                    
                    for &value in &sample {
                        let idx = (U::to_i32(&value).unwrap() - scale_min_i32) as usize;
                        freqs[idx] += 1.0;
                        *sample_freq.entry(U::to_i32(&value).unwrap()).or_insert(0) += 1;
                    }
                    
                    let horns = calculate_horns(&freqs, scale_min_i32, scale_max_i32);
                    
                    // Update frequency state with proper locking
                    {
                        let mut state = freq_state_for_thread.lock().unwrap();
                        
                        // Update all frequencies
                        for (&val, &count) in &sample_freq {
                            *state.all_freq.entry(val).or_insert(0) += count;
                        }
                        
                        // Check if this is a new min or max
                        if (horns - state.current_min_horns).abs() < 1e-10 {
                            // Equal to current min
                            for (&val, &count) in &sample_freq {
                                *state.min_freq.entry(val).or_insert(0) += count;
                            }
                            state.min_count += 1;
                        } else if horns < state.current_min_horns {
                            // New minimum found
                            state.current_min_horns = horns;
                            state.min_freq.clear();
                            for (&val, &count) in &sample_freq {
                                state.min_freq.insert(val, count);
                            }
                            state.min_count = 1;
                        }
                        
                        if (horns - state.current_max_horns).abs() < 1e-10 {
                            // Equal to current max
                            for (&val, &count) in &sample_freq {
                                *state.max_freq.entry(val).or_insert(0) += count;
                            }
                            state.max_count += 1;
                        } else if horns > state.current_max_horns {
                            // New maximum found
                            state.current_max_horns = horns;
                            state.max_freq.clear();
                            for (&val, &count) in &sample_freq {
                                state.max_freq.insert(val, count);
                            }
                            state.max_count = 1;
                        }
                    }
                    
                    horns_batch.push(horns);
                    samples_with_horns.push((sample, horns, idx));
                }
                
                // Update counter
                counter_for_thread.fetch_add(samples_with_horns.len(), Ordering::Relaxed);
                
                // Send to writer and stats collector
                // Check if the send fails (channel closed due to writer failure)
                if tx_samples.send(samples_with_horns).is_err() {
                    // Channel is closed, writer must have failed
                    if config.show_progress {
                        eprintln!("WARNING: Cannot send results to writer - channel closed");
                    }
                    return;
                }
                
                // Same for stats channel
                if tx_stats.send((horns_batch, HashMap::new())).is_err() {
                    return;
                }
            }
        });
    
    // Close channels
    drop(tx_samples);
    drop(tx_stats);
    
    // Wait for threads to complete
    let total_written = writer_handle.join()
        .unwrap_or_else(|_| {
            eprintln!("ERROR: Writer thread panicked unexpectedly");
            0
        });
    
    let (all_horns, final_freq_state) = stats_handle.join()
        .unwrap_or_else(|_| {
            eprintln!("ERROR: Statistics thread panicked unexpectedly");
            (Vec::new(), freq_state)
        });
    
    // Check if we successfully wrote any results
    if total_written == 0 {
        eprintln!("\nERROR: No results were written to disk.");
        eprintln!("This could be due to:");
        eprintln!("  1. File system errors (check the error messages above)");
        eprintln!("  2. No valid combinations found with the given parameters");
        eprintln!("  3. Parameters that are too restrictive");
        
        return StreamingResult {
            total_combinations: 0,
            file_path: config.file_path,
        };
    }
    
    // Now write the statistics files
    if let Ok((mut mm_writer, mut mh_writer, mut freq_writer, mm_schema, mh_schema, freq_schema)) = create_stats_writers(&base_path) {
        // Calculate final statistics
        let samples_all = all_horns.len();
        let values_all = samples_all * n_usize;
        
        // Calculate horns statistics
        let horns_mean = all_horns.iter().sum::<f64>() / samples_all as f64;
        let horns_sd = {
            let variance = all_horns.iter()
                .map(|&h| (h - horns_mean).powi(2))
                .sum::<f64>() / samples_all as f64;
            variance.sqrt()
        };
        
        let mut horns_sorted = all_horns.clone();
        horns_sorted.sort_by(|a, b| a.partial_cmp(b).unwrap());
        let horns_min = horns_sorted[0];
        let horns_max = horns_sorted[samples_all - 1];
        let horns_median = median(&horns_sorted);
        let horns_mad = mad(&all_horns, horns_median);
        
        // Write metrics_main
        let mm_batch = RecordBatch::try_new(
            mm_schema,
            vec![
                Arc::new(Int32Array::from(vec![count_initial_combinations(scale_min_i32, scale_max_i32)])),
                Arc::new(Int32Array::from(vec![samples_all as i32])),
                Arc::new(Int32Array::from(vec![values_all as i32])),
            ],
        );
        if let Ok(batch) = mm_batch {
            let _ = mm_writer.write(&batch);
        }
        let _ = mm_writer.close();
        
        // Write metrics_horns
        let mh_batch = RecordBatch::try_new(
            mh_schema,
            vec![
                Arc::new(Float64Array::from(vec![horns_mean])),
                Arc::new(Float64Array::from(vec![calculate_horns_uniform(scale_min_i32, scale_max_i32)])),
                Arc::new(Float64Array::from(vec![horns_sd])),
                Arc::new(Float64Array::from(vec![horns_sd / horns_mean])),
                Arc::new(Float64Array::from(vec![horns_mad])),
                Arc::new(Float64Array::from(vec![horns_min])),
                Arc::new(Float64Array::from(vec![horns_median])),
                Arc::new(Float64Array::from(vec![horns_max])),
                Arc::new(Float64Array::from(vec![horns_max - horns_min])),
            ],
        );
        if let Ok(batch) = mh_batch {
            let _ = mh_writer.write(&batch);
        }
        let _ = mh_writer.close();
        
        // Extract frequency data from final state
        let state = final_freq_state.lock().unwrap();
        let total_values = values_all as f64;
        let n_samples = samples_all as f64;
        
        // Write frequency table with all three categories
        // First write "all" frequencies
        for scale_value in scale_min_i32..=scale_max_i32 {
            let count = state.all_freq.get(&scale_value).unwrap_or(&0);
            
            let freq_batch = RecordBatch::try_new(
                freq_schema.clone(),
                vec![
                    Arc::new(StringArray::from(vec!["all"])),
                    Arc::new(Int32Array::from(vec![scale_value])),
                    Arc::new(Float64Array::from(vec![*count as f64 / n_samples])),
                    Arc::new(Int64Array::from(vec![*count])),
                    Arc::new(Float64Array::from(vec![*count as f64 / total_values])),
                ],
            );
            if let Ok(batch) = freq_batch {
                let _ = freq_writer.write(&batch);
            }
        }
        
        // Write "horns_min" frequencies
        let min_n_samples = state.min_count as f64;
        let min_total_values = (state.min_count * n_usize) as f64;
        for scale_value in scale_min_i32..=scale_max_i32 {
            let count = state.min_freq.get(&scale_value).unwrap_or(&0);
            
            let freq_batch = RecordBatch::try_new(
                freq_schema.clone(),
                vec![
                    Arc::new(StringArray::from(vec!["horns_min"])),
                    Arc::new(Int32Array::from(vec![scale_value])),
                    Arc::new(Float64Array::from(vec![*count as f64 / min_n_samples])),
                    Arc::new(Int64Array::from(vec![*count])),
                    Arc::new(Float64Array::from(vec![*count as f64 / min_total_values])),
                ],
            );
            if let Ok(batch) = freq_batch {
                let _ = freq_writer.write(&batch);
            }
        }
        
        // Write "horns_max" frequencies
        let max_n_samples = state.max_count as f64;
        let max_total_values = (state.max_count * n_usize) as f64;
        for scale_value in scale_min_i32..=scale_max_i32 {
            let count = state.max_freq.get(&scale_value).unwrap_or(&0);
            
            let freq_batch = RecordBatch::try_new(
                freq_schema.clone(),
                vec![
                    Arc::new(StringArray::from(vec!["horns_max"])),
                    Arc::new(Int32Array::from(vec![scale_value])),
                    Arc::new(Float64Array::from(vec![*count as f64 / max_n_samples])),
                    Arc::new(Int64Array::from(vec![*count])),
                    Arc::new(Float64Array::from(vec![*count as f64 / max_total_values])),
                ],
            );
            if let Ok(batch) = freq_batch {
                let _ = freq_writer.write(&batch);
            }
        }
        
        let _ = freq_writer.close();
    }
    
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
    fn test_horns_calculation() {
        // Test uniform distribution
        let uniform_freqs = vec![1.0, 1.0, 1.0, 1.0, 1.0];
        let horns_uniform = calculate_horns(&uniform_freqs, 1, 5);
        assert!((horns_uniform - 0.4).abs() < 0.01);
        
        // Test extreme distribution
        let extreme_freqs = vec![1.0, 0.0, 0.0, 0.0, 1.0];
        let horns_extreme = calculate_horns(&extreme_freqs, 1, 5);
        assert!((horns_extreme - 1.0).abs() < 0.01);
    }
    
    #[test]
    fn test_dfs_parallel_with_new_api() {
        // Test that the function returns valid statistics with new structure
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
        
        // Check that results table is properly formed
        assert!(!results.results.samples.is_empty());
        assert_eq!(results.results.samples.len(), results.results.horns_values.len());
        assert_eq!(results.results.samples.len(), results.results.id.len());
        assert_eq!(results.results.id[0], 1);
        assert_eq!(results.results.id.last(), Some(&results.results.samples.len()));
        
        // Check metrics
        assert_eq!(results.metrics_main.samples_all, results.results.samples.len());
        assert_eq!(results.metrics_main.values_all, results.results.samples.len() * 5);
        
        // Check horns metrics
        assert!(results.metrics_horns.min <= results.metrics_horns.mean);
        assert!(results.metrics_horns.mean <= results.metrics_horns.max);
        assert!(results.metrics_horns.range >= 0.0);
        
        // Check combined frequency table
        let n_values = 5; // scale_max - scale_min + 1
        let expected_rows = n_values * 3; // all, horns_min, horns_max
        assert_eq!(results.frequency.samples.len(), expected_rows);
        assert_eq!(results.frequency.value.len(), expected_rows);
        assert_eq!(results.frequency.f_average.len(), expected_rows);
        assert_eq!(results.frequency.f_absolute.len(), expected_rows);
        assert_eq!(results.frequency.f_relative.len(), expected_rows);
        
        // Check that samples column has correct values
        let all_count = results.frequency.samples.iter().filter(|&s| s == "all").count();
        let min_count = results.frequency.samples.iter().filter(|&s| s == "horns_min").count();
        let max_count = results.frequency.samples.iter().filter(|&s| s == "horns_max").count();
        assert_eq!(all_count, n_values);
        assert_eq!(min_count, n_values);
        assert_eq!(max_count, n_values);
    }
    
    #[test]
    fn test_dfs_parallel_with_file() {
        // Test with Parquet output
        let config = ParquetConfig {
            file_path: "test_output/".to_string(),
            batch_size: 100,
        };
        
        let _ = std::fs::create_dir("test_output");
        
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
        
        assert!(!results.results.samples.is_empty());
        
        // Clean up test files
        let _ = std::fs::remove_file("test_output/results.parquet");
        let _ = std::fs::remove_file("test_output/metrics_main.parquet");
        let _ = std::fs::remove_file("test_output/metrics_horns.parquet");
        let _ = std::fs::remove_file("test_output/frequency.parquet");
        let _ = std::fs::remove_dir("test_output");
    }
    
    #[test]
    fn test_dfs_parallel_streaming() {
        // Test streaming mode
        let config = StreamingConfig {
            file_path: "test_streaming/".to_string(),
            batch_size: 100,
            show_progress: false,
        };
        
        let _ = std::fs::create_dir("test_streaming");
        
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
        assert_eq!(result.file_path, "test_streaming/");
        
        // Clean up test files
        let _ = std::fs::remove_file("test_streaming/results.parquet");
        let _ = std::fs::remove_file("test_streaming/metrics_main.parquet");
        let _ = std::fs::remove_file("test_streaming/metrics_horns.parquet");
        let _ = std::fs::remove_file("test_streaming/frequency.parquet");
        let _ = std::fs::remove_dir("test_streaming");
    }
}