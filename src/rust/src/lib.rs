/// This is part of unsum, an R package that uses extendr for Rust integration

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
/// The frequency table now includes a 'samples' column as the first column
fn frequency_table_to_robj(freq_table: &closure_core::FrequencyTable) -> Robj {
    // Create a data frame with columns: samples, value, f_average, f_absolute, f_relative
    let df = data_frame!(
        samples = freq_table.samples.clone(),
        value = freq_table.value.clone(),
        f_average = freq_table.f_average.clone(),
        f_absolute = freq_table.f_absolute.clone(),
        f_relative = freq_table.f_relative.clone()
    );

    df.into()
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
    let samples_robjs: Vec<Robj> = results_table.samples
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
    rounding_error_mean: f64,
    rounding_error_sd: f64,
    write: Robj, // Accept Robj directly
) -> Robj {
    let need_to_write = !write.is_null();

    if need_to_write {
        // Parse the write parameter as StreamingConfig for streaming mode
        let mut streaming_config = match StreamingConfigR::try_from(write.clone()) {
            Ok(config_wrapper) => config_wrapper.0,
            Err(e) => {
                return Robj::from(format!("Error parsing write configuration: {}", e));
            }
        };

        // Check if show_progress was provided, default to true for better UX
        streaming_config.show_progress = write.dollar("show_progress")
            .ok()
            .and_then(|r| r.as_bool())
            .unwrap_or(true);  // Default to true instead of false

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
    let closure_results = dfs_parallel(
        mean,
        sd,
        n,
        scale_min,
        scale_max,
        rounding_error_mean,
        rounding_error_sd,
        None, // No parquet config - just return results in memory
    );

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


/// Write an R data frame to a Parquet file
#[extendr]
fn write_dataframe_to_parquet(df: Robj, file_path: &str) -> Robj {
    use arrow::datatypes::{Schema, Field, DataType};
    use arrow::array::{ArrayRef, Int32Array, Float64Array, StringArray};
    use arrow::record_batch::RecordBatch;
    use parquet::arrow::ArrowWriter;
    use parquet::file::properties::WriterProperties;
    use std::sync::Arc;
    use std::fs::File;

    // First, check if the input is actually a data frame
    if !df.inherits("data.frame") {
        return Robj::from("Error: Input must be a data frame");
    }

    // Get column names
    let col_names_robj = df.names().unwrap();
    let col_names: Vec<String> = col_names_robj
        .as_str_iter()
        .unwrap()
        .map(|s| s.to_string())
        .collect();

    // Build Arrow schema and arrays based on R data frame structure
    let mut fields = Vec::new();
    let mut arrays: Vec<ArrayRef> = Vec::new();

    for col_name in &col_names {
        let col_data = df.dollar(col_name).unwrap();

        // Detect column type and create appropriate Arrow array
        if col_data.is_integer() {
            // Integer column
            fields.push(Field::new(col_name, DataType::Int32, true));
            let values: Vec<Option<i32>> = col_data
                .as_integer_iter()
                .unwrap()
                .map(|v| v.map(|x| x as i32))
                .collect();
            arrays.push(Arc::new(Int32Array::from(values)));

        } else if col_data.is_real() {
            // Numeric/double column
            fields.push(Field::new(col_name, DataType::Float64, true));
            let values: Vec<Option<f64>> = col_data
                .as_real_iter()
                .unwrap()
                .collect();
            arrays.push(Arc::new(Float64Array::from(values)));

        } else if col_data.is_string() {
            // Character column
            fields.push(Field::new(col_name, DataType::Utf8, true));
            let values: Vec<Option<&str>> = col_data
                .as_str_iter()
                .unwrap()
                .collect();
            arrays.push(Arc::new(StringArray::from(values)));

        } else {
            return Robj::from(format!("Error: Unsupported column type for column '{}'", col_name));
        }
    }

    // Create schema
    let schema = Arc::new(Schema::new(fields));

    // Create RecordBatch
    let batch = match RecordBatch::try_new(schema.clone(), arrays) {
        Ok(b) => b,
        Err(e) => return Robj::from(format!("Error creating record batch: {}", e)),
    };

    // Write to Parquet file
    let file = match File::create(file_path) {
        Ok(f) => f,
        Err(e) => return Robj::from(format!("Error creating file: {}", e)),
    };

    let props = WriterProperties::builder().build();
    let mut writer = match ArrowWriter::try_new(file, schema, Some(props)) {
        Ok(w) => w,
        Err(e) => return Robj::from(format!("Error creating writer: {}", e)),
    };

    if let Err(e) = writer.write(&batch) {
        return Robj::from(format!("Error writing batch: {}", e));
    }

    if let Err(e) = writer.close() {
        return Robj::from(format!("Error closing writer: {}", e));
    }

    Robj::from(format!("Successfully wrote {} rows to {}", batch.num_rows(), file_path))
}


/// Read all Parquet files created by dfs_parallel_streaming plus inputs.parquet
#[extendr]
fn read_closure_parquet_files(base_path: &str) -> Robj {
    use arrow::array::{ArrayRef, Int32Array, Int64Array, Float64Array, StringArray, ListArray, AsArray};
    use parquet::arrow::{ParquetRecordBatchReaderBuilder, ArrowReader};
    use std::fs::File;
    use std::path::Path;

    // Helper function to read a single Parquet file into an R data frame
    fn read_parquet_to_robj(file_path: &str) -> Result<Robj, String> {
        let file = File::open(file_path)
            .map_err(|e| format!("Cannot open {}: {}", file_path, e))?;

        let builder = ParquetRecordBatchReaderBuilder::try_new(file)
            .map_err(|e| format!("Cannot create reader for {}: {}", file_path, e))?;

        let mut reader = builder.build()
            .map_err(|e| format!("Cannot build reader for {}: {}", file_path, e))?;

        // Read all batches and combine
        let mut all_columns: Vec<(String, Vec<Robj>)> = Vec::new();
        let schema = reader.schema();

        // Initialize columns based on schema
        for field in schema.fields() {
            all_columns.push((field.name().clone(), Vec::new()));
        }

        // Read all record batches
        for batch_result in reader {
            let batch = batch_result
                .map_err(|e| format!("Error reading batch: {}", e))?;

            // Process each column
            for (col_idx, field) in schema.fields().iter().enumerate() {
                let column = batch.column(col_idx);
                let column_data = convert_arrow_column_to_r(column, field)?;
                all_columns[col_idx].1.push(column_data);
            }
        }

        // Combine batches for each column
        let mut final_columns = Vec::new();
        for (col_name, col_batches) in all_columns {
            if col_batches.is_empty() {
                continue;
            }

            // Concatenate all batches for this column
            let combined = if col_batches.len() == 1 {
                col_batches.into_iter().next().unwrap()
            } else {
                // Need to concatenate multiple batches
                concatenate_r_vectors(col_batches)?
            };

            final_columns.push((col_name, combined));
        }

        // Create R data frame
        let mut df_list = List::new(final_columns.len());
        for (i, (col_name, col_data)) in final_columns.into_iter().enumerate() {
            df_list.set_elt(i, col_data)?;
            df_list.set_name(i, &col_name)?;
        }

        // Convert to data.frame
        let df_robj: Robj = df_list.into();

        // Set class to data.frame
        let class_vec = StrIter::from(["data.frame"]);
        df_robj.set_class(class_vec)?;

        // Set row names
        let nrows = df_robj.dollar(&df_robj.names().unwrap().as_str_iter().unwrap().next().unwrap())
            .unwrap()
            .len();
        let row_names: Vec<i32> = (1..=nrows as i32).collect();
        df_robj.set_attrib(symbol!("row.names"), row_names)?;

        Ok(df_robj)
    }

    // Helper to convert Arrow column to R vector
    fn convert_arrow_column_to_r(column: &ArrayRef, field: &arrow::datatypes::Field) -> Result<Robj, String> {
        use arrow::datatypes::DataType;

        match field.data_type() {
            DataType::Int32 => {
                let array = column.as_any().downcast_ref::<Int32Array>()
                    .ok_or("Failed to downcast to Int32Array")?;
                let values: Vec<Option<i32>> = (0..array.len())
                    .map(|i| if array.is_null(i) { None } else { Some(array.value(i)) })
                    .collect();
                Ok(values.into_robj())
            },
            DataType::Int64 => {
                let array = column.as_any().downcast_ref::<Int64Array>()
                    .ok_or("Failed to downcast to Int64Array")?;
                // Convert i64 to f64 for R compatibility
                let values: Vec<Option<f64>> = (0..array.len())
                    .map(|i| if array.is_null(i) { None } else { Some(array.value(i) as f64) })
                    .collect();
                Ok(values.into_robj())
            },
            DataType::Float64 => {
                let array = column.as_any().downcast_ref::<Float64Array>()
                    .ok_or("Failed to downcast to Float64Array")?;
                let values: Vec<Option<f64>> = (0..array.len())
                    .map(|i| if array.is_null(i) { None } else { Some(array.value(i)) })
                    .collect();
                Ok(values.into_robj())
            },
            DataType::Utf8 => {
                let array = column.as_any().downcast_ref::<StringArray>()
                    .ok_or("Failed to downcast to StringArray")?;
                let values: Vec<Option<&str>> = (0..array.len())
                    .map(|i| array.value(i))
                    .collect();
                Ok(values.into_robj())
            },
            DataType::List(inner_field) => {
                // Special handling for list columns (like samples in results.parquet)
                let list_array = column.as_list::<i32>();
                let mut r_list = List::new(list_array.len());

                for i in 0..list_array.len() {
                    if list_array.is_null(i) {
                        r_list.set_elt(i, Robj::from(NULL))?;
                    } else {
                        let inner_array = list_array.value(i);
                        let inner_robj = convert_arrow_column_to_r(&inner_array, inner_field)?;
                        r_list.set_elt(i, inner_robj)?;
                    }
                }

                Ok(r_list.into())
            },
            _ => Err(format!("Unsupported data type: {:?}", field.data_type())),
        }
    }

    // Helper to concatenate R vectors
    fn concatenate_r_vectors(vectors: Vec<Robj>) -> Result<Robj, String> {
        // For simplicity, using R's c() function
        // In a real implementation, you'd do this more efficiently
        if vectors.is_empty() {
            return Ok(Robj::from(NULL));
        }

        let mut result = vectors[0].clone();
        for vec in vectors.into_iter().skip(1) {
            // This is a simplified approach - in practice you'd handle different types
            result = R!("c({{result}}, {{vec}})")
                .map_err(|e| format!("Error concatenating vectors: {:?}", e))?;
        }
        Ok(result)
    }

    // Main function body
    let base_path = if base_path.ends_with('/') {
        base_path.to_string()
    } else {
        format!("{}/", base_path)
    };

    let mut result_list = List::new(5);
    let mut list_names = Vec::new();
    let mut idx = 0;

    // Read each file
    let files = vec![
        ("inputs", format!("{}inputs.parquet", base_path)),
        ("results", format!("{}results.parquet", base_path)),
        ("metrics_main", format!("{}metrics_main.parquet", base_path)),
        ("metrics_horns", format!("{}metrics_horns.parquet", base_path)),
        ("frequency", format!("{}frequency.parquet", base_path)),
    ];

    for (name, file_path) in files {
        if Path::new(&file_path).exists() {
            match read_parquet_to_robj(&file_path) {
                Ok(df) => {
                    result_list.set_elt(idx, df)?;
                    list_names.push(name);
                    idx += 1;
                },
                Err(e) => {
                    eprintln!("Warning: Failed to read {}: {}", file_path, e);
                }
            }
        } else {
            eprintln!("Warning: File {} does not exist", file_path);
        }
    }

    // Resize list to actual number of files read
    let mut final_list = List::new(idx);
    for i in 0..idx {
        final_list.set_elt(i, result_list.elt(i).unwrap())?;
        final_list.set_name(i, list_names[i])?;
    }

    Ok(final_list.into())
}


// Macro to generate exports.
// This ensures exported functions are registered with R.
// See corresponding C code in `entrypoint.c`.
extendr_module! {
    mod unsum;
    fn create_combinations;
}
