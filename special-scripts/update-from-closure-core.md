## Breaking changes for unsum (after closure-core commit 0d4154449ddc9c48c50fed623214d2544dd5884e on branch frequency-details)

- ResultsTable.sample: Vec<Vec<U>> → counts: SampleCounts + .sample(i) / .samples() reconstruction
- Output dir is now counts.parquet + scale_values.parquet + format.parquet (version 2); OutputFormat::Samples/Both on the config still writes the old layout for Python cross-validation
- value columns in frequency/frequency_dist/modality_* are Float64 now (fractional grids need it)
