# Write SPRITE results to disk (and read them back in)

You can use `sprite_write()` to save the results of
[`sprite_generate()`](https://lhdjung.github.io/unsum/reference/sprite_generate.md)
on your computer. A message will show the exact location.

The data are saved in a new folder as separate Parquet files, one for
each tibble in
[`sprite_generate()`](https://lhdjung.github.io/unsum/reference/sprite_generate.md)'s
output. `results` is saved as counts.parquet, along with
scale_values.parquet which maps the count columns to scale values.

`sprite_read()` is the opposite: it reads those files back into R,
recreating the original SPRITE list. This is useful for later analyses
if you don't want to re-run a lengthy
[`sprite_generate()`](https://lhdjung.github.io/unsum/reference/sprite_generate.md)
call. It also works with results that
[`sprite_generate()`](https://lhdjung.github.io/unsum/reference/sprite_generate.md)
wrote to disk itself using `path = "your/path"`.

## Usage

``` r
sprite_write(data, path)

sprite_read(
  path,
  include = c("stats_only", "stats_and_horns", "capped_error", "all"),
  samples_cap = NULL
)
```

## Arguments

- data:

  List returned by
  [`sprite_generate()`](https://lhdjung.github.io/unsum/reference/sprite_generate.md).

- path:

  String (length 1). File path where `sprite_write()` will create a new
  folder with the results. Set it to `"."` to choose the current working
  directory. For `sprite_read()`, the path to an existing folder with
  results.

- include:

  String (length 1). Which parts of the detailed results should be read
  in?

  - With `"stats_only"`, the default, no results are read.

  - `"stats_and_horns"` reads the horns index values, but not the
    samples.

  - `"capped_error"` checks whether the number of samples is higher than
    a given threshold (see `samples_cap`). If so, it throws an error;
    but if not, it reads both the samples and the horns values.

  - `"all"` reads both the samples and the horns values.

- samples_cap:

  Numeric (length 1). When using `include = "capped_error"`, enter a
  whole number here to specify a cap. Default is `NULL`.

## Value

- `sprite_write()` returns the path to the new folder it created.

- `sprite_read()` returns a list of the same kind as
  [`sprite_generate()`](https://lhdjung.github.io/unsum/reference/sprite_generate.md).

## Details

`sprite_write()` saves all tibbles as Parquet files. This is much faster
and takes up far less disk space — roughly 1% of a CSV file with the
same data. Speed and disk space can be relevant with large result sets.

Use `sprite_read()` to import the SPRITE list from the folder back into
R. This is based on
[`nanoparquet::read_parquet()`](https://nanoparquet.r-lib.org/reference/read_parquet.html).

## Folder name

The new folder's name will contain all the inputs that determine the
SPRITE results. Dashes separate values and underscores replace decimal
periods. For example:



     SPRITE-3_5-1_0-90-1-5-up_or_down-5
     

The order is the same as in
[`sprite_generate()`](https://lhdjung.github.io/unsum/reference/sprite_generate.md):



     sprite_generate(
       mean = "3.5",
       sd = "1.0",
       n = 90,
       scale_min = 1,
       scale_max = 5,
       rounding = "up_or_down",  # default
       threshold = 5             # default
     )

## Examples

``` r
data <- sprite_generate(
  mean = "2.7",
  sd = "0.6",
  n = 45,
  scale_min = 1,
  scale_max = 5,
  stop_after = 1000
)
#> Warning: SPRITE found fewer samples than requested via `stop_after`.
#> ! `stop_after` is: 1000
#> ! Samples found: 32
#> 
#> ✔ All SPRITE results found

# Writing to a temporary folder just for this example.
# You should write to a real folder instead.
# A simple way is path = "." for your current directory.
path_new_folder <- sprite_write(data, path = tempdir())
#> 
#> ✔ All SPRITE files written to:
#> /tmp/Rtmp9DZw1F/SPRITE-2_7-0_6-45-1-5-up_or_down-5/

# In a later session, conveniently read the files
# back into R. This returns the original list,
# identical except for floating-point error.
# (Of course, the `path_new_folder` variable will
# no longer be available -- instead, paste the path
# to your folder here.)
sprite_read(path_new_folder)
#> $inputs
#> # A tibble: 1 × 8
#>   technique mean  sd        n scale_min scale_max rounding   threshold
#>   <chr>     <chr> <chr> <dbl>     <dbl>     <dbl> <chr>          <dbl>
#> 1 SPRITE    2.7   0.6      45         1         5 up_or_down         5
#> 
#> $metrics_main
#> # A tibble: 1 × 2
#>   samples_all values_all
#>         <dbl>      <dbl>
#> 1          32       1440
#> 
#> $metrics_horns
#> # A tibble: 1 × 9
#>     mean uniform      sd     cv     mad    min median   max  range
#>    <dbl>   <dbl>   <dbl>  <dbl>   <dbl>  <dbl>  <dbl> <dbl>  <dbl>
#> 1 0.0875     0.5 0.00713 0.0815 0.00642 0.0758 0.0869   0.1 0.0242
#> 
#> $modality_counts
#> # A tibble: 5 × 3
#>   value count_lo count_hi
#>   <dbl>    <int>    <int>
#> 1     1        0        3
#> 2     2        7       17
#> 3     3       26       34
#> 4     4        0        3
#> 5     5        0        1
#> 
#> $modality_pairs
#> # A tibble: 4 × 4
#>   value_a value_b resolved a_greater
#>     <dbl>   <dbl> <lgl>    <lgl>    
#> 1       1       2 TRUE     FALSE    
#> 2       2       3 TRUE     FALSE    
#> 3       3       4 TRUE     TRUE     
#> 4       4       5 FALSE    FALSE    
#> 
#> $modality_conclusion
#> # A tibble: 1 × 4
#>   can_be_unimodal can_be_bimodal j_shape_low j_shape_high
#>   <lgl>           <lgl>          <lgl>       <lgl>       
#> 1 TRUE            TRUE           NA          NA          
#> 
#> $modality_shapes
#> # A tibble: 5 × 5
#>   class             n_samples value count_lo count_hi
#>   <chr>                 <dbl> <dbl>    <int>    <int>
#> 1 one_mode_interior        32     1        0        3
#> 2 one_mode_interior        32     2        7       17
#> 3 one_mode_interior        32     3       26       34
#> 4 one_mode_interior        32     4        0        3
#> 5 one_mode_interior        32     5        0        1
#> 
#> $modality_summary
#> # A tibble: 1 × 18
#>   exhaustive n_scanned min_prominence deficit_min deficit_mean deficit_max
#>   <lgl>          <dbl>          <dbl>       <int>        <dbl>       <int>
#> 1 FALSE             32           0.05           0         0.25           1
#> # ℹ 12 more variables: n_flat <dbl>, n_one_mode_interior <dbl>,
#> #   n_one_mode_low_edge <dbl>, n_one_mode_high_edge <dbl>, n_two_modes <dbl>,
#> #   n_three_or_more_modes <dbl>, band_n_flat <dbl>,
#> #   band_n_one_mode_interior <dbl>, band_n_one_mode_low_edge <dbl>,
#> #   band_n_one_mode_high_edge <dbl>, band_n_two_modes <dbl>,
#> #   band_n_three_or_more_modes <dbl>
#> 
#> $modality_prominence
#> # A tibble: 18 × 5
#>    min_prominence min_prominence_counts primary class               n_samples
#>             <dbl>                 <int> <lgl>   <chr>                   <dbl>
#>  1           0.02                     1 FALSE   flat                        0
#>  2           0.02                     1 FALSE   one_mode_interior          24
#>  3           0.02                     1 FALSE   one_mode_low_edge           0
#>  4           0.02                     1 FALSE   one_mode_high_edge          0
#>  5           0.02                     1 FALSE   two_modes                   8
#>  6           0.02                     1 FALSE   three_or_more_modes         0
#>  7           0.05                     3 TRUE    flat                        0
#>  8           0.05                     3 TRUE    one_mode_interior          32
#>  9           0.05                     3 TRUE    one_mode_low_edge           0
#> 10           0.05                     3 TRUE    one_mode_high_edge          0
#> 11           0.05                     3 TRUE    two_modes                   0
#> 12           0.05                     3 TRUE    three_or_more_modes         0
#> 13           0.1                      5 FALSE   flat                        0
#> 14           0.1                      5 FALSE   one_mode_interior          32
#> 15           0.1                      5 FALSE   one_mode_low_edge           0
#> 16           0.1                      5 FALSE   one_mode_high_edge          0
#> 17           0.1                      5 FALSE   two_modes                   0
#> 18           0.1                      5 FALSE   three_or_more_modes         0
#> 
#> $frequency
#> # A tibble: 15 × 5
#>    samples   value f_expected f_representative f_relative
#>    <chr>     <dbl>      <dbl>            <dbl>      <dbl>
#>  1 all           1      1.06                 1    0.0236 
#>  2 all           2     13.1                 13    0.290  
#>  3 all           3     29.4                 30    0.653  
#>  4 all           4      1.19                 1    0.0264 
#>  5 all           5      0.312                0    0.00694
#>  6 horns_min     1      1                    1    0.0222 
#>  7 horns_min     2     13                   13    0.289  
#>  8 horns_min     3     30                   30    0.667  
#>  9 horns_min     4      1                    1    0.0222 
#> 10 horns_min     5      0                    0    0      
#> 11 horns_max     1      1                    1    0.0222 
#> 12 horns_max     2     15                   15    0.333  
#> 13 horns_max     3     28                   28    0.622  
#> 14 horns_max     4      0                    0    0      
#> 15 horns_max     5      1                    1    0.0222 
#> 
#> $frequency_dist
#> # A tibble: 30 × 3
#>    value count n_samples
#>    <dbl> <int>     <int>
#>  1     1     0        10
#>  2     1     1        12
#>  3     1     2         8
#>  4     1     3         2
#>  5     2     7         1
#>  6     2     8         1
#>  7     2     9         1
#>  8     2    10         3
#>  9     2    11         3
#> 10     2    12         3
#> # ℹ 20 more rows
#> 
#> $directory
#> # A tibble: 1 × 1
#>   path                                               
#>   <chr>                                              
#> 1 /tmp/Rtmp9DZw1F/SPRITE-2_7-0_6-45-1-5-up_or_down-5/
#> 
```
