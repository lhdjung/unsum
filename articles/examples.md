# Usage examples

``` r

library(unsum)
```

## CLOSURE

3,800,640 values in a few seconds:

``` r

closure_generate(
  mean = "3.8",
  sd = "1.5",
  n = 120,
  scale_min = 1,
  scale_max = 5
)
#> → Just a second...
#> 
#> ✔ All CLOSURE results found
#> 
#> ── CLOSURE results: 31,672 samples ─────────────────────────────────────────────
#> $inputs · how `closure_generate()` produced these data
#> # A tibble: 1 × 8
#>   technique mean  sd        n scale_min scale_max rounding   threshold
#>   <chr>     <chr> <chr> <dbl>     <dbl>     <dbl> <chr>          <dbl>
#> 1 CLOSURE   3.8   1.5     120         1         5 up_or_down         5
#> $metrics_main · key statistics about the generated samples
#> # A tibble: 1 × 2
#>   samples_all values_all
#>         <dbl>      <dbl>
#> 1       31672    3800640
#> $metrics_horns · on the distribution of horns index values
#> # A tibble: 1 × 9
#>    mean uniform     sd     cv    mad   min median   max  range
#>   <dbl>   <dbl>  <dbl>  <dbl>  <dbl> <dbl>  <dbl> <dbl>  <dbl>
#> 1 0.555     0.5 0.0212 0.0382 0.0180 0.522  0.554 0.595 0.0738
#> With hidden elements:
#> ℹ Access $modality_counts for min/max counts per scale value (5 rows)
#> ℹ Access $modality_pairs for frequency ordering between adjacent values (4 rows)
#> ℹ Access $modality_conclusion for flagging modality and J-shape status (1 row); `NA` means not found, but the search was partial
#> ℹ Access $modality_shapes for min/max counts per scale value within each shape class (15 rows)
#> ℹ Access $modality_summary for number of samples in each shape class (1 row)
#> ℹ Access $modality_prominence for shape classes at each mode prominence threshold (18 rows)
#> ℹ Access $frequency for full frequency table (15 rows)
#> ℹ Access $frequency_dist for per-value count distributions (208 rows)
#> ℹ Access $results for count of each scale value in every sample, and horns indices (31,672 rows)
#> # Use `print(show = "all")` to see all elements
#> # Use `print(show = "none")` to hide elements
```

16,701,150 values in a few seconds:

``` r

closure_generate(
  mean = "3.0",
  sd = "1.0",
  n = 150,
  scale_min = 1,
  scale_max = 5
)
#> 
#> ✔ All CLOSURE results found
#> 
#> ── CLOSURE results: 111,341 samples ────────────────────────────────────────────
#> $inputs · how `closure_generate()` produced these data
#> # A tibble: 1 × 8
#>   technique mean  sd        n scale_min scale_max rounding   threshold
#>   <chr>     <chr> <chr> <dbl>     <dbl>     <dbl> <chr>          <dbl>
#> 1 CLOSURE   3.0   1.0     150         1         5 up_or_down         5
#> $metrics_main · key statistics about the generated samples
#> # A tibble: 1 × 2
#>   samples_all values_all
#>         <dbl>      <dbl>
#> 1      111341   16701150
#> $metrics_horns · on the distribution of horns index values
#> # A tibble: 1 × 9
#>    mean uniform     sd     cv    mad   min median   max  range
#>   <dbl>   <dbl>  <dbl>  <dbl>  <dbl> <dbl>  <dbl> <dbl>  <dbl>
#> 1 0.250     0.5 0.0143 0.0572 0.0119 0.224  0.251 0.273 0.0489
#> With hidden elements:
#> ℹ Access $modality_counts for min/max counts per scale value (5 rows)
#> ℹ Access $modality_pairs for frequency ordering between adjacent values (4 rows)
#> ℹ Access $modality_conclusion for flagging modality and J-shape status (1 row); `NA` means not found, but the search was partial
#> ℹ Access $modality_shapes for min/max counts per scale value within each shape class (15 rows)
#> ℹ Access $modality_summary for number of samples in each shape class (1 row)
#> ℹ Access $modality_prominence for shape classes at each mode prominence threshold (18 rows)
#> ℹ Access $frequency for full frequency table (15 rows)
#> ℹ Access $frequency_dist for per-value count distributions (337 rows)
#> ℹ Access $results for count of each scale value in every sample, and horns indices (111,341 rows)
#> # Use `print(show = "all")` to see all elements
#> # Use `print(show = "none")` to hide elements
```

25,278,450 values in a few seconds:

``` r

closure_generate(
  mean = "3.0",
  sd = "1.5",
  n = 150,
  scale_min = 1,
  scale_max = 5
)
#> → Just a second...
#> 
#> ✔ All CLOSURE results found
#> 
#> ── CLOSURE results: 168,523 samples ────────────────────────────────────────────
#> $inputs · how `closure_generate()` produced these data
#> # A tibble: 1 × 8
#>   technique mean  sd        n scale_min scale_max rounding   threshold
#>   <chr>     <chr> <chr> <dbl>     <dbl>     <dbl> <chr>          <dbl>
#> 1 CLOSURE   3.0   1.5     150         1         5 up_or_down         5
#> $metrics_main · key statistics about the generated samples
#> # A tibble: 1 × 2
#>   samples_all values_all
#>         <dbl>      <dbl>
#> 1      168523   25278450
#> $metrics_horns · on the distribution of horns index values
#> # A tibble: 1 × 9
#>    mean uniform     sd     cv    mad   min median   max  range
#>   <dbl>   <dbl>  <dbl>  <dbl>  <dbl> <dbl>  <dbl> <dbl>  <dbl>
#> 1 0.557     0.5 0.0214 0.0383 0.0184 0.523  0.556 0.596 0.0736
#> With hidden elements:
#> ℹ Access $modality_counts for min/max counts per scale value (5 rows)
#> ℹ Access $modality_pairs for frequency ordering between adjacent values (4 rows)
#> ℹ Access $modality_conclusion for flagging modality and J-shape status (1 row); `NA` means not found, but the search was partial
#> ℹ Access $modality_shapes for min/max counts per scale value within each shape class (30 rows)
#> ℹ Access $modality_summary for number of samples in each shape class (1 row)
#> ℹ Access $modality_prominence for shape classes at each mode prominence threshold (18 rows)
#> ℹ Access $frequency for full frequency table (15 rows)
#> ℹ Access $frequency_dist for per-value count distributions (374 rows)
#> ℹ Access $results for count of each scale value in every sample, and horns indices (168,523 rows)
#> # Use `print(show = "all")` to see all elements
#> # Use `print(show = "none")` to hide elements
```

31,235,760 values in a few seconds:

``` r

closure_generate(
  mean = "3.0",
  sd = "1.7",
  n = 180,
  scale_min = 1,
  scale_max = 5
)
#> → Just a second...
#> 
#> ✔ All CLOSURE results found
#> 
#> ── CLOSURE results: 173,532 samples ────────────────────────────────────────────
#> $inputs · how `closure_generate()` produced these data
#> # A tibble: 1 × 8
#>   technique mean  sd        n scale_min scale_max rounding   threshold
#>   <chr>     <chr> <chr> <dbl>     <dbl>     <dbl> <chr>          <dbl>
#> 1 CLOSURE   3.0   1.7     180         1         5 up_or_down         5
#> $metrics_main · key statistics about the generated samples
#> # A tibble: 1 × 2
#>   samples_all values_all
#>         <dbl>      <dbl>
#> 1      173532   31235760
#> $metrics_horns · on the distribution of horns index values
#> # A tibble: 1 × 9
#>    mean uniform     sd     cv    mad   min median   max  range
#>   <dbl>   <dbl>  <dbl>  <dbl>  <dbl> <dbl>  <dbl> <dbl>  <dbl>
#> 1 0.715     0.5 0.0242 0.0338 0.0205 0.677  0.713 0.761 0.0838
#> With hidden elements:
#> ℹ Access $modality_counts for min/max counts per scale value (5 rows)
#> ℹ Access $modality_pairs for frequency ordering between adjacent values (4 rows)
#> ℹ Access $modality_conclusion for flagging modality and J-shape status (1 row); `NA` means not found, but the search was partial
#> ℹ Access $modality_shapes for min/max counts per scale value within each shape class (10 rows)
#> ℹ Access $modality_summary for number of samples in each shape class (1 row)
#> ℹ Access $modality_prominence for shape classes at each mode prominence threshold (18 rows)
#> ℹ Access $frequency for full frequency table (15 rows)
#> ℹ Access $frequency_dist for per-value count distributions (311 rows)
#> ℹ Access $results for count of each scale value in every sample, and horns indices (173,532 rows)
#> # Use `print(show = "all")` to see all elements
#> # Use `print(show = "none")` to hide elements
```

38,740,810 values in a few seconds:

``` r

closure_generate(
  mean = "3.0",
  sd = "1.7",
  n = 190,
  scale_min = 1,
  scale_max = 5
)
#> → Just a second...
#> 
#> ✔ All CLOSURE results found
#> 
#> ── CLOSURE results: 203,899 samples ────────────────────────────────────────────
#> $inputs · how `closure_generate()` produced these data
#> # A tibble: 1 × 8
#>   technique mean  sd        n scale_min scale_max rounding   threshold
#>   <chr>     <chr> <chr> <dbl>     <dbl>     <dbl> <chr>          <dbl>
#> 1 CLOSURE   3.0   1.7     190         1         5 up_or_down         5
#> $metrics_main · key statistics about the generated samples
#> # A tibble: 1 × 2
#>   samples_all values_all
#>         <dbl>      <dbl>
#> 1      203899   38740810
#> $metrics_horns · on the distribution of horns index values
#> # A tibble: 1 × 9
#>    mean uniform     sd     cv    mad   min median   max  range
#>   <dbl>   <dbl>  <dbl>  <dbl>  <dbl> <dbl>  <dbl> <dbl>  <dbl>
#> 1 0.715     0.5 0.0242 0.0338 0.0207 0.677  0.713 0.762 0.0844
#> With hidden elements:
#> ℹ Access $modality_counts for min/max counts per scale value (5 rows)
#> ℹ Access $modality_pairs for frequency ordering between adjacent values (4 rows)
#> ℹ Access $modality_conclusion for flagging modality and J-shape status (1 row); `NA` means not found, but the search was partial
#> ℹ Access $modality_shapes for min/max counts per scale value within each shape class (10 rows)
#> ℹ Access $modality_summary for number of samples in each shape class (1 row)
#> ℹ Access $modality_prominence for shape classes at each mode prominence threshold (18 rows)
#> ℹ Access $frequency for full frequency table (15 rows)
#> ℹ Access $frequency_dist for per-value count distributions (328 rows)
#> ℹ Access $results for count of each scale value in every sample, and horns indices (203,899 rows)
#> # Use `print(show = "all")` to see all elements
#> # Use `print(show = "none")` to hide elements
```

52,231,000 values in a few seconds:

``` r

closure_generate(
  mean = "3.0",
  sd = "1.7",
  n = 200,
  scale_min = 1,
  scale_max = 5
)
#> → Just a second...
#> 
#> ✔ All CLOSURE results found
#> 
#> ── CLOSURE results: 261,155 samples ────────────────────────────────────────────
#> $inputs · how `closure_generate()` produced these data
#> # A tibble: 1 × 8
#>   technique mean  sd        n scale_min scale_max rounding   threshold
#>   <chr>     <chr> <chr> <dbl>     <dbl>     <dbl> <chr>          <dbl>
#> 1 CLOSURE   3.0   1.7     200         1         5 up_or_down         5
#> $metrics_main · key statistics about the generated samples
#> # A tibble: 1 × 2
#>   samples_all values_all
#>         <dbl>      <dbl>
#> 1      261155   52231000
#> $metrics_horns · on the distribution of horns index values
#> # A tibble: 1 × 9
#>    mean uniform     sd     cv    mad   min median   max  range
#>   <dbl>   <dbl>  <dbl>  <dbl>  <dbl> <dbl>  <dbl> <dbl>  <dbl>
#> 1 0.715     0.5 0.0241 0.0337 0.0203 0.677  0.713 0.761 0.0840
#> With hidden elements:
#> ℹ Access $modality_counts for min/max counts per scale value (5 rows)
#> ℹ Access $modality_pairs for frequency ordering between adjacent values (4 rows)
#> ℹ Access $modality_conclusion for flagging modality and J-shape status (1 row); `NA` means not found, but the search was partial
#> ℹ Access $modality_shapes for min/max counts per scale value within each shape class (10 rows)
#> ℹ Access $modality_summary for number of samples in each shape class (1 row)
#> ℹ Access $modality_prominence for shape classes at each mode prominence threshold (18 rows)
#> ℹ Access $frequency for full frequency table (15 rows)
#> ℹ Access $frequency_dist for per-value count distributions (345 rows)
#> ℹ Access $results for count of each scale value in every sample, and horns indices (261,155 rows)
#> # Use `print(show = "all")` to see all elements
#> # Use `print(show = "none")` to hide elements
```

132,389,600 values in about 3 seconds:

``` r

closure_generate(
  mean = "3.0",
  sd = "1.2",
  n = 200,
  scale_min = 1,
  scale_max = 5
)
#> → Just a second...
#> 
#> ✔ All CLOSURE results found
#> 
#> ── CLOSURE results: 661,948 samples ────────────────────────────────────────────
#> $inputs · how `closure_generate()` produced these data
#> # A tibble: 1 × 8
#>   technique mean  sd        n scale_min scale_max rounding   threshold
#>   <chr>     <chr> <chr> <dbl>     <dbl>     <dbl> <chr>          <dbl>
#> 1 CLOSURE   3.0   1.2     200         1         5 up_or_down         5
#> $metrics_main · key statistics about the generated samples
#> # A tibble: 1 × 2
#>   samples_all values_all
#>         <dbl>      <dbl>
#> 1      661948  132389600
#> $metrics_horns · on the distribution of horns index values
#> # A tibble: 1 × 9
#>    mean uniform     sd     cv    mad   min median   max  range
#>   <dbl>   <dbl>  <dbl>  <dbl>  <dbl> <dbl>  <dbl> <dbl>  <dbl>
#> 1 0.359     0.5 0.0171 0.0477 0.0148 0.329  0.360 0.389 0.0592
#> With hidden elements:
#> ℹ Access $modality_counts for min/max counts per scale value (5 rows)
#> ℹ Access $modality_pairs for frequency ordering between adjacent values (4 rows)
#> ℹ Access $modality_conclusion for flagging modality and J-shape status (1 row); `NA` means not found, but the search was partial
#> ℹ Access $modality_shapes for min/max counts per scale value within each shape class (15 rows)
#> ℹ Access $modality_summary for number of samples in each shape class (1 row)
#> ℹ Access $modality_prominence for shape classes at each mode prominence threshold (18 rows)
#> ℹ Access $frequency for full frequency table (15 rows)
#> ℹ Access $frequency_dist for per-value count distributions (491 rows)
#> ℹ Access $results for count of each scale value in every sample, and horns indices (661,948 rows)
#> # Use `print(show = "all")` to see all elements
#> # Use `print(show = "none")` to hide elements
```
