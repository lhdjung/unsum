# Get started

``` r

library(unsum)
```

Here is a brief walkthrough of using CLOSURE in unsum.

Call
[`closure_generate()`](https://lhdjung.github.io/unsum/reference/closure_generate.md)
to run the CLOSURE algorithm. Enter mean, SD, and sample size that you
read in a paper. For `scale_min` and `scale_max`, use the empirical
minimum and maximum if available. Otherwise, use the more fundamental
scale bounds, e.g., `1` and `7` for a 1-7 scale.

The `mean` and `sd` arguments must be strings to preserve trailing
zeros. Note that CLOSURE can only be used if the values must be
integers: e.g., a value can be 2 or 3, but not 2.5.

``` r

data <- closure_generate(
  mean = "3.5",
  sd = "1.8",
  n = 80,
  scale_min = 1,
  scale_max = 5
)
#> 
#> ✔ All CLOSURE results found
```

First create a plot of the mean sample found by CLOSURE. This gives us a
sense of the overall results, which are quite polarized:

``` r

closure_plot_bar(data)
```

![Barplot of \`data\`, the CLOSURE output. It specifically visualizes
the \`f_average\` column of the \`frequency\` tibble, but also gives
percentage figures, similar to the \`f_relative\` column. The overall
shape is a strongly polarized
distribution.](unsum_files/figure-html/unnamed-chunk-3-1.png)

You can customize the plot, e.g., to show the sum of all samples found
instead of the average sample, or only percentages, or different colors.
See documentation at
[`closure_plot_bar()`](https://lhdjung.github.io/unsum/reference/closure_plot_bar.md).
However, the default should be informative enough for a start.

## CLOSURE results

Now let’s look at the results themselves:

``` r

data
#> 
#> ── CLOSURE results: 2,215 samples ──────────────────────────────────────────────
#> $inputs · how `closure_generate()` produced these data
#> # A tibble: 1 × 8
#>   technique mean  sd        n scale_min scale_max rounding   threshold
#>   <chr>     <chr> <chr> <dbl>     <dbl>     <dbl> <chr>          <dbl>
#> 1 CLOSURE   3.5   1.8      80         1         5 up_or_down         5
#> $metrics_main · key statistics about the generated samples
#> # A tibble: 1 × 2
#>   samples_all values_all
#>         <dbl>      <dbl>
#> 1        2215     177200
#> $metrics_horns · on the distribution of horns index values
#> # A tibble: 1 × 9
#>    mean uniform     sd     cv    mad   min median   max  range
#>   <dbl>   <dbl>  <dbl>  <dbl>  <dbl> <dbl>  <dbl> <dbl>  <dbl>
#> 1 0.792     0.5 0.0251 0.0317 0.0189 0.756  0.787 0.844 0.0877
#> With hidden elements:
#> ℹ Access $modality_counts for min/max counts per scale value (5 rows)
#> ℹ Access $modality_pairs for frequency ordering between adjacent values (4 rows)
#> ℹ Access $modality_conclusion for flagging modality and J-shape status (1 row); `NA` means not found, but the search was partial
#> ℹ Access $modality_shapes for min/max counts per scale value within each shape class (10 rows)
#> ℹ Access $modality_summary for number of samples in each shape class (1 row)
#> ℹ Access $modality_prominence for shape classes at each mode prominence threshold (18 rows)
#> ℹ Access $frequency for full frequency table (15 rows)
#> ℹ Access $frequency_dist for per-value count distributions (86 rows)
#> ℹ Access $results for count of each scale value in every sample, and horns indices (2,215 rows)
#> # Use `print(show = "all")` to see all elements
#> # Use `print(show = "none")` to hide elements
```

- `inputs` records the arguments in
  [`closure_generate()`](https://lhdjung.github.io/unsum/reference/closure_generate.md).

- `metrics_main` shows the number of initial samples that form the basis
  of CLOSURE (`samples_initial`), the total number of possible samples
  that could have led to the reported summary statistics
  (`samples_all`), and the total number of all values found in them
  (`values_all`).

- `metrics_horns` contains statistics about the “horns index” of each
  sample, a measure of variation in bounded scales. It ranges from 0 to
  1, where 0 means no variability and 1 would be a sample evenly split
  between the extremes — here, 1, and 5 — with no values in between. In
  particular:

  - `mean` is the average horns index across all samples. The reference
    value `uniform` shows which value `mean` would have if the mean
    sample was uniformly distributed. This is 0.5 because of the 1-5
    scale. See
    [`horns()`](https://lhdjung.github.io/unsum/reference/horns.md) for
    more details.

  - The actual `mean` horns index is 0.79, which is a high degree of
    variability even in the abstract. In practice, 0.79 might be
    extremely high compared to theoretical expectations: if the sample
    should have a roughly normal shape, even the hypothetical 0.5
    uniform value would be surprisingly high, let alone the 0.79 actual
    value.

  - The remaining statistics provide some clues about the variability
    among horns values. More on this below.

- `frequency` shows the absolute and relative frequencies of values
  found by CLOSURE at each scale point. It also contains the (absolute)
  frequency of values in the average sample that we saw in the plot
  above. The `samples` column indicates to which subset of the samples a
  row belongs:

  - `all` is aggregated across all samples.

  - `horns_min` concerns the subset of samples that have the lowest
    horns index from among all samples.

  - `horns_max`, conversely, is about the subset of samples with the
    highest horns index.

- `results` stores all the samples that CLOSURE found and the
  corresponding horns index values (`horns`). Each has a unique number
  (`id`). A sample is stored as the number of times each scale value
  occurs in it: `v1` counts the 1s, `v2` the 2s, and so on. This
  describes each sample completely because the order of values doesn’t
  matter, and it takes up much less memory than listing every value.

See
[`closure_generate()`](https://lhdjung.github.io/unsum/reference/closure_generate.md)
for more details.

In addition to the bar plot, unsum offers an ECDF plot for CLOSURE
results:

``` r

closure_plot_ecdf(data)
```

![Empirical cumulative distribution function (ECDF) plot of \`data\`,
the CLOSURE output. The curve rises most steeply at the first and last
scale values, indicating a strongly polarized
distribution.](unsum_files/figure-html/unnamed-chunk-5-1.png)

## Horns index variation

You may wonder about variability between the samples. Couldn’t there be
some with a much lower or higher horns index than the overall mean
`horns`? In this case, there would be a chance that the original data
looked quite different from the average.

To check this, first use
[`closure_plot_bar()`](https://lhdjung.github.io/unsum/reference/closure_plot_bar.md).
It shows the minimum and maximum possible variability as measured by the
horns index, i.e., the average distributions of those samples with the
lowest and highest horns index values:

``` r

closure_plot_bar(data, format = "percent")
```

![Two barplots like the first, except they show the subsets of samples
within \`data\` with minimum and maximum horns index
values.](unsum_files/figure-html/unnamed-chunk-6-1.png)

As you can see, the variability does not change very much. The
distribution is starkly bimodal even with the lowest possible amount of
variability.

In sum, the horns values are quite tightly confined. Wide variation
among them seems to occur only if `mean` and `sd` have no decimal
places.

## Read and write

What if you have a huge object with CLOSURE results that you want to
save? Write it to disk with
[`closure_write()`](https://lhdjung.github.io/unsum/reference/closure_write.md):

``` r

# Using a temporary folder via `tempdir()` just for this example --
# you should use a real folder on your computer instead!
path_new_folder <- closure_write(data, path = tempdir())
#> 
#> ✔ All CLOSURE files written to:
#> /tmp/RtmpFBbPTA/CLOSURE-3_5-1_8-80-1-5-up_or_down-5/
```

This stores the results using the highly efficient Parquet format. It
will only take a tiny fraction of a CSV file’s disk space.

In your later session, read the data in from the folder to get the same
CLOSURE list back:

``` r

data_new <- closure_read(path_new_folder)
```

A caveat: don’t modify the output of
[`closure_generate()`](https://lhdjung.github.io/unsum/reference/closure_generate.md)
before passing it into other `closure_*()` functions. The latter need
input with a very specific format, and if you manipulate the data
between two `closure_*()` calls, these assumptions may no longer hold.
Some checks are in place to detect alterations, but they may not catch
all of them.
