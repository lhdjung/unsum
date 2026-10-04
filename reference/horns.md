# Horns index (\\h\\)

`horns()` measures the dispersion in ordinal data based on scale limits
or min/max values. The result is the actual variance as a proportion of
the maximum possible variance. It ranges from 0 to 1:

- 0 means no variance, i.e., all observations have the same value.

- 1 means that the observations are evenly split between the extremes,
  with none in between.

`horns_uniform()` computes the value that `horns()` would return for a
uniform distribution within given scale limits. This can be useful as a
point of reference for `horns()`.

These two functions correspond to the `horns` column of `results` and
the `uniform` column of `metrics_horns` in
[`closure_generate()`](https://lhdjung.github.io/unsum/reference/closure_generate.md)'s
output.

`horns_rescaled()` is a version of `horns()` that is normalized by scale
length, such that `0.5` always indicates a uniform distribution,
independent of the number of scale points. It is meant to enable
comparison across scales of different lengths, but it is harder to
interpret for an individual scale. This makes it unlikely to be useful
in most cases. Even so, the range and the meaning of `0` and `1` are the
same as for `horns()`.

## Usage

``` r
horns(freqs, scale_min, scale_max)

horns_uniform(scale_min, scale_max)

horns_rescaled(freqs, scale_min, scale_max)
```

## Arguments

- freqs:

  Numeric. Vector with the frequencies (relative or absolute) of binned
  observations; e.g., a vector with 5 elements for a 1-5 scale.

- scale_min, scale_max:

  Numeric (length 1 each). Minimal and maximal possible values. For
  example, with a 1-7 Likert scale, use `scale_min = 1` and
  `scale_max = 7`. Prefer the empirical min and max if available: they
  constrain the possible values further.

## Value

Numeric (length 1).

## Details

The horns index \\h\\ is defined as:

\$\$ h = \frac {\sum\_{i=1}^{k} f_i (i - \bar{s})^2} {\frac{1}{4} (k -
1)^2} \$\$

where \\k\\ is the number of scale points (i.e., the length of `freqs`
here), \\f_i\\ is the relative frequency of the \\i\\th point on an
integer scale from \\1\\ to \\k\\, and \\\bar{s}\\ is the weighted mean
frequency. The mean is derived as follows:

\$\$ \bar{s} = \sum\_{i=1}^{k} i \\ f_i \$\$

Note that \\h\\ only depends on the frequency distribution, not the
actual values of the scale points. This is why both formulas invariably
use a scale from \\1\\ to \\k\\. The only reason why the `horns()`
function still takes `scale_min` and `scale_max` arguments is safety: if
`freqs` is misstated such that its length is different from the number
of points on the scale implied by those two arguments, there will be an
error.

### Maximum possible variance

The "maximum possible" variance here is based on scale range only
(Popoviciu 1935):

\$\$ \sigma\_{\max}^2 = \frac{1}{4} (k - 1)^2 \$\$

It is deliberately agnostic to the mean and the sample size. In the
context of CLOSURE, these statistics are only relevant for generating
possible samples, which then enter the equation via the numerator; just
like the standard deviation. By contrast, the number of scale points,
\\k\\, is an intrinsic property of those samples. It is sensible to
assess the variance of the samples by benchmarking it against the
greatest variance that can occur in any sample with the same \\k\\.

### Uniform distribution

Although `horns_uniform()` is implemented as a wrapper around `horns()`
that constructs a perfect uniform distribution internally, an equivalent
closed-form solution can be given as

\$\$ h_u = \frac{k + 1}{3 (k - 1)} \$\$

### "Horns of no confidence"

The term *horns index* was inspired by [Heathers
(2017)](https://jamesheathers.medium.com/sprite-case-study-3-soup-is-good-albeit-extremely-confusing-food-96ea526c488d)
which defines the "horns of no confidence" as a reconstructed sample
"where an incorrect, impossible or unlikely value set has all its
constituents stacked into its highest or lowest bins to try meet a
ludicrously high SD". In its purest form, this is a case where \\h =
1\\, so `horns()` would return `1`. However, note that the implications
for the plausibility of any given set of summary statistics depend on
the substantive context of the data ([Heathers et al.
2018](https://peerj.com/preprints/26968/)).

Heathers wrote about SPRITE, but the logic applies equally to CLOSURE.
Indeed, as CLOSURE is exhaustive but SPRITE is not, it is only CLOSURE
that provides certainty about the set of possible samples and their
horns indices.

### Rust implementation

The `metrics_horns` tibble that is part of the output of
[`closure_generate()`](https://lhdjung.github.io/unsum/reference/closure_generate.md)
is not based on the R functions presented here. Instead, it relies on
efficient Rust implementations of the above formulas. These Rust
functions are part of
[closure-core](https://github.com/lhdjung/closure-core), which mainly
implements CLOSURE but does not currently export horns functions for
users.

## References

Popoviciu, T. (1935). Sur les équations algébriques ayant toutes leurs
racines réelles. *Mathematica* (Cluj), 9, 129-145.

## Examples

``` r
# For simplicity, all examples use a 1-5 scale and a total N of 300.

# ---- With all values at the extremes

horns(freqs = c(300, 0, 0, 0, 0), scale_min = 1, scale_max = 5)
#> [1] 0

horns(c(150, 0, 0, 0, 150), 1, 5)
#> [1] 1

horns(c(100, 0, 0, 0, 200), 1, 5)
#> [1] 0.8888889


# ---- With some values in between

horns(c(60, 60, 60, 60, 60), 1, 5)
#> [1] 0.5

horns(c(200, 50, 30, 20, 0), 1, 5)
#> [1] 0.2113889

horns(c(150, 100, 50, 0, 0), 1, 5)
#> [1] 0.1388889

horns(c(100, 40, 20, 40, 100), 1, 5)
#> [1] 0.7333333
```
