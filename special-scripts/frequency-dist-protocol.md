---
editor_options: 
  markdown: 
    wrap: 72
---

# Protocol: Distilled Frequency Distribution (`frequency_dist`)

## Overview

This document records the design decisions and implementation steps for
adding a *distilled frequency distribution* to the CLOSURE pipeline. The
feature spans two repositories:

-   **`closure-core`** (Rust crate, `frequency-details` branch) —
    computes and stores the distribution
-   **`unsum`** (R package, `master` branch) — exposes it to R and tests
    it

------------------------------------------------------------------------

## Motivation

The existing `$frequency` table reports, for each scale value, the
average frequency across all samples (`f_average`), the total count
(`f_absolute`), and the relative frequency (`f_relative`). This
aggregates away the *distribution* of per-sample counts.

The new `frequency_dist` captures what that aggregate conceals: for each
scale value `v`, how many samples contained `v` exactly `k` times (for
each `k = 0, 1, ..., n`)? This is needed for richer visualisations — in
particular, for the planned ribbon/band overlays in
`plot_frequency_bar()` — where the width of each horizontal band
represents a meaningful quantile of the per-sample count distribution
rather than just a point estimate.

------------------------------------------------------------------------

## Schema

The distribution is stored as a flat, long-format table with three
columns:

| Column | Type | Meaning |
|----|----|----|
| `value` | integer | Scale value (e.g. 1–5) |
| `count` | integer | Number of times `value` appeared in a single sample (0–n) |
| `n_samples` | integer | Number of samples with this (value, count) pair |

Only rows where `n_samples > 0` are stored. The full cross-product
`scale_values × (0..=n)` is never materialised in the output.

A list-column was considered and ruled out: `nanoparquet` does not
support nested Parquet types, so a flat long table is the only viable
on-disk representation.

------------------------------------------------------------------------

## Understanding `frequency_dist`

### Row count and the theoretical maximum

The number of rows cannot be inferred from any other field without
reading `frequency_dist` itself (or the file it was read from). It
equals the number of `(value, count)` pairs where `n_samples > 0` —
which depends on the actual sample set, not just the inputs.

What *can* be computed from `$inputs` alone is the upper bound:

``` r
max_rows <- (scale_max - scale_min + 1) * (n + 1)
```

This is the full cross-product of all scale values × all possible counts
(0 through n). The actual row count is always ≤ `max_rows`, often
considerably less.

The lower bound is `scale_max - scale_min + 1` (one row per scale
value). Every sample assigns every scale value exactly one count —
between 0 and n, and summing to n across all values — so for each scale
value, at least one count must appear at least once across all samples.
This guarantees at least one row per value.

### The fill ratio

Define the fill ratio as `actual_rows / max_rows`. Its range depends on
whether any samples were found:

-   **Empty result set (0 samples):** `frequency_dist` has 0 rows, so
    the ratio is 0. This is a degenerate case distinct from the
    non-empty minimum below.
-   **Non-empty result set:** the theoretical range is `[1/(n+1), 1]`.

**Why the non-empty minimum is 1/(n+1):**

The minimum number of rows is `scale_max - scale_min + 1` (one per
value), and `max_rows = (scale_max - scale_min + 1) * (n + 1)`, so:

```         
min ratio = (scale_max - scale_min + 1)
          / ((scale_max - scale_min + 1) * (n + 1))
          = 1 / (n + 1)
```

The `n_scale_vals` terms cancel, leaving a minimum that depends only on
n. Intuitively: no matter how many scale values there are, each value
can be "saturated" or "unsaturated" independently, so the minimum is set
by the per-value case alone, which is 1 distinct count out of n + 1
possible counts.

The minimum is achieved exactly when every scale value has the same
count in every sample — i.e., all CLOSURE samples are identical, or
(less extremely) all valid samples happen to assign each value the same
count despite differing in other ways. The per-value distribution is
then a perfect spike: one bar at full height, all other bars empty.

**Why the maximum is 1:**

Every `(value, count)` cell is occupied. For this to hold, each scale
value must have been assigned every count from 0 to n in at least one
sample. This requires a large and varied solution space.

### Shape at ratio = 1: why unimodal

At ratio = 1, the count distribution for each value has no gaps. The
shape will still be unimodal in practice. Here is why.

Each CLOSURE sample imposes two global constraints: the sample mean and
SD must match the reported values. For a given scale value v, its count
k must be consistent with these constraints simultaneously for all
values. Moving k far from its "expected" level — the count that would
occur if all values were proportionally represented — requires
compensating changes in the other values' counts, which become
increasingly difficult to satisfy jointly as k moves toward 0 or n.
Samples where k is very low or very high for v are therefore rarer, not
because those counts can't exist (at ratio = 1 they do), but because
fewer overall configurations are compatible with the constraints when v
is pushed to an extreme. This is precisely the mechanism that produces a
unimodal concentration around the expected count, with mass falling off
symmetrically (or asymmetrically for skewed distributions) toward both
ends.

The unimodality is not a mathematical guarantee for all conceivable
inputs, but it holds generically because the CLOSURE constraints act as
a soft centering force. A bimodal count distribution for a single value
would require two distinct clusters of valid samples that each place
that value at a different characteristic count — unusual and only
possible with specific, somewhat degenerate input combinations.

Values near the extremes of the scale (far from the mean) will have
their distributions concentrated near low counts (they rarely appear in
any sample), while values near the mean will peak at a moderate count.
The ratio reaching 1 means all bars are positive, but the underlying
unimodal shape remains intact.

### Shape near the minimum: no particular form implied

Near the minimum, count distributions are concentrated on very few
distinct counts — in the limit, one bar per value. Crucially, the
*location* of that concentration is not determined by the ratio: it is
set by the mean and SD, not by the fill ratio itself. The single
occupied count for value v could be:

-   Near 0 (v is rare in every sample — v is far above or below the
    mean)
-   Moderate (v appears consistently a few times per sample)
-   Near n (v dominates every sample — extreme mean with low SD)

This is unlike the ratio = 1 case, where the unimodal shape gives you
information about where the count distribution is centred. Near the
minimum, the ratio tells you only that the distribution is narrow — not
where it is centred. The centre is determined by the inputs; the width
is determined by the size and diversity of the solution space.

The practical implication: two datasets can have the same fill ratio
near the minimum but look completely different when plotted — one might
be a spike at count 0 (a value that never appears), another a spike at
count 5 (a value that consistently appears 5 times). The ratio
summarises width, not location.

### What the ratio tells you about the CLOSURE solution space

The fill ratio is a summary measure of *count diversity* across the
solution space: how much uncertainty remains about how often each scale
value appears.

| Ratio | Interpretation |
|----|----|
| Near `1/(n+1)` | Tightly constrained. Nearly all samples have identical value counts. The reported mean and SD almost completely determine the frequency distribution. |
| Intermediate | Moderate uncertainty. Value counts vary across samples within a bounded range. The typical case. |
| Near 1 | Highly unconstrained. Many different frequency distributions are compatible with the reported statistics. The solution space is large and diverse. |

### Relationship to the Horns index

The Horns index measures the *shape* of the aggregated frequency
distribution — how U-shaped (horn-like) versus bell-shaped or uniform it
is. The fill ratio measures how much that distribution varies *across
samples*. They appear to be related, but the relationship is subtler
than it seems, and empirical data reveal an important asymmetry.

**The Horns index is dominated by extreme values.** It is sensitive to
how much mass sits at the scale endpoints relative to the middle. If the
extreme values are tightly constrained across samples, the Horns score
is tightly constrained too — even if the middle values vary enormously.
The following example illustrates this:

```         
mean = 3.50, sd = 1.75, n = 86, scale 1–5
Horns range: 0.006 (min 0.754, max 0.760)

frequency_dist:
  value 1: counts 17–27   (11 distinct counts — narrow, unimodal)
  value 5: counts 38–48   (11 distinct counts — narrow, unimodal)
  value 2: counts 0–20    (20 distinct counts — nearly flat)
  value 3: counts 0–15    (11 distinct counts, with gaps)
  value 4: counts 0–21    (20 distinct counts — nearly flat)
```

Values 1 and 5 together account for \~78% of all responses. Their count
distributions are narrow and unimodal: a high-SD, U-shaped input forces
the extreme values to appear consistently in almost every valid sample.
The Horns score, being a weighted function of extreme-value mass,
therefore barely moves across samples.

Values 2, 3, and 4 are individually rare (\~5–8% each on average), but
they are free to trade counts among themselves in almost any
combination. Shifting a response from value 2 to value 4 barely affects
the Horns index, because both values sit in the "middle" of the U-shape
— the Horns calculation is nearly insensitive to rebalancing within the
middle. As a result, values 2–4 can take nearly the full range of
possible counts (0 to n), producing high fill ratio even though the
Horns index stays fixed.

**The core insight:** the fill ratio reflects variation *anywhere* in
the distribution; the Horns range reflects only variation in the
*extremes*. They can diverge dramatically when extreme values are
tightly pinned while middle values compensate freely — which is exactly
what high SD tends to produce.

| Horns range | Fill ratio | What it means |
|----|----|----|
| Low | Low | Extremes fixed, middle fixed too. Nearly all samples are identical. |
| Low | High | Extremes fixed, middle compensates freely. Horns score is stable but internal composition varies a lot. The typical high-SD case. |
| High | Low | Extremes vary, middle is rigid. Unusual. |
| High | High | Both extremes and middle vary substantially. Maximally unconstrained inputs. |

The practically important case is the second row.

**Why the `$frequency` table cannot reveal this, even broken out by
Horns group.** Looking at the same example, `$frequency`'s `horns_min`
and `horns_max` groups appear nearly identical to the overall "all"
averages — despite `frequency_dist` showing that values 2–4 range from 0
to 20+ across individual samples. There is no contradiction. The
`f_average` for each Horns group is computed by averaging over roughly
30 samples, and that averaging washes out per-sample variation
completely. Sample A might have value 2 = 0 and value 4 = 14; sample B
might have value 2 = 15 and value 4 = 2. Both can have nearly the same
Horns score and land in the same Horns group, because both are
rebalancings within the middle that barely affect the Horns calculation.
The group average converges to roughly 7 for both values regardless.

Sorting by Horns does not sort by any individual middle value — it sorts
only by a function of the extremes. So the `horns_min` and `horns_max`
group averages for middle values both look like the overall mean; the
wide per-sample distributions shown in `frequency_dist` are real, but
they are **orthogonal to Horns** and cancel entirely when averaged
within any Horns group. `frequency_dist` is the only place this
variation is visible.

**What this means for the joint interpretability of `horns_min` and
`horns_max`.** The implicit assumption behind comparing these two groups
is: *they are the most extreme samples in the set, so their difference
bounds the full range of distributional variation*. If they look nearly
identical, the distribution must be tightly determined for all samples.

This assumption holds only along the Horns dimension. The valid CLOSURE
samples occupy a space with `n_scale_vals` degrees of freedom. The Horns
index projects that space onto a single number. `horns_min` and
`horns_max` are the endpoints of that one-dimensional projection.
Directions orthogonal to it — in this case, the rebalancing of counts
among middle values — are completely invisible to the comparison.

"horns_min ≈ horns_max" therefore means *the degree of U-shapedness is
tightly constrained*, and nothing more. It does not mean all samples are
similar. Two samples with value 2 = 0 and value 2 = 15 can belong to the
same Horns group, or even to opposite Horns groups, without
contradiction — and their group averages converge regardless.

The practical consequence is that the `horns_min`/`horns_max` comparison
can **actively mislead** in the high-SD case. It makes the distribution
look more tightly determined than it is, because the dimension along
which it correctly reports tight determination (Horns) happens to be the
one that is genuinely constrained, while the dimensions along which it
is silent (middle-value composition) happen to be the ones that are
free. A reader seeing nearly identical `horns_min` and `horns_max`
columns would reasonably but incorrectly conclude that all samples look
alike.

The correct joint reading requires both:

-   **Horns range** (or `horns_min`/`horns_max`): answers *how much does
    the degree of extremity vary?*
-   **`frequency_dist`**: answers *how much does the count of each
    individual value vary across samples?*

These are complementary and can sharply disagree. A low Horns range with
high fill ratio for middle values is not a contradiction — it is a
specific and informative finding: the extremes are pinned, the interior
is free. Neither measure alone produces this reading, and the
`horns_min`/`horns_max` comparison alone actively obscures it.

**Two distinct kinds of uncertainty, and why the boundaries between them
are not where they first appear.**

Rebalancing counts *among middle values* (value 2 rising while value 3
falls) barely moves the Horns index: both values sit in the middle
region of the scale. High saturation can therefore coexist with a narrow
Horns range without any contradiction.

It is tempting to conclude that *any* ordering flip between an extreme
and a near-middle value *must* produce a large Horns change — and
therefore that a narrow Horns range rules out such flips entirely. This
turns out to be wrong, and the mechanism matters.

Consider the example above (mean = 3.50, sd = 1.75, n = 86, scale 1–5),
where h takes exactly two values: 0.754 and 0.760, alternating
throughout all 66 samples. The Horns "range" of 0.006 is not a
continuous spread — it is the gap between two discrete values. Using
`closure_plot_bar(overlay = "dots")`, the per-sample frequencies reveal:

```         
value 1:  17–27 counts  (19.8%–31.4%)
value 2:   0–20 counts  ( 0%  –23.3%)
value 3:   0–15 counts  ( 0%  –17.4%)
value 4:   0–21 counts  ( 0%  –24.4%)
value 5:  38–48 counts  (44.2%–55.8%)
```

Values 1 and 2 overlap (17–20 counts), and values 1 and 4 overlap (17–21
counts). A sample with value 2 = 20 and value 1 = 17 has value 2 \>
value 1, even within h ∈ {0.754, 0.760}.

**Why does this not destroy the Horns index?** When value 2 is elevated
(\~20 counts), the mean constraint forces value 5 to rise simultaneously
(to \~48 counts, 55.8%), while values 3 and 4 drop to near zero. The
increase in value 5 — the most extreme scale point — compensates in the
Horns calculation for the fall in value 1 and the rise in value 2,
keeping h nearly constant. The compensation that stabilises h is not
confined to middle-value rebalancing: it can involve an extreme value
increasing while a near-extreme value decreases, as long as the combined
effect on the Horns function is neutral.

Value 3 (the true middle of the scale), however, does not overlap with
value 1: its maximum is 15 counts, while value 1's minimum is 17.
Compare `horns(c(50, 20, 10, 20, 50), 1, 5)` ≈ 0.73 against
`horns(c(20, 20, 40, 20, 50), 1, 5)` ≈ 0.49 — a difference of \~0.24.
Value 3 overtaking value 1 would require that kind of redistribution,
which cannot be compensated within a Horns range of 0.006. The true
central value of the scale is the one whose ordering relative to the
extremes is robustly pinned.

**What the modality inference actually delivers.** This leads to a
precise two-part decomposition:

-   *Is the distribution non-unimodal (polarised)?* **Yes, reliably.**
    All 66 valid samples are clearly polarised: near-zero at value 3,
    moderate at the near-extreme values, high at value 5. No valid
    sample is bell-shaped. This inference is what h is designed to
    support, and a narrow, high h delivers it.

-   *Is the non-unimodal form specifically a symmetric U-shape?*
    **Uncertain.** Some samples are nearly symmetric (value 1 ≈ 23,
    value 5 ≈ 44), others are asymmetric J-shapes (value 1 ≈ 17, value 2
    ≈ 20, value 5 ≈ 48). The distribution was definitely polarised, but
    whether both extremes were roughly balanced, or one extreme strongly
    dominated with the adjacent value also elevated, cannot be
    determined. h is silent on this distinction; `frequency_dist`
    exposes it.

The bands in the visualisation should reflect this directly: narrow for
value 3 (tightly pinned), narrow for value 5 (tightly pinned by the
compensation mechanism described above), and wide for values 1, 2, and 4
(where the ordering can shift without materially affecting h).

Genuine *modality uncertainty* — whether the original distribution was
unimodal or bimodal — requires a wide Horns range. That scenario is
identified by h, not by saturation. High saturation with narrow h means:
the *existence* of polarisation is certain, but the *form* of
polarisation is not.

### Formal ordering and modality criteria

The modality classification described above can be stated precisely in
terms of per-value count ranges that are directly readable from
`frequency_dist`. This section gives the definitions, states each
criterion as a theorem with proof, and works through two examples.

#### Notation and setup

Let $\mathcal{V} = \{v_1 < v_2 < \cdots < v_k\}$ be the scale values
($v_i = \text{scale\_min} + i - 1$). Let $\mathcal{S}$ be the set of all
valid CLOSURE samples. For each sample $s \in \mathcal{S}$ and each
scale value $v_i$, let $c_i(s)$ be the count of $v_i$ in sample $s$.
Every sample satisfies:

$$c_i(s) \in \{0, 1, \ldots, n\} \quad \text{and} \quad \sum_{i=1}^{k}
c_i(s) = n.$$

#### Count ranges

The **count range** of $v_i$ across all valid samples is:

$$lo_i \;:=\; \min_{s \,\in\, \mathcal{S}} c_i(s), \qquad hi_i \;:=\;
\max_{s \,\in\, \mathcal{S}} c_i(s).$$

These are computed from `frequency_dist` as the minimum and maximum
values in the `count` column for rows where `value == v_i`. They
represent the *projected coordinate range* of the CLOSURE solution set
along each scale-value axis. Some immediate consequences:

-   $0 \leq lo_i \leq hi_i \leq n$ for all $i$.
-   If $lo_i = hi_i$, then $v_i$ has the same count in every sample (a
    spike distribution).
-   $\sum_{i=1}^{k} lo_i \leq n$ (summing the minima never exceeds $n$,
    because the minima can be attained approximately simultaneously).
-   $\sum_{i=1}^{k} hi_i \geq n$ (summing the maxima never falls below
    $n$, for the symmetric reason).

The count ranges are exposed in R as `$modality_counts`,
a 1-row-per-scale-value data frame with columns `value`, `count_lo`
($= lo_i$), and `count_hi` ($= hi_i$).

#### Ordering resolution

**Definition.** The ordering between $v_i$ and $v_j$ is *resolved* if
the sign of $c_i(s) - c_j(s)$ is the same for every $s \in \mathcal{S}$.
It is *unresolved* if both orderings ($c_i > c_j$ and $c_i < c_j$)
appear in at least one sample each.

**Theorem (Sufficient conditions for resolved ordering).**

1.  If $lo_i > hi_j$, then $c_i(s) > c_j(s)$ for every $s \in
    \mathcal{S}$ (resolved; $v_i$ always more frequent).
2.  If $hi_i < lo_j$, then $c_i(s) < c_j(s)$ for every $s \in
    \mathcal{S}$ (resolved; $v_j$ always more frequent).
3.  If neither (1) nor (2) holds — i.e., the intervals $[lo_i, hi_i]$
    and $[lo_j, hi_j]$ overlap — then both orderings occur in actual
    samples (unresolved).

*Proof of (1).* For every $s$: $c_i(s) \;\geq\; lo_i \;>\; hi_j
\;\geq\; c_j(s)$, so $c_i(s) > c_j(s)$. $\square$

*Proof of (2).* For every $s$: $c_i(s) \;\leq\; hi_i \;<\; lo_j
\;\leq\; c_j(s)$, so $c_i(s) < c_j(s)$. $\square$

*Remark on (3).* Overlapping ranges are a *necessary* condition for
ordering uncertainty. In the CLOSURE context they are also *empirically
sufficient*: when the count ranges of two values overlap, samples that
realise both orderings are observed in practice, as confirmed by the
`dots` overlay in `closure_plot_bar()`. A theoretical proof would
require characterising the CLOSURE solution set in full generality,
which is an open problem; (3) is an empirically confirmed claim.

**Adjacent pairs.** The data frame `$modality_pairs`
records the resolution status for each consecutive pair $(v_i,
v_{i+1})$, $i = 1, \ldots, k-1$. Columns: `value_a` ($= v_i$), `value_b`
($= v_{i+1}$), `resolved` (TRUE iff condition (1) or (2) holds),
`a_greater` (TRUE iff condition (1) holds and `resolved` is TRUE,
meaning $v_i$ is always more frequent than $v_{i+1}$).

#### The `can_be_unimodal` flag

**Definition.** A sample $s$ is *unimodal* if its frequency vector is
non-decreasing up to some peak and non-increasing thereafter — no
valley. The peak may sit at any scale value, not only the centre. Ties
and plateaus are permitted; the only forbidden pattern is a strict
decrease followed by a strict increase.

$$\text{can\_be\_unimodal} \;=\; \exists\, s \in \mathcal{S} : \text{freqs}(s) \text{ is unimodal}.$$

The flag is computed by iterating over every sample and checking the
shape of its frequency vector.

-   `can_be_unimodal = TRUE`: at least one valid sample has a
    single-peaked frequency profile.
-   `can_be_unimodal = FALSE`: every valid sample has at least one
    valley; no unimodal sample exists in $\mathcal{S}$.

**Why "general shape" rather than "peaked at the centre".**
An earlier draft required the mode to fall at the true-middle index
$v_m$. That is too restrictive: a slightly skewed input (mean $\neq$
midpoint) produces a valid bell-shaped sample whose peak sits off-centre
— physically unimodal, but invisible to the centre-mode check.
The general shape definition captures all single-peaked samples
regardless of where the peak lies.

This is an exact answer over $\mathcal{S}$, not an approximation from
count ranges.

#### The `can_be_bimodal` flag

**Definition.** A sample $s$ is *bimodal with the mean between the
modes* when it has at least two *qualifying* strict local maxima and the
sample mean lies strictly between the leftmost and rightmost such peak
values. A qualifying peak must (a) be a strict local maximum — both
immediate neighbours strictly lower, boundary indices treated as
$-\infty$ — and (b) have a frequency strictly above the per-value
average $\bar{f} = n / k$ (total count divided by number of scale
values).

$$\text{can\_be\_bimodal} \;=\; \exists\, s \in \mathcal{S} : \lvert\text{peaks}^+(s)\rvert \geq 2 \;\text{ and }\; v_{\text{peak}_1} < \bar{s} < v_{\text{peak}_L},$$

where $\text{peaks}^+(s)$ are the qualifying peaks (above-average strict
local maxima), $\text{peak}_1$ is the leftmost, $\text{peak}_L$ the
rightmost, and $\bar{s}$ is the sample mean. The flag is computed in the
same per-sample pass as `can_be_unimodal`.

-   `can_be_bimodal = TRUE`: at least one valid sample is bimodal with
    the reported mean falling between the two modes.
-   `can_be_bimodal = FALSE`: no such sample exists.

**Why "general two-peak" rather than "both extremes above centre".**
An earlier draft required $c_1(s) > c_m(s)$ and $c_k(s) > c_m(s)$ —
both scale *extremes* must beat the *centre*. This misses genuine
bimodal distributions whose modes sit off the scale endpoints (e.g.
modes at values 2 and 6 on a 1–7 scale). The general definition
identifies any sample whose frequency profile has two above-average
humps straddling the reported mean.

**Why the above-average threshold.**
Without it, any sample with a small uptick near the far boundary is
called bimodal. For example, $c(50, 40, 20, 5, 0, 0, 5)$ on a 1–7
scale is plainly a J-shape (strong concentration at value 1 with a
negligible tail at value 7), yet it has two strict local maxima and its
mean ($\approx 2.0$) lies between values 1 and 7. Requiring each peak
to exceed $\bar{f} = n/k$ filters out such noise: the count 5 at
value 7 is far below the average of $\approx 17$, so it is not a
qualifying mode. A symmetric U-shape like $c(50, 10, 3, 10, 50)$ has
both boundary peaks well above average and is correctly flagged.

For $k \leq 2$ the flag returns `FALSE` by convention (fewer than three
distinct values, so no valley is possible).

This is an exact answer over $\mathcal{S}$, not an approximation from
count ranges.

**Are these flags "too granular"?** The flags are existential claims
over the full solution set $\mathcal{S}$: they tell the researcher
whether any valid reconstruction of the original data has a particular
global shape. This is still a statement about *tendency* and
*possibility*, not about a single sample. A `can_be_unimodal = TRUE`
finding means: given the reported mean, SD, and n, it is *possible* that
the underlying data had a single-peaked distribution. The per-sample
check is just the mathematically correct way to evaluate that
possibility — it avoids the false positives that marginal count-range
comparisons produce when the variance constraint couples the counts
across scale values. Precision in the criterion does not mean we are
making a claim about individual samples; it means we are making an
accurate claim about the set.

#### The J-shape flags

A distribution is *J-shaped at the low end* when $c_1 < c_2$: the
distribution rises from left to right at the low end, so the second
value exceeds the first. The flag `j_shape_low` tests whether this
pattern is possible in any valid sample:

$$j\_\text{shape\_low} \;=\; (hi_2 > lo_1).$$

Similarly, *J-shaped at the high end* means $c_{k-1} > c_k$ (the
distribution rises toward the second-highest value rather than the
highest):

$$j\_\text{shape\_high} \;=\; (hi_{k-1} > lo_k).$$

For $k \leq 2$, both flags return `FALSE` by convention.

**Theorem (Clean flanks).** If $j\_\text{shape\_low} = \text{FALSE}$
(i.e., $hi_2 \leq lo_1$), then $c_1(s) \geq c_2(s)$ for every $s \in
\mathcal{S}$: the lowest scale value always counts at least as often as
the second-lowest.

*Proof.* For every $s$: $c_2(s) \leq hi_2 \leq lo_1 \leq c_1(s)$.
$\square$

*Symmetric theorem.* If $j\_\text{shape\_high} = \text{FALSE}$ (i.e.,
$hi_{k-1} \leq lo_k$), then $c_{k-1}(s) \leq c_k(s)$ for every $s$: the
highest value always counts at least as often as the second-highest.

*Proof.* For every $s$: $c_{k-1}(s) \leq hi_{k-1} \leq lo_k \leq
c_k(s)$. $\square$

*Remark.* The converses hold in the same empirical sense as for ordering
resolution: if $hi_2 > lo_1$ (the ranges of $v_1$ and $v_2$ overlap on
the upper end), samples where $c_2 > c_1$ are observed in practice.

#### Flag combinations and their interpretations

| `can_be_unimodal` | `can_be_bimodal` | `j_shape_low` | `j_shape_high` | Interpretation |
|:--:|:--:|:--:|:--:|:---|
| TRUE | FALSE | any | any | Shape undetermined toward bell; bimodal structure ruled out. |
| TRUE | TRUE | any | any | Shape fully undetermined — both bell-shaped and U-shaped samples are possible. |
| FALSE | FALSE | FALSE | FALSE | **Proven non-unimodal; bimodal ruled out; clean flanks.** Extremes dominate but the bimodal U-shape is not the explanation (unusual). |
| FALSE | TRUE | FALSE | FALSE | **Proven symmetric U-shape.** Extremes always dominate the middle; both flanks consistently non-increasing away from their extremes. |
| FALSE | TRUE | TRUE | FALSE | **Proven non-unimodal; left flank uncertain.** U-shape or left-leaning J-shape — $v_1$ usually dominant but can be overtaken by $v_2$ in some samples. Right flank is clean. |
| FALSE | TRUE | FALSE | TRUE | **Proven non-unimodal; right flank uncertain.** Mirror of above. |
| FALSE | TRUE | TRUE | TRUE | **Proven non-unimodal; both flanks uncertain.** Could be symmetric U-shape, left J, right J, or another polarised form. |

Note: `can_be_unimodal = FALSE` with `can_be_bimodal = TRUE`, `j_shape_low = FALSE`,
and `j_shape_high = FALSE` gives the strongest conclusion — a clean,
symmetric U-shape in every valid sample. The J-shape flags do not weaken
the core non-unimodality finding; they add information about the specific
form of polarisation.

#### Worked example 1: proven non-unimodal with left J-shape ambiguity

*Inputs:* mean = 3.50, sd = 1.75, n = 86, scale 1–5 (k = 5, m = 3).

Count ranges (from `$modality_counts`):

```         
value   count_lo   count_hi
    1         17         27
    2          0         20
    3          0         15
    4          0         21
    5         38         48
```

**`can_be_unimodal`:** Determined by scanning all samples for a
single-peaked frequency vector. Because $v_5$ always has at least 38
counts and $v_3$ at most 15, the frequency vector cannot be unimodal
(any peak at $v_3$ or lower would require $v_5$ to have fewer counts,
which is impossible). No sample passes the unimodal shape check.
`can_be_unimodal = FALSE`.

**`can_be_bimodal`:** Determined by scanning all samples for two strict
local maxima with the sample mean between them. Given that $lo_3 = 0$,
some samples have a near-zero middle count while both extremes are high
— a U-shape with modes at $v_1$ and $v_5$. The sample mean (≈ 3.5) lies
between those modes. `can_be_bimodal = TRUE`.

**`j_shape_low`:** Is $hi_2 = 20 > lo_1 = 17$? Yes.
`j_shape_low = TRUE`. The ranges $[17, 27]$ and $[0, 20]$ overlap over
$[17, 20]$, so both orderings ($c_1 > c_2$ and $c_2 > c_1$) occur in
actual samples.

**`j_shape_high`:** Is $hi_4 = 21 > lo_5 = 38$? No.
`j_shape_high = FALSE`. In every sample,
$c_4(s) \leq 21 < 38 \leq c_5(s)$: $v_5$ always exceeds $v_4$. The right
flank is clean.

Adjacent pair orderings:

```         
value_a   value_b   resolved   a_greater
      1         2      FALSE         —    lo_1=17 ≤ hi_2=20 and lo_2=0 ≤ hi_1=27 → overlap
      2         3      FALSE         —    lo_2=0 and lo_3=0 → fully overlapping
      3         4      FALSE         —    fully overlapping
      4         5       TRUE     FALSE    lo_5=38 > hi_4=21 → v_5 always > v_4
```

**Summary:** Non-unimodality is proven; the right flank is clean ($v_5$
always dominant); the left flank is uncertain ($v_2$ can exceed $v_1$ in
some samples). The distribution is definitely polarised but the exact
form — symmetric U-shape vs. left-leaning J-shape — is not determined.

#### Worked example 2: proven symmetric U-shape

*Inputs:* mean = 3.00, sd = 2.00, n = 60, scale 1–5 (extreme
polarisation toward the endpoints; $k = 5$, $m = 3$).

Hypothetical count ranges arising from a tightly constrained,
highly-symmetric CLOSURE solution set:

```         
value   count_lo   count_hi
    1         22         30
    2          0          5
    3          0          4
    4          0          5
    5         22         30
```

**`can_be_unimodal`:** Determined by scanning all samples for a
single-peaked frequency vector. With $c_1, c_5 \geq 22$ and $c_3 \leq 4$
in every sample, the frequency vector cannot be unimodal — it always
has two high endpoints and a depressed middle, which is a valley, not a
single peak. `can_be_unimodal = FALSE`.

**`can_be_bimodal`:** Every sample has $c_1 \geq 22$ and $c_5 \geq 22$
while $c_3 \leq 4$: two strict local maxima at $v_1$ and $v_5$ with a
valley at $v_3$. The sample mean (= 3.0) lies between the two peak
values (1 and 5). `can_be_bimodal = TRUE`.

**`j_shape_low`:** Is $hi_2 = 5 > lo_1 = 22$? No. `j_shape_low = FALSE`.
The highest count of $v_2$ (5) is far below the minimum of $v_1$ (22).
In every sample $c_1(s) \geq 22 > 5 \geq c_2(s)$.

**`j_shape_high`:** Is $hi_4 = 5 > lo_5 = 22$? No.
`j_shape_high = FALSE`. Symmetric argument: $v_5$ always dominates
$v_4$.

Adjacent pair orderings:

```         
value_a   value_b   resolved   a_greater
      1         2       TRUE      TRUE    lo_1=22 > hi_2=5 → v_1 always > v_2
      2         3      FALSE         —    fully overlapping (both can be 0–5)
      3         4      FALSE         —    fully overlapping
      4         5       TRUE     FALSE    lo_5=22 > hi_4=5 → v_5 always > v_4
```

**Summary:** A clean, proven symmetric U-shape. Both outer pairs are
resolved: the extreme values ($v_1$, $v_5$) always dominate their
adjacent near-extreme values ($v_2$, $v_4$). The inner pairs
($v_2$–$v_3$, $v_3$–$v_4$) are unresolved, but those values are all near
zero in every sample. The J-shape flags confirm the flanks are monotone.

#### The compensation mechanism and why it produces this pattern

The Horns function $H(\mathbf{f})$ is more sensitive to the true-middle
value than to near-extreme values. Formally, for a Horns-neutral shift
that increases $c_2$ by $\delta$, the fixed-count constraint
($\sum_i c_i = n$) plus the requirement $\Delta H \approx 0$ forces:

$$\Delta c_k \;\approx\; -\frac{\partial H / \partial f_2}{\partial H /
\partial f_k} \cdot \delta.$$

Since $|\partial H / \partial f_k|$ (the most extreme value) is large
relative to $|\partial H / \partial f_2|$ (a near-extreme), the required
compensation $\Delta c_k$ is modest — a small increase in $c_k$ absorbs
a large increase in $c_2$, keeping $h$ nearly constant. The J-shape
ordering flip is *cheap in Horns units*, which is why a narrow Horns
range does not preclude J-shape ambiguity.

For the true-middle value $v_m$, the analogous calculation gives a
compensation requirement that cannot be met within the observed Horns
range: raising $c_m$ to reach $lo_1$ would require an $h$ shift of
$\approx 0.24$ (compare `horns(c(50,20,10,20,50),1,5)` ≈ 0.73 against
`horns(c(20,20,40,20,50),1,5)` ≈ 0.49), far exceeding the observed range
of 0.006. The non-unimodal inference is therefore robust, and the
`can_be_unimodal = FALSE` flag correctly captures it.

This is precisely the mechanism behind the asymmetry in example 1: the
right flank is clean (`j_shape_high = FALSE`) because raising $c_4$ high
enough to flip the $v_4$–$v_5$ ordering would require a compensating
increase in $c_5$ that pushes $h$ above the observed range. The left
flank is ambiguous (`j_shape_low = TRUE`) because raising $c_2$ is cheap
— $c_5$ absorbs the change. The same compensation that stabilises $h$
simultaneously liberates the near-extreme values.

#### Implementation

`compute_modality()` in `closure-core` computes count ranges and
adjacent-pair comparisons from the `FrequencyDist`. `can_be_unimodal`
and `can_be_bimodal` are computed in a separate per-sample loop in
`samples_to_result_list()` using the private helpers `is_unimodal()` and
`is_bimodal_mean_between()`, then written into the `ModalityConclusion`
struct. The computation is $O(S \cdot k)$ where $S$ is the number of
samples and $k$ is the scale width (one frequency-vector pass per
sample, early-exit once both flags are `TRUE`). The results are exposed
in R as three flat tibbles:

-   **`$modality_counts`** — one row per scale value; columns `value`,
    `count_lo` ($= lo_i$), `count_hi` ($= hi_i$).
-   **`$modality_pairs`** — one row per adjacent pair; columns
    `value_a`, `value_b`, `resolved`, `a_greater`.
-   **`$modality_conclusion`** — a single-row tibble with four logical columns:
    `can_be_unimodal` and `can_be_bimodal` (exact per-sample shape scan),
    `j_shape_low` ($= hi_2 > lo_1$), `j_shape_high` ($= hi_{k-1} > lo_k$).
    This mirrors the layout of `$metrics_main` and `$metrics_horns`: one
    summary row, column names matching the flag names.

### Naming

The candidate names — saturation, dispersal, diffusion, precision —
capture different aspects:

**Saturation** is the strongest choice for the fill ratio itself. It
denotes the fraction of a defined capacity that is occupied, with no
directional ambiguity and no prior statistical meaning to conflict with.
"Fully saturated" (ratio = 1) and "barely saturated" (ratio near
1/(n+1)) are immediately interpretable. The metaphor is apt: we are
asking how much of the cell grid is filled, just as one asks how much of
a medium is saturated with a solute.

**Dispersal** and **diffusion** both suggest spreading, but neither
specifies *what* is spreading or *over what space*. They are less
precise and carry connotations (ecological spread, random walk) that do
not fit the context well.

For the conceptual inverse (high = count distributions are narrow,
tightly pinned), **precision** is problematic despite the right
intuition: it already means 1/variance in frequentist and Bayesian
statistics, and true-positives rate in machine learning — two
conflicting usages. **Sharpness** carries the right meaning from
probabilistic forecasting but would naturally take notation "s",
conflicting with SD. **Concentration** is mathematically grounded but
generic — it names a whole category of metrics rather than a specific
one. **Determinacy** avoids all these problems: it has no established
statistical meaning, takes notation "d" (following the pattern of h for
the Horns index and s for SD), and maps directly onto the scientific
question — how determined is the original distribution's shape by the
reported statistics? "Count determinacy" is a natural and unambiguous
phrase. High determinacy means the inputs almost uniquely fix the
distribution; low determinacy means many different shapes remain valid.

The recommended pair is **saturation** (fill ratio, high = more
uncertain, notation "sat") and **determinacy** (inverse, high = more
constrained, notation "d"). The connection between them reflects the
core inferential limitation exposed by `frequency_dist`: low determinacy
means many different frequency distributions are compatible with the
reported statistics, so the precise *form* of the original distribution
is uncertain.

Whether modality is also uncertain depends on h. If h is high and
narrow, non-unimodality is reliably established (no valid sample is
bell-shaped), but the specific form — symmetric U versus asymmetric
J-shape — remains uncertain. Only a wide Horns range leaves the
existence of polarisation itself in doubt. Low determinacy therefore
compounds whatever uncertainty h leaves unresolved, rather than
replacing it.

### Relationship to `$frequency`

`f_average` in `$frequency` is the mean of each value's count
distribution:

``` r
f_average[v] == sum(count * n_samples) / n_samples_all
  # for all rows where value == v
```

`$frequency` reports only this mean (plus totals and relatives derived
from it). `frequency_dist` exposes the full distribution. A value with
high saturation across its count range has a wide, uncertain
distribution; `f_average` is then a weak summary. A value whose count
distribution is a near-spike has a `f_average` that accurately
characterises all samples.

This is the core reason `frequency_dist` is needed for the ribbon/band
visualisation: the band width at each scale value should reflect the
spread of the count distribution, not just its mean.

------------------------------------------------------------------------

## Changes in `closure-core` (`frequency-details` branch)

### New public type: `FrequencyDist`

``` rust
pub struct FrequencyDist {
    pub value:     Vec<i32>,
    pub count:     Vec<i32>,
    pub n_samples: Vec<u32>,
}
```

One parallel set of three vectors; row `i` represents the triple
`(value[i], count[i], n_samples[i])`.

### `ResultListFromMeanSdN` extended

``` rust
pub frequency_dist: FrequencyDist,
```

Added alongside the existing `frequency`, `metrics_main`,
`metrics_horns`, and `results` fields.

### Core computation: `calculate_frequency_dist()`

Uses a flat 2-D `Vec<u32>` instead of a `HashMap<(i32,i32), u32>` to
avoid per-insert hashing overhead:

``` rust
let mut dist = vec![0u32; n_scale_vals * (n + 1)];
// Indexing: dist[v_idx * (n + 1) + count]
```

After iterating all samples, the non-zero cells are collected into the
`FrequencyDist` vectors.

### In-memory path

`samples_to_result_list()` calls `calculate_frequency_dist()` on the
completed sample list and attaches the result to
`ResultListFromMeanSdN`.

### Streaming path (`StreamingFrequencyState`)

Four fields added to the streaming accumulator:

``` rust
freq_dist:            Vec<u32>,
freq_dist_n_scale_vals: usize,
freq_dist_n:          usize,
freq_dist_scale_min:  i32,
```

The accumulator is updated inside the hot loop using the same
stride-based indexing (`stride = freq_dist_n + 1`). After all batches
are processed, `write_frequency_dist_to_parquet()` writes
`frequency_dist.parquet` alongside the other output files.

### Parquet output

`write_frequency_dist_to_parquet()` writes a flat Parquet file with
schema:

```         
value     INT32
count     INT32
n_samples INT32  (cast from u32)
```

Both the in-memory write path and `write_streaming_statistics` call this
helper, so `frequency_dist.parquet` is always produced when writing to
disk.

------------------------------------------------------------------------

## Changes in `unsum` (R package)

### `src/rust/Cargo.toml`

Switched `closure-core` to the `frequency-details` branch:

``` toml
[dependencies]
closure-core = { git = "https://github.com/lhdjung/closure-core.git", branch = "frequency-details" }
```

### `src/rust/src/lib.rs`

**Import added:**

``` rust
use closure_core::{..., FrequencyDist};
```

**New conversion helper:**

``` rust
fn frequency_dist_to_robj(freq_dist: &FrequencyDist) -> Robj {
    let n_samples_i32: Vec<i32> =
        freq_dist.n_samples.iter().map(|&x| x as i32).collect();
    let df = data_frame!(
        value     = freq_dist.value.clone(),
        count     = freq_dist.count.clone(),
        n_samples = n_samples_i32
    );
    df.into()
}
```

The `u32 → i32` cast is necessary because extendr does not natively map
Rust `u32` to an R integer vector. The maximum value of `n_samples` for
any single (value, count) cell is bounded by the total number of CLOSURE
samples, which fits comfortably in `i32` for all realistic inputs.

**`result_list_to_pairs()` extended:**

``` rust
let frequency_dist = frequency_dist_to_robj(&rl.frequency_dist);

vec![
    ("metrics_main",   metrics_main),
    ("metrics_horns",  metrics_horns),
    ("frequency",      frequency),
    ("frequency_dist", frequency_dist),   // new
    ("results",        results),
]
```

This makes `$frequency_dist` available on every object returned by
`closure_generate()` (in-memory mode) as a plain `data.frame` with
columns `value`, `count`, `n_samples`.

------------------------------------------------------------------------

## New test file: `tests/testthat/test-frequency-dist.R`

### R reference implementation

``` r
compute_frequency_dist_r <- function(data) {
  scale_vals <- seq(data$inputs$scale_min, data$inputs$scale_max)
  samples    <- data$results$sample

  rows <- lapply(scale_vals, function(v) {
    counts    <- vapply(samples, function(s) sum(s == v), integer(1))
    count_tbl <- table(counts)
    data.frame(
      value     = v,
      count     = as.integer(names(count_tbl)),
      n_samples = as.integer(count_tbl),
      stringsAsFactors = FALSE
    )
  })

  do.call(rbind, rows)
}
```

For each scale value `v`, this counts how many times `v` appears in each
sample, then tabulates those counts. Rows with `n_samples == 0` are
never created (they simply don't appear in the `table()` output), which
matches the Rust behaviour.

### Tests

Two fixture datasets are used: one with `n = 12, scale 1–5` and one with
`n = 20, scale 1–7`.

**Structural tests (R-computed):**

| Test | Assertion |
|----|----|
| Coverage | Every scale value appears in `fd_r$value` |
| Bounds | No value outside the scale; counts in `[0, n]` |
| Positivity | All `n_samples > 0` |
| Partition | For each value, `sum(n_samples) == total_samples` |
| Consistency | `sum(count * n_samples)` per value matches `f_absolute` in `$frequency` |

The last test provides cross-validation against the existing
`$frequency` table, which is independently computed by CLOSURE's own
aggregation path.

**Comparison tests (R vs. Rust):**

| Test | Assertion |
|----|----|
| Presence | `$frequency_dist` exists and has columns `value`, `count`, `n_samples` |
| (value, count) pairs | Identical after sorting by (value, count) |
| `n_samples` values | Identical after sorting |

Both comparison tests are run for both fixture datasets.

------------------------------------------------------------------------

## Rebuild instructions

After these changes, the package must be recompiled before the tests can
run:

``` r
rextendr::document()   # regenerates R wrappers and compiles Rust
devtools::load_all()   # loads the rebuilt package
devtools::test(filter = "frequency-dist")  # runs the new tests
```

Or for a full install:

``` r
devtools::install()
```
