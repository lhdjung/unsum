# unsum (development version)

* Added `closure_horns_min_max_bar()`.
* Fixed bugs that caused `closure_horns_analyze()` and `closure_plot_ecdf()` to return clearly wrong results if the scale did not start at 1.

# unsum 0.2.0

* Initial CRAN submission.
* Added `closure_horns_analyze()` and `closure_horns_histogram()`.
* Removed vignette on installing Rust since users will not need it when the package is on CRAN.
+ Fixed examples that causes CRAN check issues.
