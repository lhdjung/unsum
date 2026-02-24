# Cleanup script to remove build helpers that are only needed when the package
# is built, not when it runs on the user's machine. Removing these objects
# prevents them from inflating the binary, i.e., the final package file.

# Use .Rbuildignore instead for files like doc-helpers.R that are needed between
# sourcing the R/ files and actually building the package!

# Argument modifiers: change the list of a function's arguments
rm(list = ls(pattern = "^formals_"))

last <- NULL

rm(
  # Function factories: build parameterized wrappers around more basic functions
  new_generator_mean_sd_n,
  new_plot_fn_bar,
  new_plot_fn_horns_frequency,

  # Build mode setters
  use_debug,
  use_release,
  switch_build_mode,

  # Placeholder for possible future extensions
  last
)
