# Cleanup script to remove build helpers that are only needed when the package
# is built, not when it runs on the user's machine. Removing these objects
# prevents them from inflating the binary, i.e., the final package file.
rm(
  # Function factories: build parameterized wrappers around more basic functions
  new_generator_mean_sd_n,
  new_plot_fn_bar,
  new_plot_fn_horns_frequency,
  # Argument modifiers: change the list of a function's formal arguments
  formals_add,
  formals_remove,
  formals_change_defaults,
  # This is a list, but all other objects are functions
  formals_final
)
