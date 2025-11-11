# Cleanup script to remove build helpers that are only needed when the package
# is built, not when it runs on the user's machine. Removing these objects
# prevents them from inflating the binary, i.e., the final package file.
rm(
  new_generator_mean_sd_n,
  new_plotter_bar,
  formals_add,
  formals_remove,
  formals_change_defaults,
  # This is a list, but all other objects are functions
  formals_final_all
)
