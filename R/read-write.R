#' @include utils.R
NULL


# CLOSURE -----------------------------------------------------------------

#' Write CLOSURE results to disk (and read them back in)
#'
#' @description `r expand_section("read_write_description", "CLOSURE")`
#'
#' @param data List returned by `closure_generate()`.
#' @param path `r expand_section("read_write_param_path", "CLOSURE")`
#' @param include `r expand_section("read_write_param_include", "CLOSURE")`
#' @param samples_cap Numeric (length 1). When using `include = "capped_error"`,
#'   enter a whole number here to specify a cap. Default is `NULL`.
#'
#' @section `r expand_section("read_write_folder_name", "CLOSURE")`
#' @details `r expand_section("read_write_details", "CLOSURE")`
#' @return `r expand_section("read_write_return", "CLOSURE")`
#'
#' @export
#'
#' @examples
#' data <- closure_generate(
#'   mean = "2.7",
#'   sd = "0.6",
#'   n = 45,
#'   scale_min = 1,
#'   scale_max = 5
#' )
#'
#' # Writing to a temporary folder just for this example.
#' # You should write to a real folder instead.
#' # A simple way is path = "." for your current directory.
#' path_new_folder <- closure_write(data, path = tempdir())
#'
#' # In a later session, conveniently read the files
#' # back into R. This returns the original list,
#' # identical except for floating-point error.
#' # (Of course, the `path_new_folder` variable will
#' # no longer be available -- instead, paste the path
#' # to your folder here.)
#' closure_read(path_new_folder)
closure_write <- new_writer_fn("CLOSURE")

#' @rdname closure_write
#' @export
closure_read <- new_reader_fn("CLOSURE")


# SPRITE ------------------------------------------------------------------

#' Write SPRITE results to disk (and read them back in)
#'
#' @description `r expand_section("read_write_description", "SPRITE")`
#'
#' @param data List returned by `sprite_generate()`.
#' @param path `r expand_section("read_write_param_path", "SPRITE")`
#' @param include `r expand_section("read_write_param_include", "SPRITE")`
#' @inheritParams closure_write
#'
#' @section `r expand_section("read_write_folder_name", "SPRITE")`
#' @details `r expand_section("read_write_details", "SPRITE")`
#' @return `r expand_section("read_write_return", "SPRITE")`
#'
#' @export
#'
#' @examples
#' data <- sprite_generate(
#'   mean = "2.7",
#'   sd = "0.6",
#'   n = 45,
#'   scale_min = 1,
#'   scale_max = 5,
#'   stop_after = 1000
#' )
#'
#' # Writing to a temporary folder just for this example.
#' # You should write to a real folder instead.
#' # A simple way is path = "." for your current directory.
#' path_new_folder <- sprite_write(data, path = tempdir())
#'
#' # In a later session, conveniently read the files
#' # back into R. This returns the original list,
#' # identical except for floating-point error.
#' # (Of course, the `path_new_folder` variable will
#' # no longer be available -- instead, paste the path
#' # to your folder here.)
#' sprite_read(path_new_folder)
sprite_write <- new_writer_fn("SPRITE")

#' @rdname sprite_write
#' @export
sprite_read <- new_reader_fn("SPRITE")
