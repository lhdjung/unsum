# unsum package — Claude memory

## Package summary
R package implementing CLOSURE and SPRITE algorithms for reconstructing possible
sample distributions from summary statistics. Uses Rust (extendr) for the core
algorithm. Results are returned as named lists of tibbles.

## Key architecture
- `generate-basic.R`: single `generate_from_mean_sd_n()` function — the real
  implementation; all exported generators delegate here
- `fn-factory.R`: `new_generator_mean_sd_n()`, `new_writer_fn()`, `new_reader_fn()`,
  `new_plot_fn_bar()` build all public functions programmatically via
  `rlang::new_function()`
- `fn-formals.R`: standalone DSL for manipulating pairlists (formals_add,
  formals_remove, formals_add_defaults, …) — used by fn-factory.R
- `utils.R`: `check_generator_output()` (~230 lines) validates the output list;
  also contains all small check helpers and utility fns
- `doc-helpers.R`: `expand_section()` generates roxygen doc sections at build
  time via inline R code (`r expand_section("writing", "CLOSURE")`)
- `constants.R`: TIBBLE_NAMES, TIBBLE_NAMES_POSSIBLE_FORMS, FILES_EXPECTED

## Output structure of generator functions
Named list of tibbles: inputs, metrics_main, metrics_horns, frequency, results.
Optionally "directory" if data came from disk. Three valid structural forms
tracked in TIBBLE_NAMES_POSSIBLE_FORMS.

## Known design issue: S3 class on wrong object
The S3 class (e.g. "closure_generate") is attached to data$inputs, NOT to data
itself. So inherits(data, "closure_generate") is FALSE. Every check site does
inherits(data$inputs, "closure_generate"). This was flagged in feedback doc.

## Reading-class pattern
Classes like "closure_read_include_stats_only" are stored as strings in
data$inputs class vector and decoded with regex. Fragile; flagged in feedback.

## Feedback doc
Comprehensive architectural feedback written to:
  special-scripts/claude-feedback-on-unsum.Rmd
Main conclusions:
- Don't use S7 yet; fix S3 class placement first (add class to list, not inputs)
- Replace reading-class regex with proper S3 subclasses
- Consider S7 when adding more techniques or wanting validated properties

## Build notes
- Branch: vendor-debug (for offline/vendored Rust builds)
- Uses extendr (Rust integration), nanoparquet for Parquet I/O
- tools/config.R controls debug vs release Rust build; use_debug()/use_release()
  helpers in utils.R switch modes
