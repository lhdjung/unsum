# unsum package — Claude memory

## Package summary
R package implementing CLOSURE and SPRITE algorithms for reconstructing possible
sample distributions from summary statistics. Uses Rust (extendr) for the core
algorithm. Results are now S7 objects (UnsumResult hierarchy).

## Key architecture
- `generate-basic.R`: single `generate_from_mean_sd_n()` function — the real
  implementation; all exported generators delegate here
- `fn-factory.R`: `new_generator_mean_sd_n()`, `new_writer_fn()`, `new_reader_fn()`,
  `new_plot_fn_bar()` build all public functions programmatically via
  `rlang::new_function()`
- `fn-formals.R`: standalone DSL for manipulating pairlists (formals_add,
  formals_remove, formals_add_defaults, …) — used by fn-factory.R
- `utils.R`: `check_generator_output()` (simplified) now just checks
  `S7::S7_inherits(data, UnsumResult)` + empty-results guard; `check_component_tibble()`
  kept for use by horns-analyze.R
- `classes.R` (new): full S7 class hierarchy; loaded after utils.R in Collate
- `doc-helpers.R`: `expand_section()` generates roxygen doc sections at build
  time via inline R code (`r expand_section("writing", "CLOSURE")`)
- `constants.R`: TIBBLE_NAMES, FILES_EXPECTED (TIBBLE_NAMES_POSSIBLE_FORMS removed)

## S7 class hierarchy (implemented)
Abstract bases: UnsumResult → ClosureResult, SpriteResult
Concrete CLOSURE:
  - ClosureResultFull (in-memory, from closure_generate())
  - ClosureResultFromDisk (abstract base for disk reads)
    - ClosureResultStatsOnly (include = "stats_only")
    - ClosureResultStatsAndHorns (include = "stats_and_horns")
    - ClosureResultAll (include = "all" / "capped_error")
Parallel SPRITE hierarchy: SpriteResultFull, SpriteResultFromDisk (abstract),
  SpriteResultStatsOnly, SpriteResultStatsAndHorns, SpriteResultAll

## S7 property access
- Use `data@property` (canonical); `data$property` does NOT work — S7 rejects it with an error
- `data[[name]]` and `names(data)` do NOT work on S7 objects either
- `S7::S7_inherits(data, ClassName)` for type checks (not inherits())
- `S7::prop(data, name)` for programmatic access (used in write_basic loop)
- Validators return NULL (valid) or character string (error) — cannot abort()

## Migration changes (S7 migration completed)
Files changed: classes.R (new), DESCRIPTION (S7 added), constants.R (removed
TIBBLE_NAMES_POSSIBLE_FORMS), generate-basic.R, read-write-basic.R, utils.R,
plot-ecdf-basic.R, plot-horns-histogram-basic.R

Key patterns replaced:
- inherits(data$inputs, "closure_generate") → S7::S7_inherits(data, UnsumResult)
- has_reading_class(data$inputs) → S7::S7_inherits(data, ClosureResultFromDisk)
- add_class() calls on data$inputs → removed (class on object itself now)
- data$results["horns"] → data@results["horns"] in write_basic
- names(data) loop → explicit S7::prop() loop in write_basic

## Pre-existing issues (NOT introduced by S7 migration)
- horns-analyze.R: calls undefined check_closure_generate(), accesses
  data$metrics (not data$metrics_main), incompatible check_component_tibble args
  → file is NOT in Collate, so it doesn't affect the package build

## Build notes
- Branch: vendor-debug (for offline/vendored Rust builds)
- Uses extendr (Rust integration), nanoparquet for Parquet I/O
- tools/config.R controls debug vs release Rust build; use_debug()/use_release()
  helpers in utils.R switch modes
- S7 version: 0.2.0 (supports abstract = TRUE in new_class())
