# Agent notes for unsum

## Updating closure-core

unsum gets closure-core from its GitHub `optimize` branch, pinned in
`src/rust/Cargo.lock`. A bump is not picked up by a plain
[`devtools::load_all()`](https://devtools.r-lib.org/reference/load_all.html)
or [`devtools::test()`](https://devtools.r-lib.org/reference/test.html);
they keep linking the old Rust library and give no warning. Three things
cause this:

- `load_all()` skips compiling when no file in `src/` changed.
- The build unpacks the git-ignored `src/rust/vendor.tar.xz` and
  compiles offline from it.
- Cargo identifies that vendored crate by name and version (always
  1.0.0), not by content, so it reuses the old build in
  `src/rust/target/debug`.

After pushing closure-core, from the package root:

``` sh
# 1. Move the pin to the tip of closure-core's optimize branch
(cd src/rust && cargo update -p closure-core)

# 2. Re-vendor the crates from the new Cargo.lock
Rscript -e 'rextendr::vendor_pkgs()'

# 3. Drop the stale build (rebuilding takes about 3 minutes) and force a compile
rm -rf src/rust/target/debug
touch src/entrypoint.c

# 4. Rebuild and test
Rscript -e 'devtools::test()'

# 5. devtools rewrites src/Makevars as a debug build; restore it
git checkout -- src/Makevars
```

Confirm that the new code really runs by checking a behaviour that
changed in closure-core, or that `src/rust/target/debug/libunsum.a` has
a fresh timestamp. Then commit `src/rust/Cargo.lock`, with a NEWS entry
for anything users will notice. closure-core’s `UNSUM-MIGRATION.md`
lists what each commit changes for unsum.
