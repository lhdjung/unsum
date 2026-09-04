## R CMD check results

0 errors | 0 warnings | 2 notes

Two URLs are flagged as "possibly invalid". Both return 403 to automated
requests but are reachable in a browser.

One example runs slightly over 5 seconds.

The 'libs' sub-directory is roughly 8.6Mb because of compiled Rust code.


## Resubmission

Fixes both problems from the 0.3.0 pretest:

* Installation ERROR: the vendored 'Rust' sources were archived with macOS
  extended attributes, which extracted as spurious '._*' C files elsewhere.
  The archive is now free of them, and offline installation was verified.

* The 'crates.io' URL is valid but returns 404 to non-browser clients.
  It now points to the crate's source repository instead.


## Fix for current CRAN check problem

This release fixes the following note for unsum 0.2.0:

    Found non-API call to R: 'R_NamespaceRegistry'

It came from the 'extendr-api' Rust crate. The 'R_NamespaceRegistry'
binding was removed in extendr 0.9.0, which unsum now uses.
The symbol is no longer present in the compiled shared object.


## Please also note

* There are currently no references describing the methods in the package.
  (I will add a reference once there is a manuscript.)

* CRAN checks previously flagged file writing operations in tools/config.R,
  which is a script to create Makevars files. The config.R script is used by
  many 'Rust'-based packages. I believe this to be a false positive.
