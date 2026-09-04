## R CMD check results

0 errors | 0 warnings | 1 note

The note lists three URLs as "possibly invalid". However, all three
are reachable in a browser.

The 'libs' sub-directory is roughly 8.6Mb because of compiled Rust code.


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
