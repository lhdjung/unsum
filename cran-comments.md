## R CMD check results

0 errors | 0 warnings | 1 note

The note lists three URLs as "possibly invalid". All three are reachable in a
browser; the hosts simply answer automated requests with HTTP 403
(crates.io, jamesheathers.medium.com, peerj.com).

On some platforms there is an additional note about the installed package
size: the 'libs' sub-directory is roughly 8.6Mb because the package contains
compiled Rust code.


## Fix for the current CRAN check problem

This release fixes the note shown on the CRAN check page for unsum 0.2.0:

    Found non-API call to R: 'R_NamespaceRegistry'

The call did not come from this package's own code. It came from the
'extendr-api' Rust crate, which unsum uses for its R/Rust interface. extendr
removed the 'R_NamespaceRegistry' binding in version 0.9.0, so unsum now
depends on that version. I confirmed that the symbol is no longer present in
the compiled shared object, and 'checking compiled code' is now OK.


## Please also note

* There are currently no references describing the methods in the package.
  (I will add a reference once there is a manuscript.)

* CRAN checks previously flagged file writing operations in tools/config.R,
  which is a script to create Makevars files. The config.R script is used by
  many 'Rust'-based packages. I believe this to be a false positive.
