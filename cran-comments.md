## Resubmission
This is a resubmission. In this version I have:

* Used single quotes in the description field.

* Removed the default path in closure_write().

* Used tempdir() directly to write results to disk in the examples of R/read-write.R, and in vignettes/unsum.Rmd (which was not flagged but contained similar code).

* Added some new features.

Please also note:

* CRAN checks previously flagged file writing operations in tools/config.R, which is a script to create Makevars files. The config.R script is used by many 'Rust'-based packages. I believe this to be a false positive.

* There are currently no references describing the methods in the package. Therefore, I believe that the corresponding check might not be applicable. (I will add a reference once there is a manuscript.)

* The package contains Rust code, which is why the 'libs' sub-directory is 5.7Mb in size.


## R CMD check results

0 errors | 0 warnings | 1 note

* This is a new release.
