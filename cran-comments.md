## Test environments
* local Windows 10, R version 4.5.2 (2025-10-31 ucrt)
* win-builder R Under development (unstable) (2026-06-06 r90114 ucrt)
* R-hub: linux (R-devel), windows (R-devel), macos-arm64 (R-devel)

## R CMD check results
There are no ERRORs or WARNINGs or NOTEs

The use of `rgl` triggered harmless warnings on headless rhub machines. This was resolved
by moving `rgl` from "Depends:" to "Suggests:" because it was only used in one function
and was suitably trapped there. 


## Version 0.9.6

This is a cumulative release of a number of small enhancements to the package since the last CRAN version

* Fix @aliases for roxygen 8.0.0
* Make some `color_table()` examples visible in documentation

## Version 0.9.5

This is a major release of the package, completing work on a sizable collection of tidy tools
for manipulating categorical data in various forms

* Added a general `collapse_levels()` function that can collapse levels of variables belonging to data sets of any form. [GK]
* Added as_matrix() to the set of as_*() conversion functions. [GK]
* Added `prop` arguments to applicable as_*() conversion functions to easily convert counts to proportions (either relative 
  to the grand total count or to specified margins). [GK]
* Added vignette on tidy operations (convert, collapsing) [GK]
* Gavin Klorfine (@gklorfine) becomes a package author


## Version 0.9.4

* suppressWarnings() from `ca:mcja()` in `mcaplot()` examples


# reverse dependencies

> devtools::revdep()
 [1] "aplore3"            "CASIdata"           "catdata"            "genridge"          
 [5] "gnm"                "heplots"            "iarm"               "jmv"               
 [9] "junco"              "public.ctn0094data"
 
* `revdepcheck::revdep_check()` could not be run due to an incompatibility between
`revdepcheck` 1.0.0.9002 and `gmailr` 3.0.0 (`mime` is no longer exported by
`gmailr`). The 10 reverse dependencies were checked manually using

* `devtools::check_reverse_dependencies()` / `rcmdcheck::rcmdcheck()`, and no new
problems were introduced by this release.

