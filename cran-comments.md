## Test environments
* local Windows 10, R version 4.5.2 (2025-10-31 ucrt)
* win-builder R Under development (unstable) (2026-06-06 r90114 ucrt)
* R-hub: linux (R-devel), windows (R-devel), macos-arm64 (R-devel)

## R CMD check results
There are no ERRORs or WARNINGs or NOTEs

The use of `rgl` triggered harmless warnings on headless rhub machines. This was resolved
by moving `rgl` from "Depends:" to "Suggests:" because it was only used in one function
and was suitably trapped there. See below.

### Previous rhub WARNING (resolved)
An earlier rhub run produced:
  `checking whether package 'vcdExtra' can be installed ... WARNING`
  `Warning: 'rgl.init' failed, will use the null device.`

This was caused by `rgl` being listed in `Imports`, which forced it to load
(and attempt to initialise an OpenGL display) on headless CI runners. Fixed
by moving `rgl` to `Suggests`, since `mosaic3d()` is the only function that
uses it and already guards its use with `requireNamespace("rgl")`.

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
 
> revdepcheck::revdep_check(num_workers = 4)

We checked 9 reverse dependencies, comparing R CMD check results across CRAN and dev versions of this package.

 * We saw 0 new problems
 * We failed to check 0 packages

