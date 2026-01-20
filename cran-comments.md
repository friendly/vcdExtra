## Test environments
* local Windows 10, R version 4.5.2 (2025-10-31 ucrt) 
* win-builder R Under development (unstable) (2026-01-18 r89306 ucrt)


## R CMD check results
There are no ERRORs or WARNINGs or NOTEs 

## Version 0.9.0 (2026-01-17)

This is a major release of the package, adding structure to dataset examples and introducing an new extension Woolf's test for homogeneity of odds ratios.

* Begin to categorize the datasets in the package with `@concept` tags
* Begin to flesh out more general conception of CDA
* `datasets()`: added `ndim` arg; package="vcdExtra" default
* Added a tidyCat hex logo
* Generalized `vcd::woolf_test()` to handle strata better, with special handling for 2 x 2 x R x C case. 
* Completed extension of `vcd::woolf_test()` to fully handle R x C strata, with tests for rows/cols/residuals
* Added GH actions R-CMD-check & pkgdown
* Now depends R (>= 4.1.0) due to use of native pipe `|>`
* Fixed some broken URLs in `R/data.R`

# reverse dependencies

> devtools::revdep()
 [1] "aplore3"            "catdata"            "genridge"           "gnm"                "heplots"           
 [6] "iarm"               "jmv"                "junco"              "public.ctn0094data" "reappraised" 

> revdepcheck::revdep_check(num_workers = 4)

## revdepcheck results

We checked 10 reverse dependencies, comparing R CMD check results across CRAN and dev versions of this package.

 * We saw 0 new problems
 * We failed to check 0 packages


