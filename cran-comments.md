## Test environments
* local Windows 10, R version 4.5.1 (2025-06-13 ucrt)
* win-builder R Under development (unstable) (2025-07-22 r88445 ucrt)


## R CMD check results
There are no ERRORs or WARNINGs or NOTEs 

## Version 0.8.7 (2025-11-18)

This is a major release of the package, fixing bugs and revising documentation

o Added tests for CMHtest() PR #13 [Thx: Daniel Sabanes Bove] 
o Automatically omit strata with a single observation in CMHtest() because they do not contribute to the test statistics
o Use the generalized Moore-Penrose inverse from MASS in CMHtest() such that it can work when the variance
  matrix is singular.
o Converted the package to use roxygen documentation.
o Added `CrabSatellites` data from {countreg} b/c that's still not available on CRAN
o Moved `grid` from `Depends:` to `Imports:`
o Now use markdown in package documentation for easier maintenance, via {roxygen2md}


# reverse dependencies

> devtools::revdep()
[1] "aplore3"            "catdata"            "genridge"           "gnm"               
[5] "heplots"            "iarm"               "jmv"                "public.ctn0094data"
[9] "reappraised" 

> revdepcheck::revdep_check(num_workers = 4)

## revdepcheck results

We checked 9 reverse dependencies, comparing R CMD check results across CRAN and dev versions of this package.

 * We saw 0 new problems
 * We failed to check 0 packages


