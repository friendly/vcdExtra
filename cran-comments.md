## Test environments
* local Windows 10, R version 4.2.2 (2022-10-31 ucrt)
* win-builder R version 4.2.2 (2022-10-31 ucrt)
* Rhub Ubuntu Linux 20.04.1 LTS, R-release, GCC
* Rhub Fedora Linux, R-devel, clang, gfortran

## R CMD check results
There are no ERRORs or WARNINGs or NOTEs 

## Version 0.8-3 (2023-01-19)

This is a modest enhancement release of the `vcdExtra` package, focusing on documentation and examples.

- All datasets now Classified datasets with concept/method tags. These can be found using `help.search(pattern, field="concept")`
- Added a vignette, `datasets.Rmd` showing all datasets classified by method tags. Links only work in the `pkgdown` site.
- Added `Asbestos` data

# reverse dependencies

> revdep()
[1] "aplore3" "catdata" "gnm"     "iarm"    "jmv" 
> revdepcheck::revdep_check(num_workers = 4)

*Wow, no problems at all. :)*

## revdepcheck results

We checked 5 reverse dependencies, comparing R CMD check results across CRAN and dev versions of this package.

 * We saw 0 new problems
 * We failed to check 0 packages

