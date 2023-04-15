## Test environments
* local Windows 10, R version 4.2.2 (2022-10-31 ucrt)
* win-builder R 4.3.0 beta (2023-04-12 r84240 ucrt)
* R Under development (unstable) (2023-03-03 r83933 ucrt)
* Rhub Ubuntu Linux 20.04.1 LTS, R-release, GCC
* Rhub Fedora Linux, R-devel, clang, gfortran

## R CMD check results
There are no ERRORs or WARNINGs or NOTEs 

## Version 0.8-4 (2023-04-12)

This is first a maintenance release to correct an error users encounter to load
the package.  It also extends/expands on vignettes

- Imports: tidyr must depend on (>= 1.3.0) for one use in a vignette.
- Enhanced the vignette, "Creating and manipulating frequency tables", `vignettes/creating.Rmd`
- Created a new vignete, "Mobility tables"

# reverse dependencies

> devtools::revdep()
[1] "aplore3"     "catdata"     "gnm"         "iarm"        "jmv"         "reappraised"
> revdepcheck::revdep_check(num_workers = 4)

*Wow, no problems at all. :)*

## revdepcheck results

We checked 6 reverse dependencies, comparing R CMD check results across CRAN and dev versions of this package.

 * We saw 0 new problems
 * We failed to check 0 packages

