## Test environments
* local Windows 10, R version 4.2.2 (2022-10-31 ucrt)
* win-builder R Under development (unstable) (2023-01-27 r83711 ucrt)
* Rhub Ubuntu Linux 20.04.1 LTS, R-release, GCC
* Rhub Fedora Linux, R-devel, clang, gfortran

## R CMD check results
There are no ERRORs or WARNINGs or NOTEs 

## Version 0.8-2 (2023-01-19)

This is a major enhancement release of the `vcdExtra` package, focusing on documentation and examples.

- added `HouseTasks` data set, illustrating permutation of row / col variables
- package now depends on R (>= 3.5.0) per CRAN nit
- add `Suggests: seriation` to illustrate CA re-ordering of rows/cols based on correspondence analysis
- all .Rd files reformatted and many examples extended.
- the vignette `mosaic.Rmd` on mosaic displays has been extensively revised with examples for square tables and permutation of row / column variables.
- begin to classify datasets with `\concept{}` tags
- Added a new `demo-housing.Rmd` vignette, using content from `demo/housing.R`.

# reverse dependencies

> revdep()
[1] "aplore3" "catdata" "gnm"     "iarm"    "jmv" 
> revdepcheck::revdep_check(num_workers = 4)

*Wow, no problems at all. :)*