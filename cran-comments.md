## Test environments
* local Windows 10, 4.2.3 (2023-03-15 ucrt)
* win-builder R Under development (unstable) (2023-08-19 r84989 ucrt)
* R Under development (unstable) (2023-03-03 r83933 ucrt)
* Rhub Windows Server 2022, R-devel, 64 bit

## R CMD check results
There are no ERRORs or WARNINGs or NOTEs 

## Version 0.8-5 (2023-08-19)
Very minor update:

- Fix CRAN nit re vcdExtra-package.Rd
- Fix moved URL - http: -> https:
- Fix xrefs in vignettes
- More spellcheck

# reverse dependencies

> devtools::revdep()
[1] "aplore3"     "catdata"     "genridge"    "gnm"         "iarm"        "jmv"         "reappraised"
> revdepcheck::revdep_check(num_workers = 4)

## revdepcheck results

We checked 7 reverse dependencies, comparing R CMD check results across CRAN and dev versions of this package.

 * We saw 0 new problems
 * We failed to check 0 packages

