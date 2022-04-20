## Test environments
* local Windows 7 install, R version 4.1.2 
* win-builder 4.2.0 RC (2022-04-19 r82220 ucrt)
* Rhub Ubuntu Linux 20.04.1 LTS, R-release, GCC
* Rhub Fedora Linux, R-devel, clang, gfortran

## R CMD check results
There are no ERRORs or WARNINGs or NOTEs 

## Version 0.8-0

This is a modest release, replacing the old Sweave .Rnw vignette with
new .Rmd vignettes using knitr

# reverse dependencies

> revdep()
[1] "aplore3" "catdata" "gnm"     "iarm"    "jmv" 
> revdepcheck::revdep_check(num_workers = 4)

*Wow, no problems at all. :)*