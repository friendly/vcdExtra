## Test environments
* local Windows 7 install, R version 4.1.2 
* win-builder R Under development (unstable) (2022-02-11 r81718 ucrt)
* Rhub Ubuntu Linux, 16.04 LTS, R-release, GCC
* Rhub Fedora Linux, R-devel, clang, gfortran

## R CMD check results
There are no ERRORs or WARNINGs or NOTEs 

## Version 0.7-6

This is a minor release, fixing a problem related to the CMHtest function

# reverse dependencies

> revdep()
[1] "aplore3" "catdata" "gnm"     "iarm"    "jmv" 
> revdepcheck::revdep_check(num_workers = 4)

*Wow, no problems at all. :)*