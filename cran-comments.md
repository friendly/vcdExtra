## Test environments
* local Windows 7 install, R version 3.6.3 (2020-02-29)
* win-builder R Under development (unstable) (2021-01-21 r79854)
* Rhub Ubuntu Linux, 16.04 LTS, R-release, GCC
* Rhub Fedora Linux, R-devel, clang, gfortran

## R CMD check results
There are no ERRORs or WARNINGs or NOTEs 

## Version 0.7-5

This is a minor release, fixing a problem related to rgl in examples

# reverse dependencies

> revdep()
[1] "aplore3" "catdata" "gnm"     "iarm"    "jmv" 
> revdepcheck::revdep_check(num_workers = 4)

*Wow, no problems at all. :)*