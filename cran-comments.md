## Test environments
* local Windows 7 install, R 3.1.2
* win-builder (3.2.3 Patched (2016-02-04 r70085), R-development (unstable) (2016-02-23 r70215))

## R CMD check results
There are no errors or notes. 

R-devel gives the following warning, that I am unable to cure.  It occurred only after I 
modified the vignette and re-built the vignette to check it.  However, I deleted the 
file `inst/doc/vcd-tutorial.pdf` from the package source, and also added
`inst/doc/*.pdf` to .Rbuildignore, so I am mystified as to why this warning occurs.


* checking sizes of PDF files under 'inst/doc' ... WARNING
  'gs+qpdf' made some significant size reductions:
     compacted 'vcd-tutorial.pdf' from 779Kb to 528Kb


## Comments
This is a minor release, but the major package version has been bumped to coincide with my new
book "Discrete Data Analysis with R"  (DDAR) where it serves as the support package.

As well, I am starting on an intensive short course next week using this package and
it would be very useful to have the new version available on CRAN.

o Added Glass data
o introduce links and references to DDAR
o added mcaplot() to plot MCA solutions in DDAR style
o added update.xtabs method
o updated vignette to refer to DDAR


