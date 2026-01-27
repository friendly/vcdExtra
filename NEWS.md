## Version 0.9.1 (2026-01-23)

* Added `labeling_points()` for mosaic displays to show observed or expected frequencies as random points in the tiles
* Changed defaults for `clip` and `margin` in `labeling_points()` to be more sensible [Thx: Achim Zeileis]
* Added `color_table()` to display frequency tables with color shaded backgrounds to show patterns
* Refactored `color_table()` as S3 methods, adding support for frequency data frames. Added file output, other formatting options
* Print message for `color_table()` giving the model fit statistics.
* Now provide `color_table(values =)` to display either the frequencies (default) or residuals in the table.
* Add `legend = "note"` to include a table note regarding what is shown

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

## Version 0.8.7 (2025-12-10)

This is a major release of the package, fixing bugs and revising documentation

o Added tests for CMHtest() PR #13 [Thx: Daniel Sabanes Bove] 
o Automatically omit strata with a single observation in CMHtest() because they do not contribute to the test statistics
o Use the generalized Moore-Penrose inverse from MASS in CMHtest() such that it can work when the variance
  matrix is singular.
o Converted the package to use roxygen documentation via {rd2roxygen}. UGH!
o Added `CrabSatellites` data from {countreg} b/c that's still not available on CRAN
o Moved `grid` from `Depends:` to `Imports:`
o Now use markdown in package documentation for easier maintenance, via {roxygen2md}

## Version 0.8-6 (2025-07-23)

This is a minor release to satisfy the CRAN gods who like all crossref links to be correct.

o Fixed CRAN nits re crossrefs
o Update issues link
o Update README
o Fixed Winbuilder notes re: Rd links to data sources


## Version 0.8-5 (2023-08-19)

- Fix CRAN nit re vcdExtra-package.Rd
- Fix moved URL - http: -> https:
- Fix xrefs in vignettes
- More spellcheck
- Fixed one more NOTE re: AirCrash.Rd for a URL

## Version 0.8-4 (2023-04-12)

- Imports: tidyr must depend on (>= 1.3.0) for one use in a vignette.
- Enhanced the vignette, "Creating and manipulating frequency tables", `vignettes/creating.Rmd`
- Created a new vignette, "Mobility tables", with extensive examples of models and graphs for square mobility tables.


## Version 0.8-3 (2023-02-16)

- All datasets now classified with \concept{} tags by method of analysis. These can be found using `help.search(pattern, field="concept")`
- Added a vignette, `datasets.Rmd` showing all datasets classified by method tags. Links only work in the `pkgdown` site.
- Added `Asbestos` data


## Version 0.8-2 (2023-01-19)

This is a major enhancement release of the `vcdExtra` package, focusing on documentation and examples.

- added `HouseTasks` data set, illustrating permutation of row / col variables
- package now depends on R (>= 3.5.0) per CRAN nit
- add `Suggests: seriation` to illustrate CA re-ordering of rows/cols based on correspondence analysis
- all .Rd files reformatted and many examples extended.
- the vignette `mosaic.Rmd` on mosaic displays has been extensively revised with examples for square tables and permutation of row / column variables.
- begin to classify datasets with `\concept{}` tags
- Added a new `demo-housing.Rmd` vignette, using content from `demo/housing.R`.

## Version 0.8-1 (2022-04-22)

- rename vignettes to be in order


## Version 0.8-0 (2022-04-20)

- Fixed warning from `expand.dft()` re type.convert
- Old `.Rnw` vignettes converted to `.Rmd`
- Fixed two problems detected in the initial submission.

## Version 0.7-6 (2022-02-12)

- Fix some issues with `CMHtest()` `types` argument #PR11 [Thx: ShuguangSun, Matt Kumar]
- Fix some Winbuilder URL nits; extensive spell checking

## Version 0.7-5 (2020-12-25)

- Fix problem re use of rgl in `mosaic3d()` examples

## Version 0.7-4 (2019-09-25)

- Fix `datasets()` to work with packages not using LazyData #PR7 [Thx: Duncan Murdoch]
- Bump package Version

## Version 0.7-3 (2018-06-04)

- fix Version number for gnm in `DESCRIPTION`
- fix `expand.dft` and `expand.table` to work with tibbles [Thx: Duncan Murdoch]
- vcdExtra gets a hex sticker


## Version 0.7-1 (2017-09-28)

- Fixed buglet in `expand.dft()` when table is 1-dim [Thx: Long Qu]
- Added `zero.test()`, a simple score test for zero inflation
- Development has moved to https://github.com/friendly/vcdExtra

## Version 0.7-0 (2016-01-27)

- Added Glass data
- introduce links and references to DDAR
- added `mcaplot()` to plot MCA solutions in DDAR style
- added `update.xtabs()` method
- updated vignette to refer to DDAR

## Version 0.6-12 (2015-10-06)

- Added Burt data
- Fixed examples/vignette for ggplot_2.0.0


## Version 0.6-11 (2015-09-14)

- bump pkg Version for CRAN

## Version 0.6-10 (2015-07-27)

- Added HospVisits data

## Version 0.6-9 (2015-06-11)

- Added Mice data 
- Removed uses of `summarise()` and Summarise() from demos and examples, in preparation for deprecating them.
- `summarise()` is now deprecated
- Now use `importsFrom()` for all functions from recommended packages

## Version 0.6-8 (2015-04-15)

- Fixed Title: and Description: for CRAN

## Version 0.6-7 (2015-04-02)

- Removed loddsratio (now in vcd) (rev 252)
- Removed print.Kappa (now in vcd) (rev 253)
- Fixed bug in `CMHtest()`: rmeans and cmeans labels were reversed (rev 254)
- Fixed error in Fungicide.Rd, now that we require vcd_1.3-3 (rev 254)
- Added WorkerSat data - 2 x 2 x 2
- Added AirCrash data

## Version 0.6-6 (2015-02-04)

- Minor doc changes

## Version 0.6-5 (2014-11-07)

- Added Cormorants data (fixed to latest)
- Added `LRstats()`, to replace Summarise. Older summarise() and Summarise() will eventually be deprecated.
- Now Suggests: AER for NMES1988 data
- `collapse.table()` now works with array objects


## Version 0.6-3 (2014-10-27)

- Fixed bug in logLik.loglm when the data contain zero frequencies (rev 228)
- Made Summarise generic, adding a method for "glmlist" objects (rev 230)
- Added a coef() method for "glmlist" objects (thx: John Fox)
- Added a Summarise.loglmlist method (rev 232)
- Replaced all documentation uses of summarise() with Summarise() (rev 233)
- Added `cutfac()`, a convenience wrapper for cut() (rev 234)
- Now use `rgl::` in all mosaic3d functions

## Version 0.6-2 (2014-06-30)

- added Summarise, to replace summarise
- Added HairEyePlace data - 4 x 5 x 2, Caithness and Aberdeen hair/eye color (rev 223)
- Added PhdPubs data from Long (1997) - publications by PhD candidates
- Allow Summarise to work with models w/o a deviance() function
- Fixed bug in Summarise wrt degrees of freedom

## Version 0.6-1 (2014-04-14)

- Added ICU data
- Added Toxaemia data - multivariate response contingency table (rev 209)
- Added Vietnam data - 2 x 5 x 4 frequency table (rev 210)
- Added logLik.loglm to allow use of AIC() and BIC() for loglm models (rev 212)
- Fixed loddsratio.Rd to work with revised vcd::CoalMiners data  (rev 212)
- Added blogits for bivariate binary response data
- Added Vote1980 data (rev 214)

## Version 0.6-0 (2014-03-07)

- Removed Authors: in DESCRIPTION, bumped Version

## Version 0.5-12 (2013-12-16)

- Added ShakeWords data set- word frequency counts from Shakespeare (rev 188)
- Added Geissler data-- all family sizes for Saxony sex composition data (rev 190)
- Added logseries functions for the logarithmic series distribution (rev 191)
- Added Depends data -- dependencies of r packages (rev 192)
- Fixed buglet in seq_loglm() not respecting arrays
- Added seq_mosaic() (rev 194)
- Added CyclingDeaths data (rev 196)
- mosaic3d() gets an interpolate= option to control shading levels (rev 197)
- Fixed bug in seq_mosaic, thx to David Meyer (rev 200)
- Fixed bug in seq_loglm() when marginals != 1:nf

## Version 0.5-11 (2013-07-01)

- Added mosaic.glmlist to plot mosaics (or other strucplots) for some or all models in a glmlist (rev 169)
- Added loglin-utilities.R, containing a suite of functions to provide a more conceptual way to specify
  loglinear models by type ('joint', 'conditional', 'mutual', 'markov', 'saturated') (rev 171)
- Added mosaic.loglmlist, similar to mosaic.glmlist for models fit using MASS::loglm (rev 173)
- Both mosaic.glmlist and mosaic.glmlist get an explicit panel=argument; both get some more sensible default
  default arguments (rev 175)
- Added seq_loglm to fit sequential loglm models to marginal subtables, giving a loglmlist result (rev 176)
- Added Accident data (rev 178); fleshed out Accident examples (rev 180)
- Fixed use of ::: for R 3.0.1 (rev 179)
- Fixed various problems related to use of MASS:loglm (rev 181-183)
- Added Titanicp to datasets (rev 185)


## Version 0.5-8 (2013-03-06)

- Revised vcd-tutorial showing some examples of plyr; added a section on RC models
- Added Donner data with example of ggplot2 plot for a binomial glm()
- Added vcd-tutorial section using ggplot2 for Donner data
- Enhanced datasets() to provide a maxTitle argument (rev 153)
- Added doubledecker plots to Dyke.Rd (rev 156)
- Added Draft1970table and Draft1970 data sets (rev 158)
- Added example of doubledecker plots to vcd-tutorial vignette (rev 164)

## Version 0.5-7 (2013-03-01)

- Completed CMHtest methods, adding overall tests across strata in a general way
- CMHtest now gets an S3 generic with a formula interface
- print Kappa gets digits= and CI= arguments

## Version 0.5-6 (2012-11-30)

- Added Hosmer Lemeshow and HLtest methods, including plotting via vcd::rootogram()
- Added CMHtest for general Cochran-Mantel-Haenszel tests
- Revised vcd-tutorial vignette, adding a section on CMH tests; removed dependence on Z.cls

## Version 0.5-3 (2012-03-07)

- Added Mammograms data (4x4, ordered factors, agreement)
- Extended mosaic.glm examples
- Added Alligator data (4x2x2x5, in frequency form)
- Added DaytonSurvey data (5-way, 2x2x2x2x2 in frequency form)
- Extended vcd-tutorial vignette with a section on collapsing over factors
- Removed aperm.* now that aperm.table is in base R

## Version 0.5-2 (2010-11-28)

- Added loddsratio and related methods for log odds ratios, generalizing vcd::oddsratio from
  2 x 2 (x strata) tables to R x C (x strata) tables
- Added as.matrix.loddsratio, as.array.loddsratio methods
- Added some simple plot examples to example(loddsratio), anticipating a plot method
- Added data(Fungicide), a 2 x 2 x 2 x 2 table
- Renamed summarize() and related methods to summarise() to avoid conflict with plyr.
- Addition to vcd-tutorial vignette on use of aperm() with table objects
- Updated demo(yamaguchi-xie) to correct row/col nomenclature and add plot of BIC
- Added aperm() S3 generic to handle table objects
- Moved tv.dat to inst/doc/extdata to avoid warnings in R 2.12+

## Version 0.5-1 (2010-09-17)

- Added Yamaguchi87 data (5x5x3 three-way mobility table in frequency form)
- Added demo(yamaguchi-xie) illustrating fitting and visualization of the models of homogeneous and 
  log multiplicative layer effects fit in Xie (1992, Table 1)
- Added BIC to summarize() and friends
- Added Hauser79 data (two-way mobility table), plus some examples from Powers and Xie (2008)
- Added Crossings() to construct interactions for Goodman 1972 crossings model.
- Added datasets() to list datasets in packages
- Extended description and examples of Kway()
- Added meanResiduals() and extended mosaic.glm() (Heather Turner)
- summarize() gets a sortby argument for glmlist and loglmlist objects

## Version 0.5-0 (2010-04-28)

- Fleshed out mosaic3d, allowing display of observed or expected, internally calculated or externally
  supplied residuals, specifying the initial 3D shape, etc.  This completes the 'top-level' work on
  mosaic3d(), borrowing code from vcd::strucplot.
- Added initial handling for zero cells in the table to mosaic3d().  
- Added center3d() for finding the mean coordinates of shape3d objects.
- Added demo(mosaic-hec) comparing 2D and 3D mosaics for HairEyeColor data
- Gave mosaic3d a label_edge argument, allowing labels for dimensions at minima or maxima
- Made mosaic3d object oriented, giving it a loglm method
- Added Kway(), fitting all 0-way, 1-way, 2-way, ... k-way models in a glm

## Version 0.4-3 (2010-03-25)

- Added demo(mosaic3d-demo), a proof-of-concept for doing 3D mosaic displays
- Added mosaic3d(), an initial basic Version.
- Factored out split3d() and gave it S3 methods

## Version 0.4-2 (2010-03-09)

- Revised vignette("vcd-tutorial"): added some hints for mosaic(), corrected stuff regarding prior
  limitations of mosaic.glm()
- Added demo(Wong3-1): three-way table, with models of conditional association
- Added Suggest: effects for effects plots of glm(), multinom() and polr() models
- Added demo(housing): visualize models fit in example(housing, package="MASS") using mosaic() and
  effect plots.
- Updated demo(Wong2-3): added model comparison plots, glmlist processing
- Added Suggest: VGAM
- Extended package description in vcdExtra-package.Rd
- Added glmlist() to facilitate processing, extraction, plotting, etc. of a collection of glm() models
- Added loglmlist(), for collections of loglm() objects
- Added summarize methods for glm, glmlist, loglm and loglmlist objects

## Version 0.4-1 (2010-02-21)

- Added example(Caesar), illustrating structural zeros
- Re-named Heckman variables to e1971, ..., e1968 (errors from loglm); began example(Heckman)
- Added example(Detergent), example(Dyke)
- Fixed bug with mosaic.glm when data in global environment (Heather Turner)
- Added sieve.glm and assoc.glm methods (MF)
- Added modFit.glm and modFit.loglm
- Added demo(Wong2-3)

## Version 0.4-0 (2010-02-23)

- Added new datasets: data/{Abortion, Bartlett, Caesar, Cancer, Detergent, Dyke, Gilby, Heart, Heckman, Hoyt, Mobility} 
  from mosdata.sas via md2r.sas converter.
- Fixed small documentation warnings
- Switched inst/CHANGES to NEWS

## Version 0.3-6 (2009-04-21)

- Added Depends: gnm
- Added demo/{mental-glm, ucb-glm, vision-quasi}
- Added demo/{occStatus,yaish-unidiff}
- Initial release to CRAN

## Version 0.3-5 (2009-3-6)

- mosaic.glm now uses object$data if available

## Version 0.3-4 (2009-2-11)

- Fixed bugs in mosaic.glm, mosaic.gnm in models with terms like Diag(dest, origin)
  that get included in x$xlevels

## Version 0.3-3 (2009-2-10)

- Fixed bugs in mosaic.gnm
- Fixed print.GKgamma
- Added example of GKgamma to vcd-tutorial

## Version 0.3-2 (2009-2-8)

- Added more examples to mosaic.glm.Rd

## Version 0.3-0 (2009-2-6)

- Fixed bugs in mosaic.glm and mosaic.gnm

## Version 0.2 (2009-2-1)

- Added vcd-tutorial vignette

## Version 0.1 (2009-1-26)

- Initial Version on R-Forge.



