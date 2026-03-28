## Test environments
* local Windows 10, R version 4.5.2 (2025-10-31 ucrt)
* win-builder R version 4.5.2 (2025-10-31 ucrt)
* win-builder R Under development (unstable) (2026-03-16 r89642 ucrt)
* R-hub: linux (R-devel), windows (R-devel), macos-arm64 (R-devel)

## R CMD check results
There are no ERRORs or WARNINGs or NOTEs

The use of `rgl` triggered harmless warnings on headless rhub machines. This was resolved
by moving `rgl` from "Depends:" to "Suggests:" because it was only used in one function
and was suitably trapped there. See below.

### Previous rhub WARNING (resolved)
An earlier rhub run produced:
  `checking whether package 'vcdExtra' can be installed ... WARNING`
  `Warning: 'rgl.init' failed, will use the null device.`

This was caused by `rgl` being listed in `Imports`, which forced it to load
(and attempt to initialise an OpenGL display) on headless CI runners. Fixed
by moving `rgl` to `Suggests`, since `mosaic3d()` is the only function that
uses it and already guards its use with `requireNamespace("rgl")`.

## Version 0.9.3

This bundle brings quite a few enhancements, improvements in documentation and bug fixes

* Added tidy conversion functions: `as_array()`, `as_caseform()`, `as_freqform`, `as_table()` PR #22 [Thx: Gavin Klorfine]
* Fixed bug in `mcaplot()` coming from `ca::cacoords(): "non-conformable arguments"
* Expanded documentation of `color_table()` to give better advice on how to use this in Rmd or qmd documents.
* Added: `knit_include()` as a general solution to using `gt`, `DT`, `plotly`, ... outputs in non-HTML documents.
* Added `pairs_diagonal_mosaic()`, overriding the {vcd} version to give more flexibility in printing the cell values in the diagonal cells. PR #24 [Thx: Gavin Klorfine]

## Version 0.9.2

* Added a `label = c("name", "formula")` argument to `LRstats()` to provide for labeling models by their model formulas in the output using `get_models()`.
* Handle list (...) of models with formula labels more flexibly in `LRstats()`
* Document `get_model()` and `get_models()` together
* Added `assoc_graph() and a plot method for association graphs of loglinear models.
* Added edge weights to `assoc_graph()` representing partial G^2 or Cramer's V

# reverse dependencies

> devtools::revdep()
 [1] "aplore3"            "CASIdata"           "catdata"            "genridge"          
 [5] "gnm"                "heplots"            "iarm"               "jmv"               
 [9] "junco"              "public.ctn0094data"
 
> revdepcheck::revdep_check(num_workers = 4)

We checked 9 reverse dependencies, comparing R CMD check results across CRAN and dev versions of this package.

 * We saw 0 new problems
 * We failed to check 0 packages

