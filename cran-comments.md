## Test environments
* local Windows 11, R version 4.6.1 (2026-06-24 ucrt) 
* win-builder R Under development (unstable) (2026-07-30 r90327 ucrt)

## R CMD check results
There are no ERRORs or WARNINGs or NOTEs



## Version 0.9.7

This is a significant release, with a number of new features 

* `color_table()` now displays multiple row variables as separate stub columns (e.g. "Class",
  "Sex") instead of concatenated labels like "1st_Male", using `gt`'s multi-column stub support.
  Mirrors the existing column-spanner display for multiple column variables, and the two can be
  combined in the same table.
* Added more worked examples to the `Mental` dataset documentation
* General package clean-up: removed stale pre-roxygen and superseded `dev`/`extra` files, and
  reorganized `color_table()`/`assoc_graph()` development notes into their own subfolders
* Now reference new work on tidy methods (`as_*()` methods, `collapse_levels()`, ...) in the tidyCats vignette
* Added `breslow_day_test()` for the Breslow-Day test of homogeneity of odds ratios across strata
  in 2x2xk tables, generalized to handle tables of any dimensionality, with an optional Tarone
  correction and a `decompose` option (for 2x2xRxC tables) giving a `woolf_test()`-style
  row/column/residual decomposition of the overall test
* Added a `layout = "2x2"` display option to `print.CMHtest()`, reorganizing the four CMH statistics
  (`cor`, `rmeans`, `cmeans`, `general`) into a 2x2 table crossing how the row and column variables are
  each treated (general/nominal vs. ordered/scored). Falls back to the existing flat table
  display, with a warning, when not all four statistics are available



# reverse dependencies

> devtools::revdep()
 [1] "aplore3"            "CASIdata"           "catdata"            "genridge"          
 [5] "gnm"                "heplots"            "iarm"               "jmv"               
 [9] "junco"              "public.ctn0094data"
 
* `revdepcheck::revdep_check()` could not be run due to an incompatibility between
`revdepcheck` 1.0.0.9002 and `gmailr` 3.0.0 (`mime` is no longer exported by
`gmailr`). The 10 reverse dependencies were checked manually using

* `devtools::check_reverse_dependencies()` / `rcmdcheck::rcmdcheck()`, and no new
problems were introduced by this release.

