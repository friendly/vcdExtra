## Test environments
* local Windows 10, R version 4.5.2 (2025-10-31 ucrt)
* win-builder R Under development (unstable) (2026-02-08)


## R CMD check results
There are no ERRORs or WARNINGs or NOTEs

## Version 0.9.1 (2026-02-08)

This is a major release of the package, adding facility to show observations in mosaic plots as jittered points and a new
visualization method based on background shading of frequency tables.

* Added `labeling_points()` for mosaic displays to show observed or expected frequencies as random points in the tiles
* Changed defaults for `clip` and `margin` in `labeling_points()` to be more sensible [Thx: Achim Zeileis]
* Added `color_table()` to display frequency tables with color shaded backgrounds to show patterns
* Refactored `color_table()` as S3 methods, adding support for frequency data frames. Added file output, other formatting options
* Print message for `color_table()` giving the model fit statistics.
* Now provide `color_table(values =)` to display either the frequencies (default) or residuals in the table.
* Add `legend = "note"` to include a table note regarding what is shown.
* Added `Reinis` data as an example of a higher-way table, 2^6
* Make the default label for `seq_loglm()` models reflect the model type
* Added `get_models()` to extract model formulas from `loglmlist` and `glmlist` objects
* Added `get_model()` to do the same for `loglm()` and `glm()` objects
* Fixed bug in `get_models()` where the `abbrev` argument caused an error

# reverse dependencies

> devtools::revdep()
 [1] "aplore3"            "catdata"            "genridge"           "gnm"                "heplots"
 [6] "iarm"               "jmv"                "junco"              "public.ctn0094data" "reappraised"

> revdepcheck::revdep_check(num_workers = 4)

## revdepcheck results

We checked 10 reverse dependencies, comparing R CMD check results across CRAN and dev versions of this package.

 * We saw 0 new problems
 * We failed to check 0 packages

