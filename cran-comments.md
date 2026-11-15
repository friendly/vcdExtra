## Test environments
* local Windows 11 x64 install, R version 4.6.1 (2026-06-24 ucrt)
* win-builder: R Under development (unstable) (2026-11-14 r90890 ucrt)

## R CMD check results
0 error(s) | 0 warning(s) | 0 note(s)



## Reverse dependencies checks


We checked 11 reverse dependencies, comparing R CMD check results across CRAN and dev versions of this package.

* We saw 0 new problems
* We failed to check 0 packages


## Comments

## Version 0.9.9

* `color_table()` with `model =` put the residuals into the wrong cells of multi-way tables
with more than one row or column variable, because they were reshaped in the order of the
original array rather than the displayed layout. Residuals are now matched to cells by
factor names, for `loglm` and `glm` models, and for `table`, `structable` and `ftable`
input; a model whose factors don't match the table now gives an error.

* `color_table()`'s `expected =` now also accepts an array over the table's factors (in any
order of dimensions), not only a matrix in the flattened layout.

* `color_table()` now reports its shading note with `message()` instead of `cat()`, so it can
be silenced with `suppressMessages()` or `message: false`, and only when the note is not
already shown in the table (`legend = FALSE`). The "Re-fitting to get frequencies and fitted
values" line from its internal independence model no longer appears.

* `logist_plot()` and its convenience wrappers now use the eight distinct colours of the
Okabe-Ito palette by default. Plots with more than eight observed groups must supply
`group.colors`.

* `woolf_test()` now returns `LOR` and `LOR_se`, the per-stratum log odds ratio and its
standard error, shaped as an array over the stratifying dimensions (e.g., an R x C matrix
for a 4-way table) rather than a flat vector, with dimnames carried over from the input.
This avoids having to recompute them from scratch for further analysis of the log odds
ratios.

* Added `woolf_twoway()`, which decomposes the stratum log odds ratios of a
2 x 2 x R x C table into additive row and column effects, following Tukey's two-way fit
as implemented by the `twoway` package. Since `twoway::twoway()` has no `weight` argument,
`woolf_twoway()` fits the additive model directly by weighted least squares using the same
Woolf inverse-variance weights as `woolf_test()` (`weighted = TRUE`, the default), or via
`twoway::twoway()` directly for the ordinary unweighted mean-polish (`weighted = FALSE`).
The result is an object of class `"twoway"`, so `twoway`'s own `print()`/`plot()` methods
(`which = "fit"` or `"diagnose"`) work on it unchanged.

