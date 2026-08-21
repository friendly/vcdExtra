## Test environments
* local Windows 10 x64 install, R version 4.6.1 (2026-06-24 ucrt)
* R-hub v2: linux, macos-arm64, windows (R-devel) -- all OK (2026-08-20)
* win-builder: R Under development (unstable) (2026-08-17 r90424 ucrt)

## R CMD check results
0 error(s) | 0 warning(s) | 0 note(s)



## Reverse dependencies checks


We checked 10 reverse dependencies, comparing R CMD check results across CRAN and dev versions of this package.

* We saw 0 new problems
* We failed to check 0 packages


## Comments

## Version 0.9.8

This is a significant release, with a number of new features, notably plotting
logistic regression models with display of marginal distributions of x,
and term-level tests for loglinear models.

* Added `logist_plot()` for plotting a `glm(y ~ x, family = binomial)` fit for a single
quantitative predictor together with a representation of the marginal distribution of `x`
within each response group -- mirrored histograms, filled density estimates, or jittered
points -- as suggested by Smart et al. (2004). Supports vector, data frame, and formula
calling conventions; an optional `group=` for grouped fits and marginals with per-group
colours; and `fit.args=`/`marginal.args=`/`marginal.height=` for layer-level customization
beyond the `fit.color=`/`marginal.color=` convenience arguments. `logist_hist()`,
`logist_point()`, and `logist_density()` are convenience wrappers with `marginal=` fixed to
`"hist"`/`"points"`/`"density"`. Work by Gavin Klorfine.

* Fixed bug in `CMHtest3()` (the internal `overall = TRUE` helper) which
suppressed `Df` and `Prob` (p-value) for the overall population results
whenever `types` was not the full default set of four (#2, reported by
@bill-raynor and @imazubi; fix by @danielinteractive in #26).

* Added `drop1.loglm()`, `LRanova()`, and `assoc_strength()` for term-level analysis of `loglm`
models. `drop1.loglm()` (registered as an S3 method for `stats::drop1()`, and also directly
callable by name) performs single-term deletion tests for a model's generating class, refitting
via `update()` and reporting both LR (`G^2`) and Pearson (`X^2`) statistics per term.

* `LRanova()` wraps it with a partial R^2 effect-size column relative to a (nesting-checked)
baseline model.

* `assoc_strength()` wraps it with a partial Cramer's V / Cohen's w
effect-size column, generalized to terms of any order. The fitted models underlying each test
are attached as a `loglmlist` (`attr(., "models")`), reusable directly with `mosaic()`,
`LRstats()`, and `get_models()` without refitting.

* `Summarise()` is now formally deprecated (`.Deprecated("LRstats")`), in
favor of `LRstats()`, which provides the same brief model-fit comparison.
`Summarise()`'s capitalized name reads as an easy-to-mistype near-collision
with `dplyr::summarise()`, which vcdExtra also imports internally; `LRstats()`
has been the recommended replacement since 0.6-5 but was never actually
wired up to warn. See `?vcdExtra-deprecated`.

