# Term-level analysis for `loglm` models

This directory is a development prototype for `dev/anova-like.md`.

- `anova-like.R` -- first draft (kept for reference/history). Implements
  `LRanova()` only.

- `drop1.loglm.R` -- second draft. `drop1.loglm()` is now the primary
  function, with `LRanova()` as a thin wrapper that adds a partial R^2
  column, and `assoc_strength()` adding a partial Cramer's V / Cohen's w
  column. See "What changed" below. Each function also has a roxygen
  `@examples` block illustrating its main variations.

- `drop1-test.R` -- lightweight `stopifnot()`-based check suite (not
  `testthat`, per the priority call below) covering the worked-example
  values, the `models` attribute, the DaytonSurvey data.frame case, and the
  nesting-check error path. Run with `Rscript dev/anova-like/drop1-test.R`
  after any change; it should print `All checks passed.`

Both drop each member of the fitted model's generating class (`$margin`),
refit the reduced hierarchical model, and report changes in likelihood-ratio
and Pearson statistics, degrees of freedom, and the likelihood-ratio p-value.
`LRanova()` additionally reports a partial R^2 relative to a baseline model
(default: mutual independence over the variables in the generating class).

## What changed from the first draft, and why

`anova-like.R`'s `LRanova()` failed on the `DaytonSurvey` example:

```
Error in loglm1.data.frame(formula, data, ..., .call = .call, .formula = .formula) :
  formula specifies no response
```

Root cause: `.margin_formula()`/`.drop_loglm_margin()` rebuilt each reduced
model's formula from `$margin` alone (`~ A:B + A:C + ...`), which has no
LHS. That's fine for array/table-based fits (`loglm(~ (A+B+C)^2, data =
UCBAdmissions)`, no response in the formula to begin with) but wrong for
data.frame-based fits like `DaytonSurvey`, which use `Freq ~ ...` -- the
rebuilt formula silently drops the response, and `loglm1.data.frame`
requires one.

Rather than patch the formula-rebuilding to reattach the response, the
second draft (`drop1.loglm.R`) doesn't rebuild formulas at all. It turns out
`MASS` already supplies `update.loglm()`, `extractAIC.loglm()`, and a
working `terms()`/`formula()` for `"loglm"` objects, which means
**`stats::drop1()` and `MASS::dropterm()` already work correctly on `loglm`
objects today**, response variable included -- they dispatch to
`drop1.default()`/`dropterm.default()`, which refit via `update()`. Verified
against `UCBAdmissions`, `HairEyeColor`, and `DaytonSurvey` (MASS 7.3-66, R
4.6.1); `drop.scope()` on a hierarchical model already restricts to exactly
the generating-class terms, matching what we want.

So the "gap" in `issues/anova-like.md` is narrower than it first looked: the
significance-testing half is already available via base R, just not
presented in loglinear-friendly form and missing the Pearson X^2 column.
`drop1.loglm()` in the second draft refits via `update(object, ~ . - term)`
-- the same mechanism `drop1.default()` uses -- instead of reconstructing
formulas, which is both simpler and avoids the response-variable bug by
construction. It adds:

- the Pearson X^2 column alongside LR G^2 (read directly off `$pearson` on
  each refit -- `loglm` objects store both `$lrt` and `$pearson`, but
  `extractAIC.loglm()`/`drop1.default()` only expose a deviance-derived LRT)
- a `scope` argument restricted to (a subset of) the generating class, with
  validation
- bracket-notation headings via `get_model()`, consistent with
  `LRstats()`/`seq_loglm()` elsewhere in the package

`LRanova()` is now a wrapper around `drop1.loglm()` and additionally
validates that a user-supplied `baseline` is nested within `object` (see
resolved open question below) before computing partial R^2.

## Open questions

- Should the public API be `LRanova()`, `drop1.loglm()`, or both?
  MF: The primary one should be `drop1.loglm`. `LRanova` can be a wrapper.
  **Done in `drop1.loglm.R`.**

- Is the generating class exactly the desired set of tested terms, or should
  lower-order terms receive Type II tests as well?
  MF: Just the generating class.
  **Done** -- `scope` defaults to `$margin` term labels, and a user-supplied
  `scope` must be a subset of them.

- Should a supplied baseline be required to be nested below the fitted model?
  MF: Yes, ensure nesting.
  **Done** -- `LRanova()` checks that every term in `terms(baseline)` appears
  in `terms(object)` (which already includes implied lower-order relatives
  for a hierarchical model, so a plain subset check is sufficient) and that
  `baseline$df >= object$df`. Both checks are exercised in the worked
  examples at the bottom of `drop1.loglm.R`.

- Which sample-size adjustment, if any, should accompany partial R-squared?
  MF: Not clear what this means; what are the possibilities?
  **Proposal (not yet implemented):** the current `Partial R^2 =
  Delta-G^2_term / G^2_baseline` is an *entropy R^2*, directly analogous to
  eta-squared -- it's biased upward for small/noisy terms, especially when
  `Df` is large relative to `N`, because it doesn't discount the G^2 you'd
  expect to see by chance alone.

  The standard fix (paralleling omega-squared
  vs. eta-squared in ANOVA) is exactly the "partial omega^2" already listed
  in `issues/anova-like.md`'s candidate table:
  `(Delta-G^2_term - Delta-df_term) / (G^2_baseline + N)`. The `- Delta-df`
  in the numerator subtracts the expected contribution of a null term
  (E[chi-sq] = df under H0); the `+ N` in the denominator is the standard
  omega^2-style rescaling. This would sit alongside (not replace) the plain
  entropy R^2, as a second, debiased column -- worth confirming before
  adding it.

- For a two-factor term in a multiway table, what conditioning convention and
  effective sample size should define partial Cramer's V?
  MF: Not clear what this means; what are the possibilities? ... Could this
  be an option, `c("Cramer", "Cohen")`, allowing either?
  **Done** -- `assoc_strength(object, scope, method = c("Cramer", "Cohen"))`
  is implemented in `drop1.loglm.R`, defaulting to `"Cramer"`:
  
  - `"Cramer"`: `V = sqrt(Delta-X^2_term / (N * (k - 1)))`, where `k` is the
    *smallest* factor level count among the term's variables (not just the
    two-way `min(r,c)` -- this generalizes to terms of any order, and
    reduces exactly to the ordinary two-way Cramer's V when the term has two
    factors).
    
  - `"Cohen"`: `w = sqrt(Delta-X^2_term / N)`, unbounded, no reference to
    table shape.
    
  - When every factor in a term is binary, `k - 1 = 1` and the two methods
    coincide exactly -- seen in the DaytonSurvey worked example, where every
    variable is binary and the `"Cramer"`/`"Cohen"` columns are identical;
    they diverge properly on HairEyeColor's `Hair:Eye` term (4x4 levels).
  - Sanity check: on UCBAdmissions, the *partial* `Admit:Gender` V comes out
    at 0.016 (barely any association once Dept is accounted for), versus a
    *marginal* V of 0.143 from `vcd::assocstats()` on the raw 2-way margin
    -- exactly the well-known Simpson's-paradox story for this dataset (the
    apparent gender gap is really a department effect). Good evidence the
    formula is behaving sensibly and not just running without error.
    
  - N is read off `object$fitted`; if the model wasn't originally fit with
  `fitted = TRUE`, `assoc_strength()` calls `update(object, fitted = TRUE)`
  once to get it (loglm objects don't store `$fitted`/`$frequencies` by
  default).

## Next steps

- Confirm (or amend) the partial-omega^2 proposal above (partial R^2's
  sample-size adjustment is still open; `assoc_strength()`/Cramer's V vs.
  Cohen's w is now resolved and implemented).

- Decide whether `drop1.loglm()`/`LRanova()` should also handle `glm`
  (Poisson) models directly (the issue's "Handling the glm case" section),
  or stay `loglm`-only with `car::Anova()` left as the `glm` path.

- Move to `R/drop1.loglm.R` with roxygen docs (markdown syntax, no Rd
  macros) once the API above is confirmed, and register `S3method(drop1,
  loglm)` in `NAMESPACE`.

- Could something more generally useful be added to what `drop1.loglm()` returns?
  It is an object of class `c("anova", "data.frame")` for its' print() method,
  but it could also carry an attribute, e.g., `models`, giving either the symbolic
  formulas for the models, or the result of fitting them. I'm thinking of an
  extension of `mosaic()` that could plot the collection models, using `mosaic.loglmlist()`
  
  **Done** -- turns out no extension of `mosaic()` was needed: `mosaic.loglmlist()`,
  `LRstats.loglmlist()`, and `get_models.loglmlist()` already exist in the
  package and work on any object of class `loglmlist`. `drop1.loglm()` was
  already fitting every drop-one model internally to compute the
  Delta-statistics, so `attr(result, "models")` now attaches them as a
  `loglmlist` (`"<none>"` + one per dropped term) at no extra fitting cost.
  `LRanova()`/`assoc_strength()` inherit it automatically since they build on
  `drop1.loglm()`'s return value. Verified end-to-end: `mosaic(attr(result,
  "models"), ask = FALSE)` and `LRstats(attr(result, "models"))` both render
  correctly against the UCBAdmissions example. `LRanova()`'s baseline model
  stays separate as `attr(., "baseline")` (a single reference model, not a
  `<none>`/drop-one row) rather than being folded into the same list.
  
- `testthat` coverage: low priority for now. Worked examples in
  `drop1.loglm.R` are wrapped in `if (FALSE) { ... }` and run via `source()`
  for interactive/manual checking in the meantime.

