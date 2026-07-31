# CMHtest 2 x 2 display

## Idea

`CMHtest()` computes four generalized Cochran-Mantel-Haenszel statistics (`cor`, `rmeans`,
`cmeans`, `general`), currently printed as a flat 4-row table. 

The idea: reorganize these into a
2x2 layout crossing how each margin (row variable, column variable) is treated -- **general**
(nominal) vs. **ordered** (scored) -- since that's actually what distinguishes the four tests from
each other, and the flat table obscures that structure.

## Statistical structure (confirmed against `R/CMHtest.R`)

In `CMHtest.default()`/`CMHtest3()`, `cmeans` is dropped when `rscores` is `NULL` (so `cmeans`
depends on *row* scores) and `rmeans` is dropped when `cscores` is `NULL` (so `rmeans` depends on
*column* scores). Concretely: `rmeans` ("Row mean scores differ") assigns scores to the **column**
variable and tests whether the mean column-score differs across **row** categories -- i.e. row is
treated as nominal groups, column as ordered. `cmeans` is the mirror image.

|                  | col: general (nominal) | col: ordered (scored) |
|------------------|-------------------------|------------------------|
| **row: general** | `general`, df=(R-1)(C-1) | `rmeans`, df=(R-1)    |
| **row: ordered** | `cmeans`, df=(C-1)      | `cor`, df=1            |

This is the opposite of the version originally sketched below (which had `rmeans`/`cmeans`
swapped) -- worth noting there's a `# FIXED: rmeans and cmeans tests were labeled incorrectly`
comment already sitting at the top of `R/CMHtest.R`, so this may have tripped up the original
draft too.

## Worked example (Mental data, ses [R=6] x mental [C=4])

```
> CMHtest(Freq ~ ses + mental, data=Mental)
                 AltHypothesis  Chisq Df       Prob
cor        Nonzero correlation 37.156  1 1.0907e-09
rmeans  Row mean scores differ 40.297  5 1.3012e-07
cmeans  Col mean scores differ 40.666  3 7.6971e-09
general    General association 45.958 15 5.4003e-05
```

|                  | col: general        | col: ordered        | **diff (gen−ord)** |
|------------------|----------------------|----------------------|----------------------|
| **row: general** | 45.958, df=15        | 40.297, df=5         | 5.661, df=10          |
| **row: ordered** | 40.666, df=3         | 37.156, df=1         | 3.510, df=2           |
| **diff (gen−ord)** | 5.292, df=12       | 3.141, df=4          | 2.151, df=8 (see below) |

Margins: for a fixed column, diff = (row:general entry) − (row:ordered entry), df = difference of
the two df's. Symmetrically for a fixed row, diff = (col:general entry) − (col:ordered entry).
E.g. the `col:general` diff is `general − cmeans` = 45.958 − 40.666 = 5.292, df = 15 − 3 = 12.

**The bottom-right corner** (diff of diffs) is algebraically consistent regardless of which path
you take to it: `general − rmeans − cmeans + cor` = 2.151, df = (R−2)(C−2) = 8, and this matches
subtracting either pair of margin diffs (5.661 − 3.510 = 2.151, or 5.292 − 3.141 = 2.151). That
consistency is just inclusion-exclusion arithmetic, though -- **it does not by itself establish
that 2.151 is meaningfully chi-square(8) distributed under the null.** `general`, `rmeans`,
`cmeans`, `cor` are four different quadratic-form statistics, not a nested sequence of LR tests,
so unlike degrees of freedom (which are guaranteed to combine this way), additivity of the
*statistics* needs checking, not assuming. Worth simulation-checking before leaning on the corner
cell for anything -- same spirit as what Gavin is doing for the `woolf_test()` decomposition.
**Recommendation for v1: show the corner cell but flag it as experimental/unverified, or omit it
and only show the two 1-way margins.**

## Implementation notes

* **Where it lives**: this is pure reformatting of statistics `CMHtest()` already computes --
  nothing new needs to be calculated. Belongs in `print.CMHtest()`, not `CMHtest()` itself. Sketch:
  a new argument, e.g. `print(x, layout = c("table", "2x2"))`, defaulting to the current flat
  `"table"` behavior, with `"2x2"` opting into this display. (Or a separate `format_CMH_2x2()`
  helper that `print.CMHtest()` calls when requested -- keeps the reshaping logic testable on its
  own.)

* **Data needed**: `x$table` is a matrix with rownames `cor`/`rmeans`/`cmeans`/`general` (subset
  thereof) and columns `Chisq`/`Df`/`Prob`; `x$names` gives the row/column variable names (e.g.
  `c("ses", "mental")`) for axis labels.

* **Precondition**: only makes sense when all four types are present. If `CMHtest()` was called
  with `types` restricted, or `rscores`/`cscores = NULL` (which drops `cmeans`/`cor` or
  `rmeans`/`cor` respectively -- see `R/CMHtest.R` lines ~427-430), the 2x2 layout can't be built;
  fall back to the existing flat table with a message, don't error.

* **What to show in each cell**: implemented as a `scale = FALSE` argument (plain logical, same
  reasoning as `stars` above) to `print.CMHtest()` in `dev/print-CMHtest-2x2.R`. Default shows
  `X^2 (df)` per cell, as above. `scale = TRUE` shows `X^2/df` instead -- normalizes for the very
  different df's across cells (1 vs 3 vs 5 vs 15 here), giving a more comparable "strength of
  evidence per df" reading -- and in that mode the `(df)` is dropped from the cell text entirely
  (it's no longer the divisor being displayed alongside the raw stat, so showing it would be
  redundant/confusing); the heading note switches from "Cell values: X^2 (df)" to "Cell values:
  X^2/df" accordingly. Significance stars, when `stars = TRUE`, are still computed from the
  underlying raw (X^2, df) pair regardless of `scale`, since the p-value depends on the actual
  chi-square statistic, not the ratio.

* **Significance stars**: made optional via a `stars = FALSE` argument (default off, matching
  `layout = "table"`'s always-plain display) -- in the Mental example all four core cells are
  highly significant, so stars clutter more than they discriminate. **Not** a `match.arg()`-style
  enum like `layout`: `match.arg()` only accepts a logical `choices` vector like `c(FALSE, TRUE)`
  by accident, when the caller leaves the arg at its default (its first check is
  `identical(arg, choices)`, true only for the untouched default promise); an explicit
  `stars = TRUE` call hits `is.character(arg)` and errors. So `stars` is just a plain logical
  argument (`isTRUE(stars)`), not routed through `match.arg()`. Implemented in
  `dev/print-CMHtest-2x2.R`. Still need a second, less lopsided example (real or synthetic) to
  judge whether the star thresholds are useful when `stars = TRUE`.

* **Strata**: `CMHtest(..., strata = ...)` (implicit for 3+ way tables) returns a *plain, unclassed
  `list`* of per-stratum `"CMHtest"` objects, plus one named `"ALL"` when `overall = TRUE` -- e.g.
  for `MSPatients` (4x4x2 strata), a list of length 3: `"Patients:Winnipeg"`,
  `"Patients:New Orleans"`, `"ALL"`. Each *element* already carries class `"CMHtest"` (that's why
  `print(cmh_ms)` with no extra args happens to print all three in `layout = "table"` -- R's default
  list-printing dispatches `print()` on each classed element) but the outer list itself has no
  class to dispatch on, so `print(cmh_ms, layout = "2x2")` can't work directly -- there's nowhere
  for `layout`/`stars`/`scale` to be forwarded to the per-element `print.CMHtest()` calls. Since
  each element is already a proper `"CMHtest"` object, no new class/method is needed -- just an
  `lapply()` over `print.CMHtest()`: added `print_CMHtest_list(x, ...)` in
  `dev/print-CMHtest-2x2.R`, which forwards `...` (layout, stars, scale, digits) to every stratum +
  `"ALL"`. Each stratum's own heading (`"in stratum ..."` vs `"Overall tests..."`) already
  distinguishes them, so no extra separator logic was needed.

* **`# TODO: handle the printing of pvalues better`** (already noted at the top of
  `R/CMHtest.R`) and **`# TODO: determine score types (integer, midrank) for heading`** are
  existing nearby TODOs in the same function -- this display could reasonably land in the same pass
  as those, since it touches the same print method.

## An LRstats/loglinear analog (verified 2026-07-31)

The `Mental` dataset's own `@examples` in `R/data.R` (lines ~2478-2498) fit four Poisson GLMs --
`indep`, `linlin` (+`Rscore:Cscore`), `roweff` (+`mental:Cscore`), `coleff` (+`Rscore:ses`) -- plus
`rowcol` (both interaction terms together) and compare them with `LRstats()`. This is a real
loglinear-model analog of the CMH 2x2 idea: each interaction term corresponds to treating a margin
as general (its own factor/dummy effects) vs. ordered (a single score). Checked this by running the
LR tests against `indep` and comparing to `CMHtest()` on the same data (`Freq ~ ses + mental`,
ses=row, mental=column):

| CMH cell (row:col) | CMH stat | LR test vs. `indep` | LR stat |
|---|---|---|---|
| `cor` (ord, ord)      | 37.156, df=1  | `linlin` (`Rscore:Cscore`)         | 37.523, df=1 |
| `cmeans` (ord, gen)   | 40.666, df=3  | **`roweff`** (`mental:Cscore`)     | 41.137, df=3 |
| `rmeans` (gen, ord)   | 40.297, df=5  | **`coleff`** (`Rscore:ses`)        | 40.589, df=5 |
| `general` (gen, gen)  | 45.958, df=15 | fully saturated `mental*ses`       | 47.418, df=15 |

Each pair lines up closely (same df, similar magnitude) -- expected, since CMH and LR tests are
asymptotically equivalent ways of testing the same association hypotheses.

**Two things worth flagging, both found by actually running it rather than assuming:**

1. **Naming trap**: `roweff` (the model with `mental:Cscore`, i.e. the *column* variable `mental`
   getting its own effect while `ses`'s contribution is a single score `Cscore`) matches CMH's
   `cmeans`, not `rmeans` -- and `coleff` matches `rmeans`, not `cmeans`. The variable names in the
   `R/data.R` example describe which score is being used, not which CMH cell they land in; they're
   inverted from what the names suggest at first glance.

2. **`rowcol` is not the LR analog of `general`.** Adding both interaction terms together only
   gets to df=7 (`anova(indep, rowcol)`), not the full df=15 of general association -- there's a
   1-df redundancy between the two term sets when combined, and even accounting for that, `rowcol`
   is a genuinely different (more restrictive) "additive row + column effects" model, not the fully
   saturated one. The real LR analog of `general` is the saturated model `mental*ses` (df=15,
   matches CMH's `general` closely, see table above).

   That said, `rowcol` turns out to be useful for something CMH's naive arithmetic can't do
   cleanly: `anova(rowcol, saturated)` gives a **proper nested LR test for lack of fit beyond the
   additive effects model** -- df=8 (same df as the CMH "corner" cell's `(R-2)(C-2)`, by
   construction), LR-chisq=3.045, p=0.93. Unlike the CMH corner cell (`general - rmeans - cmeans +
   cor`, an inclusion-exclusion combination of four different quadratic forms that isn't
   guaranteed >= 0), this is a genuine deviance difference between nested models, so it's
   **guaranteed non-negative**. The numeric value differs from the CMH corner's 2.150 (different
   statistics, not the same quantity computed two ways), but this may be the more principled way to
   get a "residual/interaction" cell if the experimental CMH corner cell doesn't pan out.

**Suggestion**: if the `print.CMHtest(layout = "2x2")` corner cell turns out to be unreliable
(negative, or just not trustworthy as a chi-square), this LR-based `rowcol`-vs-saturated residual
test is a solid fallback for that specific cell, at the cost of needing to fit loglinear models
rather than just reorganizing `CMHtest()`'s existing output.

See `dev/CMH-2x2-LRstats.Rmd` for a runnable version of everything in this section.
