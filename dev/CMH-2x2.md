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

* **What to show in each cell**:
  - The values of X^2 and their degrees of freedom, as above.
  - Alternative worth trying: X^2 / df instead of raw X^2 -- normalizes for the very different df's
    across cells (1 vs 3 vs 5 vs 15 here), giving a more comparable "strength of evidence per df"
    reading. Probably better than raw X^2 for this specific display, since the whole point is
    comparing cells that have different df.

* **Significance stars**: in this example all four core cells are highly significant, so stars
  wouldn't discriminate -- need a second, less lopsided example (real or synthetic) to see whether
  star thresholds are actually useful here before committing to a scheme.

* **`# TODO: handle the printing of pvalues better`** (already noted at the top of
  `R/CMHtest.R`) and **`# TODO: determine score types (integer, midrank) for heading`** are
  existing nearby TODOs in the same function -- this display could reasonably land in the same pass
  as those, since it touches the same print method.
