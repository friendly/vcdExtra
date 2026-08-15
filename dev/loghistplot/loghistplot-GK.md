# GK `loghistplot` notes

## Q from email

> Whether `...` should stay silently ignored, error on unrecognized names (catch typos), or actually forward into `theme()`/`geom_point()`/`geom_histogram()` for further customization. Flagged in the review notes rather than deciding unilaterally.

- This is tricky; I kind of wish there was an easy way to export a ggplot object so that `labs()` and `theme()` calls could be added to `logist_plot()`.

- What if we forward `...` to the `geom` corresponding to `marginal=`?
  + E.g., for `marginal = "points"`, `...` would forward to `geom_point()`
  + Wondering what to do with `marg.color=` argument if we go this route
  + Then can have an argument within `logist_plot()` for `theme()` control (e.g., `theme.args=`)
  + Maybe also a `fit.args=` argument to cover `geom_smooth()` customization

## `dev/loghistplot-extensions.md`

### `group=` argument

- GPT: Can reserve `|` formula conditioning for faceting, and use `group=` for the described grouping
- Can error if `group=` is used when `marginal = "hist"`

### `marginal = "density"`

- This is great; I think it is also a potential indirect solution to the grouping issue when `marginal = "hist"`:
  + Thinking it would provide similar information to the histograms and may look neater (e.g., than stacked bars)
- Can adjust alpha for density curves and give each group different line types and shading
  + Or perhaps stack each group's density curve on top of one another
  + E.g., `dev/loghist-ggdist-naive.png`, have another pair of age "stripes"; one above the `survived` one and the other below the `died` one
- I think we should include `adjust` as an argument, though see above relating to forwarding `...`. If we forward `...` to `geom_density()` when `marginal = "density"`, we won't need to explicitly include this

## Extra notes from GK (model MA-2)

- Once we think this is good to go I should do one (hopefully last) check with Claude Fable to see if it can catch anything
- We should have an argument for plot title [MF: NO; just use `labs()`]
- Should support `with(data, logist_plot(y ~ x, ...))`

## Extra notes from GPT (model 5.6 Sol)

Independently re-verified 2026-08-09 with R 4.6.1, ggplot2 4.0.3, cowplot 1.2.0,
and roxygen2 8.0.0. Every runtime claim retained below was reproduced from the current
`dev/loghistplot2.R`, including forced `ggplot_build()`/`ggsave()` checks where applicable.

### Release / CRAN findings

- **No function-specific package-check failure appeared after simulated integration.** In a
  fresh temporary package copy, I moved `loghistplot2.R` into `R/`, regenerated Rd/NAMESPACE,
  built the package with all vignettes, and ran `R CMD check --no-manual` on the tarball. The
  result was `Status: OK`, including installation, dependencies, S3 consistency, static code
  analysis, Rd, examples, existing tests, and vignette rebuilding. This does not replace a
  final multi-platform `R CMD check --as-cran`, but it confirms that the current function does
  not introduce an immediately detectable code, namespace, documentation, or example failure.

- **Correct the article metadata and add its DOI.** The authors are Jennifer Smart,
  William J. Sutherland, Andrew R. Watkinson, and Jennifer A. Gill, pages 100--102. Therefore,
  the current `Smart, S. M. et al.` attribution has the wrong initials. The DOI is
  `10.1890/0012-9623(2004)85[100:ANMOPT]2.0.CO;2`. The existing `esapubs.org` URL returned HTTP
  200 in direct verification. Keep that working link if desired and add the DOI as stable
  bibliographic metadata.

- Before moving this file into `R/`, trim the large historical review log and machine-specific
  Windows paths from the production source. In particular, several comments point maintainers
  to `dev/loghist-test.R`, but `^dev$` is excluded by `.Rbuildignore`, so those references would
  be unavailable in the CRAN source tarball. Keep concise rationale beside non-obvious code and
  move the review history to a development note.

### Correctness and rendering bugs

- **Sparse histograms produce repeated/non-integer `Count` labels.** When `max_count = 1`,
  `pretty(c(0, max_count))` returns fractional ticks such as 0.2, 0.4, etc.; the later `round()`
  turns these into repeated labels (`0, 0, 0, 1, 1, 1`) on both halves of the secondary axis.
  The plot is especially cluttered with small samples or many bins. Filter the pretty breaks
  to distinct whole-number counts (and ensure 0 plus a useful upper tick remain) before mapping
  them to probability-axis positions.

- **Nonzero range is not sufficient validation for histogram arithmetic.** Two reproducible
  cases return a plot but lose the histogram layers with only ggplot warnings:

  - `x = c(1, 1 + .Machine$double.eps)`, `bins = 30` generates non-unique internal breaks.
  - A finite range from `-1e308` to `1e308` makes `(max_x - min_x) / bins` infinite.

  Subnormal ranges can underflow `bin_width` to zero as well. Validate that the range and
  `bin_width` are finite and strictly positive and that the effective breaks are unique. It may
  be better to reduce the effective number of bins for low-resolution data or fail clearly
  instead of returning a visually incomplete object.

- **A failed smoother can still produce a successful plot object with no fitted curve.** With
  `marginal = "points"` and a finite numeric predictor alternating between `-1e308` and
  `1e308`, the function returns a ggplot object; rendering warns `Failed to fit group -1`
  because `qr.default()` receives `NA/NaN/Inf`, and the built smooth layer contains zero rows
  while the point layer still contains all observations. Consider fitting/validating the
  binomial model explicitly, or at minimum document this behavior and test that rendering
  failures are detected.

- **Histogram composites ignore ordinary post-hoc `labs()` changes.** The returned object is a
  `ggdraw()` canvas containing already-captured plot grobs. With the current ggplot2/cowplot
  versions, saved output from `p + labs(title = "T")` and from
  `p + labs(x = "NEWX", y = "NEWY")` was byte-for-byte identical to saved output from `p`;
  the new title and labels never reached the embedded plots. Title/axis-label controls need to
  be applied before grob capture, and the help should state which normal ggplot additions are
  ineffective for histogram mode. Add regression snapshots for title, labels, themes, and
  saved output.

### Input/API bugs and semantic gaps

- **`xvar`/`yvar` validation is incomplete and sometimes selects the wrong data.** Confirmed
  failures include vector, zero-length, zero, and negative selectors producing base-R
  length/missing-value errors; a fractional selector such as `1.9` silently selects column 1;
  and factor selectors are accepted accidentally. Validate that each selector is exactly one
  non-missing character name or one finite whole-number position in range.

- **Duplicate data-frame names break even positional selection.** Numeric positions are first
  converted back to names and then extracted by name. With two columns both named `dup`,
  `yvar = 2` resolves to the string `"dup"`, and `x[["dup"]]` extracts the first duplicate
  rather than the second. Either extract numeric selectors directly by position or reject
  duplicate names when name-based selection would be ambiguous. Unnamed data frames also fail
  with `missing value where TRUE/FALSE needed` rather than a diagnostic.

- **The claimed equal-length/numeric-predictor fix is incomplete.** Unequal vectors still fail
  inside `data.frame()` with `arguments imply differing number of rows`; character, factor,
  complex, matrix, and matrix-valued formula predictors fail later with unrelated messages;
  while logical, Date, and POSIXct predictors are accepted despite the documentation saying
  numeric. Validate equal positive lengths, an atomic one-dimensional numeric `x`, and finite
  values before building the internal data frame. If Date/POSIXct support is intentional, it
  needs an explicit, tested modeling conversion rather than accidental acceptance.

- **Response type checking is too permissive outside the documented types.** Complex, Date,
  and `difftime` responses can be silently converted to 0/1, while raw/list responses fail with
  obscure downstream errors. Restrict `y` explicitly to numeric/logical/factor/character (and
  reject matrices/dimensions), then validate finite numeric values. A matrix response such as
  `cbind(success, failure)` should get a direct unsupported-response error rather than
  “found 0 distinct values.”

- **Formula calls do not actually share the same missing-data policy under all user options.**
  With `options(na.action = "na.fail")`, `logist_plot(y ~ x, data = d)` errors in
  `model.frame()`, whereas the equivalent vector call removes the incomplete row and succeeds.
  If the intended policy is always the internal complete/finite-case filter, call
  `model.frame(..., na.action = stats::na.pass)` and let the common implementation perform it.

- **Named formula dispatch does not propagate through the convenience wrappers.**
  `logist_plot(formula = y ~ x, data = d)` works, but both
  `logist_hist(formula = y ~ x, data = d)` and `logist_point(formula = y ~ x, data = d)` fail
  with `argument "x" is missing`. Either give the wrappers a formula-aware interface or narrow
  the claim that they accept the same calling conventions; positional formula calls do work.

- **The modeled-event convention is still missing from user-facing help and is not portable
  for character responses.** The implementation comment defines the mapping, but `bin$levels`
  is discarded and the Rd only says that two-level character/factor/logical responses are
  accepted. Users therefore cannot see which probability the curve represents. In addition,
  `sort()` of character values follows the current collation locale, so the event can change
  across systems. Document the mapping in `@param y`/details and label the axis as something
  like `Pr(y = <event>)`; preferably add an explicit `event=`/`success=` argument. For a
  deterministic fallback, do not rely on locale-sensitive character sorting.

- The formula validation counts columns of `model.frame()` rather than validating the formula
  and resulting predictor shape. This gives misleading messages for `~ x` (reported as zero
  predictors rather than a missing response) and lets a single matrix-valued term such as
  `poly(x, 2)` pass the formula check only to fail later as “No complete observations.” Inspect
  the terms response and then validate that the evaluated predictor is a one-dimensional
  numeric vector.

### Additional tests worth adding

- Force both plot construction **and** `ggplotGrob()`/`ggsave()` in tests; several failures are
  lazy or are swallowed as rendering warnings.
- Add targeted cases for `max_count = 1` count labels, near-zero/overflowing ranges,
  duplicate/unnamed data-frame columns, malformed selectors,
  matrix-valued formula terms, unsupported classes, global `na.action = "na.fail"`, named
  formula calls through both wrappers, and post-composition title/label behavior.
- A visual regression test is justified for histogram mode because object-class assertions and
  outer layer counts do not detect failed or missing plots inside captured grobs.
