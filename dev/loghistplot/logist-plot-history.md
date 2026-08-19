# `logist_plot()` development history

Full design-decision and review-and-fix log for `logist-plot.R` (formerly `loghistplot3.R`),
moved out of the source file per `cran-ready.md` item 7 ("trim the long development and review
history from the promoted source file... source hygiene, not a CRAN blocker"). Preserved verbatim
below, exactly as it stood at the top of the file before this move -- including the
`C:\Dropbox\...`-style machine-specific paths and `dev/`-relative references, which is exactly why
it doesn't belong in the file itself once that file moves to `R/`.

```text
# Plotting logistic regressions with marginal distributions of Y -- v3
#
# GOAL: `logist_plot()`, a general function for plotting a `glm(y ~ x, family = binomial)`
#       fit for a single quantitative predictor, with a representation of the marginal
#       distribution of cases for which y==0 vs. y==1 (histogram, density, or jittered points),
#       optionally conditioning the fit and supported marginals on a grouping variable.
#
# This version starts from dev/loghistplot/loghistplot2.R and replaces the histogram-mode
# cowplot composite with rectangle layers drawn in the main plot's probability coordinates.
# Both earlier implementations are left untouched for reference.
#
# Suggested in: Smart et al. (2004), A New Means of Presenting the Results of Logistic Regression
# Bulletin of the Ecological Society of America, 85(3),
# https://esapubs.org/bulletin/backissues/085-3/bulletinjuly2004_2column.htm#tools1
#
# See: How I did this in DDAR Ch. 7. Code for all examples: http://ddar.datavis.ca/pages/Rcode/ch07.R
#   Sample plots: Fig 7.7, 7.8 Arthritis data. See also: "C:\Dropbox\Documents\VCDR\ch07\R\donner1.R"
# See also: vcd::binreg_plot() -- similar, using {grid} graphics directly.
# See also: popbio::logi.hist.plot(), https://www.rdocumentation.org/packages/popbio/versions/2.8/topics/logi.hist.plot
#   MF improved code in "C:\Dropbox\Documents\VCDR\functions\logi.hist.plot.R"
# Original sketch implemented by Scott Chamberlain, https://recology.info/2012/01/logistic-regression-barplot-fig/
#
# TODO status vs. dev/loghistplot.R:
# ✅ [**DONE**] Combine hist/points into one function via `marginal = c("hist", "points")` (Gavin)
# ✅ [**DONE**] Make into a proper, general function with x=, y=, data= -- via S3 methods below
#        (default / data.frame / formula), matching base R's plot()/boxplot() convention.
#        Deliberately dropped `data=` from the generic itself -- it only makes sense for the
#        `formula` method, so it lives there, not on every call.
# ✅ [**DONE**] Get variable labels from data or xlab=/ylab= args -- each method derives sensible
#        defaults (deparsed vector expression / data frame column name / formula term name),
#        overridable via xlab=/ylab=.
#
# CRAN-readiness fixes vs. the original:
#
# - No more `require()` inside functions (not CRAN-compliant) -- guarded via requireNamespace()
#   + `pkg::fun()`, matching this package's existing style (see R/color_table.R).
#
# - Dropped the `gridExtra` dependency entirely -- it was require()d but never actually used
#   (all grid calls were from `grid` itself). Histogram mode now maps precomputed counts into
#   the main plot's 0--1 probability coordinates and draws them with geom_rect(). It therefore
#   returns a native ggplot whose labels and theme can be changed after construction.
#
# - Fixed deprecated ggplot2 arg: `geom_smooth(..., size = 1.5, ...)` -> `linewidth = 1.5`
#   (the "points" branch was still using the pre-3.4.0 `size` aesthetic for lines).
#
# - `aes(x = x, y = y)` -> `aes(x = .data$x, y = .data$y)` to avoid an R CMD check NOTE
#   ("no visible binding for global variable").
#
# - Added input validation: `data` must have >= 2 columns (data.frame method), and `y` must be
#   binary (exactly 2 distinct values) -- both silently misbehaved before.
#
# - Removed dead code (`min_y`/`max_y` were computed but never used).
#
# - Renamed loop variables `a`, `b`, `c` (the last of which shadowed base::c()). Histogram
#   mode no longer needs three separately named plots because all layers share one panel.
#
# Kept, per discussion: the *idea* behind the old loghistplot()/logpointplot() single-purpose
# functions is not dropped -- they're reimplemented, and renamed, as thin convenience wrappers
# `logist_hist()` / `logist_point()` that just call `logist_plot(..., marginal = "hist"/
# "points")`, plus `logist_density()` for the density mode, so they inherit all three calling
# conventions (vector/data.frame/formula) for free instead of duplicating the implementation.
# The old names themselves are gone; nothing in this file is still literally called
# loghistplot()/logpointplot().

# ---- review notes ------------------------------------------------------------------------
#
# Package integration / dependencies
#
# - This file is under dev/, so none of its functions or documentation are included in the
#   package. Before release it needs to move to R/, be processed by roxygen2, and have tests.
#
# - Move ggplot2 from Suggests to Imports as ggplot2 (>= 3.4.0). Every execution path requires
#   it, the function returns a ggplot object, and linewidth requires ggplot2 3.4.0 or newer.
#   Keep the qualified ggplot2:: calls; once it is imported, the requireNamespace() guard is
#   unnecessary.
#
# - cowplot is no longer needed by this implementation. Before package integration, check for
#   other production uses before changing DESCRIPTION. .data remains imported via
#   @importFrom rlang .data because it is used unqualified inside aes().
#
# Interface / behavior
#
# - Question for Michael: should logist_plot() require callers to choose "hist" or "points"
#   explicitly? I had it as no default here, while logist_hist() and
#   logist_point() make the choice for their callers. The current method and implementation
#   formals use marginal = c("hist", "points"), so match.arg() selects "hist" when marginal is
#   omitted. If the no-default proposal is adopted, remove the marginal default from all three
#   public methods and from .logist_plot_impl(), and remove "(default)" from @param marginal.
#   ✅ [**RESOLVED** (Michael): keep the "hist" default as-is on logist_plot(); logist_hist()/
#   logist_point() remain convenience wrappers, not the only way to skip specifying it.]
#
# - The documented factor/character/logical response support is not implemented reliably.
#   Histogram mode errors on a discrete y scale, while point mode can draw points but fails
#   one or more glm smooth groups. Convert the two response levels deterministically to 0/1;
#   for numeric y, either require 0/1 or document and perform the same conversion.
#   [**CLARIFIED** (2026-08-08), not yet fixed -- see dev/loghist-test.R for reproducible cases:
#   ggplot2 treats any non-numeric y (factor/character/logical) as *discrete*. aes(y = .data$y)
#   sets no explicit group=, so ggplot2's implicit grouping splits stat_smooth()'s calculation
#   into one glm() fit PER DISTINCT y VALUE -- each such subset has a constant y, which
#   glm(family = binomial) rejects ("y values must be 0 <= y <= 1"). That's the exact failure
#   in "points" mode for all three non-numeric types ("Failed to fit group 2"). "hist" mode
#   fails earlier for factor/character specifically: scale_y_continuous() on a discrete y
#   aesthetic is a hard construction-time error ("Discrete value supplied to a continuous
#   scale"). Logical is a confusing partial exception -- scale_y_continuous() tolerates
#   logical->0/1 coercion, so "hist" + logical doesn't error the same way, but the same
#   implicit-grouping problem is still present in the shared smooth layer; in the v2
#   implementation it was likely swallowed inside cowplot's grob capture rather than genuinely
#   absent, so don't treat "hist" + logical as safe on that result alone. Bottom line: only numeric 0/1 currently
#   works reliably, contradicting the @param y doc. Fix is as Gavin describes above -- convert
#   y to numeric 0/1 immediately after .check_binary_y(), before it ever reaches ggplot() --
#   not yet applied.]
#
#   ✅ [**FIXED** (Michael, 2026-08-08): .check_binary_y() replaced with .to_binary01(), which
#   validates AND canonicalizes in one step (see its own comment for the level-ordering
#   convention per type). .logist_plot_impl() converts data$y to numeric 0/1 immediately after
#   building `data`, before anything reaches ggplot(). Re-run dev/loghist-test.R to confirm --
#   all four y types now render cleanly in the histogram and point modes. This also fixes the row-order
#   dependency in the next bullet below (level order no longer comes from unique()-encounter
#   order), though the separate p_top/p_bottom naming-vs-rendered-direction question there is
#   still open.]
#
#   ✅ [**FIXED** (Michael, 2026-08-08): confirmed via dev/loghist-test.R's
#   .demo_top_bottom_direction() that the logic was already correct -- uy[1]=0 (unreversed
#   scale) really does grow up from the bottom and uy[2]=1 (scale_y_reverse()) really does hang
#   down from the top, matching the intended mirrored-histogram design. Version 2 renamed the
#   component plots by response group; version 3 expresses the same directions directly through
#   each rectangle's ymin/ymax coordinates.]
#
# - Define which response value is the modeled event and use that same ordering for the fit
#   and marginal plots. .check_binary_y() returns unique() order, so reordering rows can swap
#   the two histograms. The p_top/p_bottom names are also opposite the rendered directions:
#   the ordinary scale grows from the bottom and the reversed scale grows from the top.
#   ✅ [**FIXED** (Michael, 2026-08-08): row-order independence already fixed above via
#   .to_binary01(); rectangle construction now uses that canonical 0/1 order directly.]
#
# - Vector and data-frame calls retain incomplete cases, unlike model.frame() in the formula
#   method. NA/Inf x values make histogram setup fail; constant x gives invalid histogram
#   bins. Validate equal lengths and numeric/finite x after applying one consistent NA policy,
#   then either reject a zero-range predictor or provide a defined histogram fallback.
#
#   ✅ [**FIXED** (Michael, 2026-08-08): .logist_plot_impl() now applies one policy for all calling
#   conventions -- complete.cases() + is.finite(x) right after building `data`, and an explicit
#   error if x has zero range (min_x == max_x). model.frame()'s own NA-dropping in the formula
#   method still runs first, but re-filtering already-clean data afterward is a harmless no-op.]
#
# - The formula method silently ignores every predictor after the first because it passes only
#   mf[[2]] and mf[[1]]. Reject formulas that do not contain exactly one response and one
#   predictor. Also decide whether the promised `formula =` spelling should work: currently
#   logist_plot(formula = y ~ x, data = d) fails because the generic requires an argument x.
#
#   ✅ [**FIXED** (Michael, 2026-08-08): logist_plot.formula()'s first argument renamed from `x` to
#   `formula` -- an S3 method's formals don't have to match the generic's names (verified this
#   is legal and matches base R's own boxplot.formula()/lm() convention), so
#   logist_plot(formula = y ~ x, data = d) now works, as does the existing positional form.
#   Also added an explicit ncol(mf) != 2 check, so a multi-predictor formula now errors instead
#   of silently dropping every predictor but the first.]
#
# - Validate xvar and yvar as single existing column names or valid positions before [[ ]]. An
#   unknown name currently fails later with an unrelated differing-row-count message.
#   ✅ [**FIXED** (Michael, 2026-08-08): logist_plot.data.frame() now validates xcol/ycol resolve to
#   an existing column name before subsetting, erroring immediately with a clear message
#   otherwise.]
#
# - The methods accept ... but do not forward or check it, so misspelled arguments are silently
#   ignored. Either document a purpose for ..., pass it onward, or check that it is empty.
#
#   ✅ [**RESOLVED** (Michael, 2026-08-08; extended 2026-08-17): ... is forwarded from all three
#   public methods into .logist_plot_impl(), which calls rlang::check_dots_empty() and errors
#   on anything unconsumed. A flat ... is not routed to ggplot layers. Advanced layer control
#   instead uses the explicitly scoped fit.args and marginal.args lists; fit.color/marginal.color
#   remain convenience arguments, with the scoped lists taking precedence.]
#
# - The shared plot already has coord_cartesian(); the points branch adds a second coordinate system and
#   reports that the first is being replaced on every call. Construct the coordinate once.
#
#   ✅ [**FIXED** (Michael, 2026-08-08): removed coord_cartesian() from the shared base construction;
#   each branch now sets it exactly once (points: xlim+ylim together; hist: xlim, alongside the
#   scale_y_continuous() it already adds). No more "replacing" message.]
#
# Documentation / tests
#
# - The compatibility comment says loghistplot()/logpointplot() were not dropped, but those
#   names are not defined here; the new wrappers are logist_hist()/logist_point(). Clarify that
#   this is a rename, or retain aliases if the old names were ever public.
#
#   ✅ [**FIXED** (Michael, 2026-08-08): reworded the top-of-file comment to say explicitly that the
#   old names are gone and this is a rename, not a preserved alias. The old names were never
#   public (this file has never shipped in R/), so no alias is needed.]
#
# - @seealso is not required for CRAN, but @seealso [vcd::binreg_plot()] would be useful. With
#   no other help topic in this @family, the family tag currently adds no related-page links.
#   ✅ [**FIXED** (Michael, 2026-08-08): added.]
#
# - Add tests for the three interfaces, all marginal modes, the omitted-marginal error,
#   response encodings/event direction, row reordering, NA/Inf/constant x, column selection,
#   formula validation, labels/colors, optional dependencies, and successful plot building.
```

## After this move (2026-08-19)

Subsequent work (grouping, `marginal = "density"`, the `fit.args=`/`marginal.args=` forwarding
design, `marginal.height=`, and the `head()`/`tail()` qualification) is tracked in
`implemented-plans/` (`rect-plan.md`, `grouping.md`, `density.md`, `forwarding.md`) and
`marginal_height.md`, not appended here -- this file is a snapshot of the log as it stood through
the v3/v4 comparison, not a running changelog.
