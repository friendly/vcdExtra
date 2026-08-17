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
# [**DONE**] Combine hist/points into one function via `marginal = c("hist", "points")` (Gavin)
# [**DONE**] Make into a proper, general function with x=, y=, data= -- via S3 methods below
#        (default / data.frame / formula), matching base R's plot()/boxplot() convention.
#        Deliberately dropped `data=` from the generic itself -- it only makes sense for the
#        `formula` method, so it lives there, not on every call.
# [**DONE**] Get variable labels from data or xlab=/ylab= args -- each method derives sensible
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
#   [**RESOLVED** (Michael): keep the "hist" default as-is on logist_plot(); logist_hist()/
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
#   [**FIXED** (Michael, 2026-08-08): .check_binary_y() replaced with .to_binary01(), which
#   validates AND canonicalizes in one step (see its own comment for the level-ordering
#   convention per type). .logist_plot_impl() converts data$y to numeric 0/1 immediately after
#   building `data`, before anything reaches ggplot(). Re-run dev/loghist-test.R to confirm --
#   all four y types now render cleanly in the histogram and point modes. This also fixes the row-order
#   dependency in the next bullet below (level order no longer comes from unique()-encounter
#   order), though the separate p_top/p_bottom naming-vs-rendered-direction question there is
#   still open.]
#
#   [**FIXED** (Michael, 2026-08-08): confirmed via dev/loghist-test.R's
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
#   [**FIXED** (Michael, 2026-08-08): row-order independence already fixed above via
#   .to_binary01(); rectangle construction now uses that canonical 0/1 order directly.]
#
# - Vector and data-frame calls retain incomplete cases, unlike model.frame() in the formula
#   method. NA/Inf x values make histogram setup fail; constant x gives invalid histogram
#   bins. Validate equal lengths and numeric/finite x after applying one consistent NA policy,
#   then either reject a zero-range predictor or provide a defined histogram fallback.
#
#   [**FIXED** (Michael, 2026-08-08): .logist_plot_impl() now applies one policy for all calling
#   conventions -- complete.cases() + is.finite(x) right after building `data`, and an explicit
#   error if x has zero range (min_x == max_x). model.frame()'s own NA-dropping in the formula
#   method still runs first, but re-filtering already-clean data afterward is a harmless no-op.]
#
# - The formula method silently ignores every predictor after the first because it passes only
#   mf[[2]] and mf[[1]]. Reject formulas that do not contain exactly one response and one
#   predictor. Also decide whether the promised `formula =` spelling should work: currently
#   logist_plot(formula = y ~ x, data = d) fails because the generic requires an argument x.
#
#   [**FIXED** (Michael, 2026-08-08): logist_plot.formula()'s first argument renamed from `x` to
#   `formula` -- an S3 method's formals don't have to match the generic's names (verified this
#   is legal and matches base R's own boxplot.formula()/lm() convention), so
#   logist_plot(formula = y ~ x, data = d) now works, as does the existing positional form.
#   Also added an explicit ncol(mf) != 2 check, so a multi-predictor formula now errors instead
#   of silently dropping every predictor but the first.]
#
# - Validate xvar and yvar as single existing column names or valid positions before [[ ]]. An
#   unknown name currently fails later with an unrelated differing-row-count message.
#   [**FIXED** (Michael, 2026-08-08): logist_plot.data.frame() now validates xcol/ycol resolve to
#   an existing column name before subsetting, erroring immediately with a clear message
#   otherwise.]
#
# - The methods accept ... but do not forward or check it, so misspelled arguments are silently
#   ignored. Either document a purpose for ..., pass it onward, or check that it is empty.
#
#   [**RESOLVED** (Michael, 2026-08-08; extended 2026-08-17): ... is forwarded from all three
#   public methods into .logist_plot_impl(), which calls rlang::check_dots_empty() and errors
#   on anything unconsumed. A flat ... is not routed to ggplot layers. Advanced layer control
#   instead uses the explicitly scoped fit.args and marginal.args lists; fit.color/marginal.color
#   remain convenience arguments, with the scoped lists taking precedence.]
#
# - The shared plot already has coord_cartesian(); the points branch adds a second coordinate system and
#   reports that the first is being replaced on every call. Construct the coordinate once.
#
#   [**FIXED** (Michael, 2026-08-08): removed coord_cartesian() from the shared base construction;
#   each branch now sets it exactly once (points: xlim+ylim together; hist: xlim, alongside the
#   scale_y_continuous() it already adds). No more "replacing" message.]
#
# Documentation / tests
#
# - The compatibility comment says loghistplot()/logpointplot() were not dropped, but those
#   names are not defined here; the new wrappers are logist_hist()/logist_point(). Clarify that
#   this is a rename, or retain aliases if the old names were ever public.
#
#   [**FIXED** (Michael, 2026-08-08): reworded the top-of-file comment to say explicitly that the
#   old names are gone and this is a rename, not a preserved alias. The old names were never
#   public (this file has never shipped in R/), so no alias is needed.]
#
# - @seealso is not required for CRAN, but @seealso [vcd::binreg_plot()] would be useful. With
#   no other help topic in this @family, the family tag currently adds no related-page links.
#   [**FIXED** (Michael, 2026-08-08): added.]
#
# - Add tests for the three interfaces, all marginal modes, the omitted-marginal error,
#   response encodings/event direction, row reordering, NA/Inf/constant x, column selection,
#   formula validation, labels/colors, optional dependencies, and successful plot building.

# ---- public generic + methods ------------------------------------------------------------

#' Plot a fitted logistic regression with marginal distributions of the predictor
#'
#' Plots predicted probabilities from a `glm(y ~ x, family = binomial)` fit for a single
#' quantitative predictor `x` and binary response `y`, and also with the smoothed
#' logistic fit and its confidence band.
#' What this plot method adds is a representation of
#' the marginal distribution of `x` within each `y` group -- mirrored histograms or filled
#' density estimates above and below the curve, or jittered points -- as suggested by Smart et
#' al. (2004). These help you
#' see where the data supporting the fit exist; e.g., where the data are "thin", so the confidence band is wide.
#'
#' `logist_plot()` is generic, with methods for a pair of vectors, a data frame, or a
#' model formula. `logist_hist()`, `logist_point()`, and `logist_density()` are convenience
#' wrappers with `marginal=` fixed to `"hist"`/`"points"`/`"density"`, but otherwise accept
#' the same `x`/`...` as `logist_plot()` -- i.e., they also work with a data frame or a formula.
#' An optional `group` produces separate fits and colour identities in point and density
#' modes. Grouped histograms are deliberately unsupported because stacked or overlapping
#' mirrored bars obscure both the distributions and the fitted curves.
#' Grouped density lanes begin at 0 and 1 and stack outward in fixed, narrow bands. Each
#' group's two response-specific densities are normalized together within that band, so the
#' shapes show conditional distributions but do not encode group sample sizes.
#'
#' All marginal modes return a native ggplot object. Standard additions such as
#' [ggplot2::labs()] and [ggplot2::theme()] can therefore be applied after construction.
#' Adding another `scale_y_*()` replaces the internally configured probability scale; in
#' histogram mode this can remove or invalidate the secondary count-axis mapping.
#'
#' @param x a numeric predictor vector or a data frame; see `formula` below for the
#'   model-formula interface
#' @param ... arguments passed to methods, or on to `logist_plot()` from the convenience
#'   wrappers. Arguments not consumed by the selected method are an error rather than being
#'   silently ignored. Use `fit.args` and `marginal.args` for layer customization.
#'
#' @return A native `ggplot` object that can be extended with ordinary ggplot2 additions.
#' @author Gavin Klorfine, Michael Friendly
#'
#' @family logistic regression plots
#'
#' @seealso [vcd::binreg_plot()], a similar plot using `grid` graphics directly.
#'
#' @references
#' Smart, J. M. R., Sutherland, W. J., Watkinson, A. R., and Gill, J. A. (2004). A New Means of
#' Presenting the Results of Logistic Regression, *Bulletin of the Ecological Society of
#' America*, 85(3), 100--102. \doi{10.1890/0012-9623(2004)85[100:ANMOPT]2.0.CO;2}
#' <https://esapubs.org/bulletin/backissues/085-3/bulletinjuly2004_2column.htm#tools1>
#'
#' @examples
#' data(Donner, package = "vcdExtra")
#'
#' # three interfaces to the same underlying plot
#' logist_plot(Donner$age, Donner$survived, marginal = "points")
#' logist_plot(Donner[, c("age", "survived")], marginal = "hist")
#' logist_plot(survived ~ age, data = Donner, marginal = "density")
#'
#' # post-hoc labels and themes work in histogram mode
#' logist_plot(survived ~ age, data = Donner, marginal = "hist") +
#'   ggplot2::labs(title = "Survival of the Donner Party") +
#'   ggplot2::theme(plot.title = ggplot2::element_text(face = "bold"))
#'
#' # layer-specific customization; scoped lists override graphical defaults
#' logist_plot(
#'   survived ~ age, data = Donner, marginal = "hist",
#'   fit.args = list(linewidth = 2, fill = "lightblue"),
#'   marginal.args = list(colour = "black", linewidth = 0.2, alpha = 0.8)
#' )
#'
#' # convenience wrappers -- marginal= fixed, still get all calling conventions
#' logist_point(survived ~ age, data = Donner)
#' logist_hist(survived ~ age, data = Donner)
#' logist_density(survived ~ age, data = Donner, adjust = 1.25)
#'
#' # grouped fits and marginals; grouped density lanes extend outward from 0 and 1
#' logist_point(survived ~ age, data = Donner, group = "sex")
#' logist_density(
#'   survived ~ age, data = Donner, group = "sex",
#'   group.colors = c(Female = "#D55E00", Male = "#0072B2"),
#'   marginal.args = list(alpha = 0.35, linewidth = 0.6)
#' )
#'
#' @importFrom rlang .data
#' @export
logist_plot <- function(x, ...) {
  UseMethod("logist_plot")
}

#' @param y a binary (0/1, or 2-level factor/character/logical) response vector
#' @param marginal character string, how to represent the marginal distribution of `x` within
#'   each `y` group: `"hist"`, mirrored histograms (default); `"points"`, jittered points; or
#'   `"density"`, mirrored filled density estimates
#' @param bins number of histogram bins, for `marginal = "hist"`; default: 30
#' @param adjust positive numeric bandwidth adjustment passed to [stats::density()] for
#'   `marginal = "density"`; default: 1
#' @param xlab,ylab axis labels; default to the deparsed `x`/`y` expressions
#' @param fit.color color of the fitted logistic curve and its confidence band; default:
#'   "steelblue". This scalar is inactive when `group` is supplied; use `group.colors`
#'   instead.
#' @param marginal.color color of the marginal representation of `x` within each `y` group
#'   (histogram/density fill, or point color for `marginal = "points"`); default: "orange"
#'   This scalar is inactive when `group` is supplied; use `group.colors` instead.
#' @param group optional grouping input. For the default method, a vector the same length as
#'   `x` and `y`; for data-frame and formula methods, a single column name or position.
#'   Grouping is supported for `marginal = "points"` and `"density"`, but not `"hist"`.
#' @param group.colors optional character vector of colours for grouped plots. An unnamed
#'   vector is applied in group-level order; a named vector must contain every observed group
#'   label. The same palette is used for fits, marginals, and the legend. The default `NULL`
#'   uses ggplot2's discrete scales.
#' @param fit.args named list of graphical arguments for the fitted curve and confidence band.
#'   Values override the defaults established by `fit.color`. The fit remains a binomial GLM,
#'   so `data`, `mapping`, `stat`, `position`, `inherit.aes`, `method`, `formula`, and
#'   `method.args` cannot be replaced. In grouped mode, `colour` and `fill` must instead be
#'   controlled through `group.colors`.
#' @param marginal.args named list of graphical arguments for the active marginal layer.
#'   Values override the defaults established by `marginal.color`. Valid arguments depend on
#'   `marginal`: point aesthetics and `position` for `"points"`, rectangle aesthetics for
#'   `"hist"`, or ribbon aesthetics for `"density"`. Histogram computation remains controlled
#'   by `bins`; density bandwidth remains controlled by `adjust`. In grouped mode, `colour`
#'   and `fill` must instead be controlled through `group.colors`.
#' @rdname logist_plot
#' @export
logist_plot.default <- function(x, y, marginal = c("hist", "points", "density"),
                                 bins = 30, adjust = 1, xlab = NULL, ylab = NULL,
                                 fit.color = "steelblue", marginal.color = "orange",
                                 fit.args = list(), marginal.args = list(),
                                 group = NULL, group.colors = NULL, ...) {
  xlab <- xlab %||% deparse(substitute(x))
  ylab <- ylab %||% deparse(substitute(y))
  group.label <- if (is.null(group)) NULL else paste(deparse(substitute(group)), collapse = "")
  .logist_plot_impl(x, y, marginal = marginal, bins = bins, adjust = adjust,
                     xlab = xlab, ylab = ylab,
                     fit.color = fit.color, marginal.color = marginal.color,
                     fit.args = fit.args, marginal.args = marginal.args,
                     group = group, group.label = group.label,
                     group.colors = group.colors, ...)
}

#' @param xvar,yvar which columns of `x` to use as predictor/response -- column name or
#'   position; default to the first two columns (matches the original 2-column-data-frame
#'   calling convention)
#' @rdname logist_plot
#' @export
logist_plot.data.frame <- function(x, xvar = 1L, yvar = 2L,
                                    marginal = c("hist", "points", "density"),
                                    bins = 30, adjust = 1, xlab = NULL, ylab = NULL,
                                    fit.color = "steelblue", marginal.color = "orange",
                                    fit.args = list(), marginal.args = list(),
                                    group = NULL, group.colors = NULL, ...) {
  if (ncol(x) < 2L) {
    stop("`x` must have at least 2 columns.", call. = FALSE)
  }
  xres <- .resolve_col(x, xvar, "xvar")
  yres <- .resolve_col(x, yvar, "yvar")
  gres <- if (is.null(group)) NULL else .resolve_col(x, group, "group")
  .logist_plot_impl(xres$value, yres$value, marginal = marginal, bins = bins, adjust = adjust,
                     xlab = xlab %||% xres$name, ylab = ylab %||% yres$name,
                     fit.color = fit.color, marginal.color = marginal.color,
                     fit.args = fit.args, marginal.args = marginal.args,
                     group = if (is.null(gres)) NULL else gres$value,
                     group.label = if (is.null(gres)) NULL else gres$name,
                     group.colors = group.colors, ...)
}

#' @param formula a model formula, `y ~ x` -- exactly one response and one predictor;
#'   `formula` method only. The first argument may be passed positionally or as
#'   `formula = y ~ x` (matching base R's `boxplot()`/`lm()` convention) -- unlike the other
#'   methods, it is not named `x`
#' @param data a data frame -- `formula` method only
#' @rdname logist_plot
#' @export
logist_plot.formula <- function(formula, data, marginal = c("hist", "points", "density"),
                                 bins = 30, adjust = 1, xlab = NULL, ylab = NULL,
                                 fit.color = "steelblue", marginal.color = "orange",
                                 fit.args = list(), marginal.args = list(),
                                 group = NULL, group.colors = NULL, ...) {
  gres <- if (is.null(group)) NULL else .resolve_col(data, group, "group")
  mf <- stats::model.frame(formula, data = data, na.action = stats::na.pass)
  if (ncol(mf) != 2L) {
    stop("`formula` must have exactly one response and one predictor (y ~ x); found ",
         ncol(mf) - 1L, " predictor(s).", call. = FALSE)
  }
  .logist_plot_impl(mf[[2]], mf[[1]], marginal = marginal, bins = bins, adjust = adjust,
                     xlab = xlab %||% names(mf)[2], ylab = ylab %||% names(mf)[1],
                     fit.color = fit.color, marginal.color = marginal.color,
                     fit.args = fit.args, marginal.args = marginal.args,
                     group = if (is.null(gres)) NULL else gres$value,
                     group.label = if (is.null(gres)) NULL else gres$name,
                     group.colors = group.colors, ...)
}

# ---- convenience wrappers (fixed marginal=) ------------------------------------------------

#' @rdname logist_plot
#' @export
logist_hist <- function(...) {
  logist_plot(..., marginal = "hist")
}

#' @rdname logist_plot
#' @export
logist_point <- function(...) {
  logist_plot(..., marginal = "points")
}

#' @rdname logist_plot
#' @export
logist_density <- function(...) {
  logist_plot(..., marginal = "density")
}

# if (FALSE) {
#   data(Donner, package = "vcdExtra")
#
#   # three interfaces to the general function
#   logist_plot(Donner$age, Donner$survived, marginal = "hist")
#   logist_plot(Donner[, c("age", "survived")], marginal = "points")
#   logist_plot(survived ~ age, data = Donner, marginal = "hist")
#
#   # xvar=/yvar= to pick columns by name from a wider data frame
#   logist_plot(Donner, xvar = "age", yvar = "survived", marginal = "hist")
#
#   # convenience wrappers -- marginal= fixed, still get all 3 calling conventions
#   logist_hist(survived ~ age, data = Donner)
#   logist_point(survived ~ age, data = Donner)
#   logist_density(survived ~ age, data = Donner, adjust = 1.25)
#   logist_point(Donner[, c("age", "survived")])
# }

# ---- internal helpers (not exported) ----------------------------------------------------

`%||%` <- function(a, b) if (is.null(a)) b else a

# Resolve a xvar=/yvar= selector to a single column. Numeric positions are extracted directly
# by position (never converted to a name and re-looked-up), so this still works correctly when
# `x` has duplicate column names. Character selectors must match exactly one column name.
.resolve_col <- function(x, var, argname) {
  if (length(var) != 1L || is.na(var)) {
    stop("`", argname, "` must be a single column name or position.", call. = FALSE)
  }
  if (is.character(var)) {
    idx <- which(names(x) == var)
    if (length(idx) == 0L) {
      stop("`", argname, "` does not identify an existing column of `x`.", call. = FALSE)
    }
    if (length(idx) > 1L) {
      stop("`", argname, "` (\"", var, "\") matches more than one column of `x`; ",
           "column names must be unique for name-based selection.", call. = FALSE)
    }
    list(value = x[[idx]], name = var)
  } else if (is.numeric(var)) {
    if (var != floor(var) || var < 1L || var > ncol(x)) {
      stop("`", argname, "` must be a whole number between 1 and ", ncol(x), ".", call. = FALSE)
    }
    idx <- as.integer(var)
    list(value = x[[idx]], name = names(x)[idx])
  } else {
    stop("`", argname, "` must be a single column name (character) or position (integer).",
         call. = FALSE)
  }
}

.check_bins <- function(bins) {
  if (length(bins) != 1L || !is.numeric(bins) || is.na(bins) ||
      !is.finite(bins) || bins < 1 || bins != floor(bins)) {
    stop("`bins` must be one positive whole number.", call. = FALSE)
  }
}

.check_adjust <- function(adjust) {
  if (length(adjust) != 1L || !is.numeric(adjust) || is.na(adjust) ||
      !is.finite(adjust) || adjust <= 0) {
    stop("`adjust` must be one finite positive number.", call. = FALSE)
  }
}

# Convert a supported discrete grouping vector to a factor with deterministic levels.
# Existing factor order is meaningful and therefore retained; other supported types are
# sorted independently of row order so colours, legends, and density lanes remain stable.
.as_group_factor <- function(group) {
  if (!is.null(dim(group))) {
    stop("`group` must be a plain vector, not a matrix/array/data.frame.", call. = FALSE)
  }
  if (is.list(group)) {
    stop("`group` must be a plain atomic vector, not a list.", call. = FALSE)
  }
  if (!(is.factor(group) || is.character(group) || is.logical(group) || is.numeric(group))) {
    stop("`group` must be numeric, logical, factor, or character; found class \"",
         paste(class(group), collapse = "/"), "\".", call. = FALSE)
  }

  observed <- group[!is.na(group)]
  levs <- if (is.factor(group)) {
    levels(group)[levels(group) %in% as.character(observed)]
  } else if (is.logical(group)) {
    c(FALSE, TRUE)[c(FALSE, TRUE) %in% observed]
  } else if (is.numeric(group)) {
    sort(unique(observed))
  } else {
    sort(unique(observed), method = "radix")
  }
  factor(as.character(group), levels = as.character(levs))
}

# Validate and order an optional manual palette against the observed group levels.
.check_group_colors <- function(group.colors, group.levels) {
  if (is.null(group.colors)) {
    return(NULL)
  }
  if (!is.character(group.colors) || !length(group.colors) ||
      anyNA(group.colors) || any(group.colors == "")) {
    stop("`group.colors` must be a non-empty character vector of valid colours.",
         call. = FALSE)
  }

  color_names <- names(group.colors)
  if (!is.null(color_names) && any(nzchar(color_names))) {
    if (anyNA(color_names) || any(color_names == "") || anyDuplicated(color_names)) {
      stop("Named `group.colors` must have one unique, non-empty name per colour.",
           call. = FALSE)
    }
    missing_groups <- setdiff(group.levels, color_names)
    if (length(missing_groups)) {
      stop("Named `group.colors` is missing colour(s) for group(s): ",
           paste(missing_groups, collapse = ", "), ".", call. = FALSE)
    }
    group.colors <- group.colors[group.levels]
  } else {
    if (length(group.colors) < length(group.levels)) {
      stop("`group.colors` must provide at least ", length(group.levels),
           " colours, one for each observed group.", call. = FALSE)
    }
    group.colors <- group.colors[seq_along(group.levels)]
    names(group.colors) <- group.levels
  }

  valid <- vapply(group.colors, function(col) {
    tryCatch({
      grDevices::col2rgb(col)
      TRUE
    }, error = function(e) FALSE)
  }, logical(1))
  if (!all(valid)) {
    stop("Invalid colour value(s) in `group.colors`: ",
         paste(group.colors[!valid], collapse = ", "), ".", call. = FALSE)
  }
  group.colors
}

# Apply a shared colour identity and legend title to every grouped layer.
.add_group_scales <- function(plot, group.label, group.levels, group.colors) {
  if (is.null(group.colors)) {
    plot + ggplot2::labs(colour = group.label, fill = group.label)
  } else {
    plot +
      ggplot2::scale_colour_manual(
        values = group.colors,
        breaks = group.levels, limits = group.levels, drop = FALSE
      ) +
      ggplot2::scale_fill_manual(
        values = group.colors,
        breaks = group.levels, limits = group.levels, drop = FALSE
      ) +
      ggplot2::labs(colour = group.label, fill = group.label)
  }
}

# Validate an explicitly scoped ggplot layer-argument list. Structural arguments are kept
# under logist_plot()'s control; the per-layer allowlist makes misspellings fail eagerly
# instead of surfacing later as a ggplot warning during rendering.
.check_layer_args <- function(args, argname, allowed, protected, layer) {
  if (!is.list(args)) {
    stop("`", argname, "` must be a named list.", call. = FALSE)
  }
  if (length(args) == 0L) {
    return(args)
  }

  nms <- names(args)
  if (is.null(nms) || anyNA(nms) || any(nms == "")) {
    stop("Every element of `", argname, "` must be named.", call. = FALSE)
  }
  dup <- unique(nms[duplicated(nms)])
  if (length(dup)) {
    stop("`", argname, "` contains duplicate argument name(s): ",
         paste(dup, collapse = ", "), ".", call. = FALSE)
  }

  # ggplot2 accepts both spellings. Canonicalizing before the merge gives them one
  # deterministic precedence slot instead of potentially passing both to a layer.
  if ("color" %in% nms && "colour" %in% nms) {
    if (!identical(args[["color"]], args[["colour"]])) {
      stop("`", argname, "` supplies conflicting `color` and `colour` values.",
           call. = FALSE)
    }
    args[["color"]] <- NULL
  } else if ("color" %in% nms) {
    names(args)[names(args) == "color"] <- "colour"
  }

  blocked <- intersect(names(args), protected)
  if (length(blocked)) {
    stop("`", argname, "` cannot replace protected ", layer, " argument(s): ",
         paste(blocked, collapse = ", "), ".", call. = FALSE)
  }
  unknown <- setdiff(names(args), allowed)
  if (length(unknown)) {
    stop("Unsupported argument(s) in `", argname, "` for ", layer, ": ",
         paste(unknown, collapse = ", "), ".", call. = FALSE)
  }
  args
}

# Merge named layer arguments with user values taking precedence over defaults.
.merge_layer_args <- function(defaults, user) {
  defaults[names(defaults) %in% names(user)] <- NULL
  c(defaults, user)
}

# Validate and canonicalize a binary response to numeric 0/1, so ggplot2 never sees a
# factor/character/logical y -- which it would treat as *discrete*, silently triggering
# per-level grouping that breaks geom_smooth()'s glm() fit (see dev/loghist-test.R for the
# exact failures this avoids). Which value becomes 1 (the modeled "event") is deterministic,
# not dependent on row order:
#   logical:   FALSE -> 0, TRUE -> 1
#   factor:    the two observed levels, in their existing `levels()` order
#   character: the two observed values, sorted alphabetically (lower -> 0, higher -> 1) --
#              the same convention R itself uses for a factor's default (alphabetical) levels
#   numeric:   must already be coded 0/1; any other numeric coding is rejected rather than
#              guessed at
.to_binary01 <- function(y) {
  if (!is.null(dim(y))) {
    stop("`y` must be a plain vector, not a matrix/array (e.g. not `cbind(success, failure)`).",
         call. = FALSE)
  }
  if (!(is.logical(y) || is.factor(y) || is.character(y) || is.numeric(y))) {
    stop("`y` must be numeric, logical, factor, or character; found class \"",
         paste(class(y), collapse = "/"), "\".", call. = FALSE)
  }
  uy <- unique(y[!is.na(y)])
  if (length(uy) != 2L) {
    stop("`y` must be binary (exactly 2 distinct values); found ", length(uy), ".",
         call. = FALSE)
  }
  levs <- if (is.logical(y)) {
    c(FALSE, TRUE)
  } else if (is.factor(y)) {
    levels(y)[levels(y) %in% as.character(uy)]
  } else if (is.numeric(y)) {
    if (!setequal(uy, c(0, 1))) {
      stop("numeric `y` must be coded 0/1; found ", paste(sort(uy), collapse = ", "), ".",
           call. = FALSE)
    }
    c(0, 1)
  } else {
    # method = "radix" sorts by C-locale byte order, independent of the session locale --
    # otherwise the modeled "event" (which level maps to 1) could differ across systems.
    sort(as.character(uy), method = "radix")
  }
  list(y01 = as.numeric(factor(as.character(y), levels = as.character(levs))) - 1,
       levels = levs)
}

# The one real implementation, shared by all logist_plot() methods and by
# logist_hist()/logist_point()/logist_density().
.logist_plot_impl <- function(x, y, marginal = c("hist", "points", "density"),
                               bins = 30, adjust = 1, xlab = NULL, ylab = NULL,
                               fit.color = "steelblue", marginal.color = "orange",
                               fit.args = list(), marginal.args = list(),
                               group = NULL, group.label = NULL,
                               group.colors = NULL, ...) {
  rlang::check_dots_empty()
  marginal <- match.arg(marginal)
  grouped <- !is.null(group)
  if (grouped && marginal == "hist") {
    stop("Grouping is not supported for `marginal = \"hist\"`; use ",
         "`marginal = \"points\"` or `marginal = \"density\"`.", call. = FALSE)
  }
  if (!grouped && !is.null(group.colors)) {
    stop("`group.colors` requires a non-NULL `group`.", call. = FALSE)
  }

  fit.args <- .check_layer_args(
    fit.args,
    argname = "fit.args",
    allowed = c("se", "n", "span", "fullrange", "level", "na.rm", "orientation",
                "show.legend", "colour", "fill", "linewidth", "linetype", "alpha"),
    protected = c("data", "mapping", "stat", "position", "inherit.aes",
                  "method", "formula", "method.args"),
    layer = "fitted-curve layer"
  )
  marginal.args <- if (marginal == "points") {
    .check_layer_args(
      marginal.args,
      argname = "marginal.args",
      allowed = c("position", "na.rm", "show.legend", "colour", "fill", "alpha",
                  "shape", "size", "stroke"),
      protected = c("data", "mapping", "stat", "inherit.aes"),
      layer = '`marginal = "points"` layer'
    )
  } else if (marginal == "hist") {
    .check_layer_args(
      marginal.args,
      argname = "marginal.args",
      allowed = c("na.rm", "show.legend", "colour", "fill", "alpha", "linetype",
                  "linewidth", "lineend", "linejoin"),
      protected = c("data", "mapping", "stat", "position", "inherit.aes",
                    "binwidth", "boundary", "closed"),
      layer = '`marginal = "hist"` rectangle layer'
    )
  } else {
    if (is.list(marginal.args) && "adjust" %in% names(marginal.args)) {
      stop("Supply `adjust` as a top-level argument, not inside `marginal.args`.",
           call. = FALSE)
    }
    .check_layer_args(
      marginal.args,
      argname = "marginal.args",
      allowed = c("na.rm", "show.legend", "colour", "fill", "alpha", "linetype",
                  "linewidth", "lineend", "linejoin", "linemitre", "outline.type"),
      protected = c("data", "mapping", "stat", "position", "orientation", "inherit.aes",
                    "adjust"),
      layer = '`marginal = "density"` ribbon layers'
    )
  }

  if (grouped) {
    fit_color_args <- intersect(names(fit.args), c("colour", "fill"))
    marginal_color_args <- intersect(names(marginal.args), c("colour", "fill"))
    if (length(fit_color_args)) {
      stop("Grouped plots map fit colours to `group`; supply colours through ",
           "`group.colors`, not `fit.args`.", call. = FALSE)
    }
    if (length(marginal_color_args)) {
      stop("Grouped plots map marginal colours to `group`; supply colours through ",
           "`group.colors`, not `marginal.args`.", call. = FALSE)
    }
  }

  if (!is.null(dim(x))) {
    stop("`x` must be a plain vector, not a matrix/array/data.frame (e.g. a multi-column ",
         "formula term such as poly(x, 2) is not supported).", call. = FALSE)
  }
  if (!is.null(dim(y))) {
    stop("`y` must be a plain vector, not a matrix/array/data.frame (e.g. not ",
         "`cbind(success, failure)`).", call. = FALSE)
  }
  if (is.list(x)) {
    stop("`x` must be a plain atomic vector, not a list.", call. = FALSE)
  }
  if (is.list(y)) {
    stop("`y` must be a plain atomic vector, not a list.", call. = FALSE)
  }
  if (length(x) != length(y)) {
    stop("`x` and `y` must be the same length; found ", length(x), " and ", length(y), ".",
         call. = FALSE)
  }
  if (!is.numeric(x)) {
    stop("`x` must be numeric; found class \"", paste(class(x), collapse = "/"), "\".",
         call. = FALSE)
  }

  if (grouped) {
    if (!is.null(dim(group))) {
      stop("`group` must be a plain vector, not a matrix/array/data.frame.", call. = FALSE)
    }
    if (is.list(group)) {
      stop("`group` must be a plain atomic vector, not a list.", call. = FALSE)
    }
    if (length(group) != length(x)) {
      stop("`group` must have the same length as `x` and `y`; found ", length(group),
           " and ", length(x), ".", call. = FALSE)
    }
  }

  data <- if (grouped) {
    data.frame(x = x, y = y, group = group)
  } else {
    data.frame(x = x, y = y)
  }
  data <- data[stats::complete.cases(data) & is.finite(data$x), , drop = FALSE]
  if (nrow(data) == 0L) {
    stop("No complete observations remain after removing missing/non-finite `x` values.",
         call. = FALSE)
  }
  bin <- .to_binary01(data$y)
  data$y <- bin$y01
  uy <- c(0, 1)
  xlab <- xlab %||% "x"
  ylab <- ylab %||% "y"

  group.levels <- NULL
  if (grouped) {
    data$group <- .as_group_factor(data$group)
    group.levels <- levels(data$group)
    if (length(group.levels) < 2L) {
      stop("`group` must contain at least 2 observed groups after filtering; found ",
           length(group.levels), ".", call. = FALSE)
    }
    if (is.null(group.label) || length(group.label) != 1L ||
        is.na(group.label) || !nzchar(group.label)) {
      group.label <- "group"
    }
    group.colors <- .check_group_colors(group.colors, group.levels)

    group_has_both_y <- vapply(group.levels, function(lev) {
      setequal(unique(data$y[data$group == lev]), uy)
    }, logical(1))
    if (!all(group_has_both_y)) {
      stop("Every group must contain both response outcomes; missing an outcome in group(s): ",
           paste(group.levels[!group_has_both_y], collapse = ", "), ".", call. = FALSE)
    }
    group_has_x_range <- vapply(group.levels, function(lev) {
      length(unique(data$x[data$group == lev])) >= 2L
    }, logical(1))
    if (!all(group_has_x_range)) {
      stop("Every group must contain at least 2 distinct predictor values; insufficient ",
           "variation in group(s): ", paste(group.levels[!group_has_x_range], collapse = ", "),
           ".", call. = FALSE)
    }
  }

  min_x <- min(data$x)
  max_x <- max(data$x)
  if (min_x == max_x) {
    stop("`x` has zero range (all values are identical); cannot fit or bin.", call. = FALSE)
  }

  base_mapping <- if (grouped) {
    ggplot2::aes(
      x = .data$x, y = .data$y,
      colour = .data$group, fill = .data$group, group = .data$group
    )
  } else {
    ggplot2::aes(x = .data$x, y = .data$y)
  }
  p_base <- ggplot2::ggplot(data, base_mapping) +
    ggplot2::theme_bw(base_size = 16) +
    ggplot2::theme(panel.grid.major = ggplot2::element_blank(),
                   panel.grid.minor = ggplot2::element_blank(),
                   panel.background = ggplot2::element_blank(),
                   plot.background = ggplot2::element_blank()) +
    ggplot2::labs(y = paste0(ylab, "\n"), x = paste0("\n", xlab))

  fit_defaults <- list(
    method = "glm", formula = y ~ x,
    method.args = list(family = "binomial"),
    se = TRUE, linewidth = 1.5, alpha = 0.3
  )
  if (!grouped) {
    fit_defaults$colour <- fit.color
    fit_defaults$fill <- fit.color
  }
  fit_layer <- do.call(
    ggplot2::geom_smooth,
    .merge_layer_args(
      fit_defaults,
      fit.args
    )
  )

  if (marginal == "points") {
    point_defaults <- list(
      alpha = 0.5,
      position = ggplot2::position_jitter(w = 0, h = 0.02)
    )
    if (!grouped) {
      point_defaults$colour <- marginal.color
    }
    point_layer <- do.call(
      ggplot2::geom_point,
      .merge_layer_args(
        point_defaults,
        marginal.args
      )
    )
    point_plot <- p_base +
      fit_layer +
      point_layer +
      ggplot2::coord_cartesian(xlim = c(min_x, max_x), ylim = c(0, 1))
    if (grouped) {
      point_plot <- .add_group_scales(
        point_plot, group.label, group.levels, group.colors
      )
    }
    point_plot
  } else if (marginal == "hist") {
    .check_bins(bins)

    bin_width <- (max_x - min_x) / bins
    if (!is.finite(bin_width) || bin_width <= 0) {
      stop("Cannot compute histogram bins for `x`: the range of `x` (", min_x, " to ", max_x,
           ") produces a non-finite or non-positive bin width. Try fewer `bins` or check for ",
           "extreme values.", call. = FALSE)
    }
    hist_breaks <- seq(min_x, max_x, length.out = bins + 1)
    if (length(unique(hist_breaks)) != length(hist_breaks)) {
      stop("Cannot compute histogram bins for `x`: the range of `x` is too small relative to ",
           "`bins` (", bins, ") to produce distinct break points. Try fewer `bins`.",
           call. = FALSE)
    }
    hist_counts <- lapply(uy, function(lev) {
      graphics::hist(data$x[data$y == lev], breaks = hist_breaks, right = FALSE,
                      include.lowest = TRUE, plot = FALSE)$counts
    })
    max_count <- max(unlist(hist_counts))
    bin_no <- 4 * max_count

    # pretty() can return fractional ticks when max_count is small (e.g. 0, 0.2, 0.4, ... for
    # max_count = 1); counts are always whole numbers, so round + dedupe, then guarantee at
    # least two distinct ticks (0 and max_count) rather than let rounding collapse them.
    count_ticks <- pretty(c(0, max_count))
    count_ticks <- unique(round(count_ticks[count_ticks >= 0 & count_ticks <= max_count]))
    if (length(count_ticks) < 2L) {
      count_ticks <- unique(c(0L, max_count))
    }
    count_positions <- sort(c(count_ticks / bin_no, 1 - count_ticks / bin_no))
    count_labels <- round(bin_no * pmin(count_positions, 1 - count_positions))

    # Draw both response-specific histograms in the probability coordinate system. Each row
    # describes one bin rectangle: y = 0 grows up from 0; y = 1 hangs down from 1. Since
    # bin_no is four times the largest count, neither side can occupy more than one quarter
    # of the panel. Keeping zero-count rows makes the bin/count correspondence explicit.
    hist_data <- rbind(
      data.frame(
        xmin = head(hist_breaks, -1L),
        xmax = tail(hist_breaks, -1L),
        ymin = 0,
        ymax = hist_counts[[1L]] / bin_no
      ),
      data.frame(
        xmin = head(hist_breaks, -1L),
        xmax = tail(hist_breaks, -1L),
        ymin = 1 - hist_counts[[2L]] / bin_no,
        ymax = 1
      )
    )

    rect_layer <- do.call(
      ggplot2::geom_rect,
      .merge_layer_args(
        list(
          data = hist_data,
          mapping = ggplot2::aes(
            xmin = .data$xmin, xmax = .data$xmax,
            ymin = .data$ymin, ymax = .data$ymax
          ),
          inherit.aes = FALSE,
          fill = marginal.color,
          alpha = 0.67
        ),
        marginal.args
      )
    )

    p_base +
      rect_layer +
      # Draw the fitted curve last so its line and confidence band remain visible over the bars.
      fit_layer +
      ggplot2::scale_y_continuous(
        limits = c(0, 1),
        breaks = seq(0, 1, by = 0.2),
        expand = ggplot2::expansion(mult = 0),
        sec.axis = ggplot2::dup_axis(breaks = count_positions, labels = count_labels, name = "Count")
      ) +
      ggplot2::coord_cartesian(xlim = c(min_x, max_x))
  } else {
    .check_adjust(adjust)

    if (grouped) {
      density_lane_height <- 0.05
      # The outermost density edge reaches the nominal y limit exactly. Reserve a small
      # display-only margin so geom_ribbon()'s centered outline stroke is not clipped by the
      # panel border; this does not change the density estimates or their 0.05 lane height.
      density_outline_padding <- density_lane_height * 0.15

      estimate_group_density <- function(group_level, response) {
        cell_x <- data$x[data$group == group_level & data$y == response]
        original_level <- bin$levels[response + 1L]
        if (length(cell_x) < 2L) {
          stop("Cannot estimate the marginal density for group ",
               dQuote(as.character(group_level)), " and response level ",
               dQuote(as.character(original_level)),
               ": at least 2 observations are required; found ", length(cell_x), ".",
               call. = FALSE)
        }
        estimate <- tryCatch(
          stats::density(cell_x, from = min_x, to = max_x, adjust = adjust),
          error = function(e) {
            stop("Cannot estimate the marginal density for group ",
                 dQuote(as.character(group_level)), " and response level ",
                 dQuote(as.character(original_level)), ": ", conditionMessage(e),
                 call. = FALSE)
          }
        )
        if (!all(is.finite(estimate$x)) || !all(is.finite(estimate$y))) {
          stop("Density estimation produced non-finite values for group ",
               dQuote(as.character(group_level)), " and response level ",
               dQuote(as.character(original_level)), ".", call. = FALSE)
        }
        if (any(estimate$y < 0)) {
          stop("Density estimation produced negative values for group ",
               dQuote(as.character(group_level)), " and response level ",
               dQuote(as.character(original_level)), ".", call. = FALSE)
        }
        estimate
      }

      density_pairs <- lapply(group.levels, function(group_level) {
        pair <- lapply(uy, function(response) {
          estimate_group_density(group_level, response)
        })
        pair_max <- max(vapply(pair, function(z) max(z$y), numeric(1)))
        if (!is.finite(pair_max) || pair_max <= 0) {
          stop("Density estimation produced a non-finite or non-positive maximum density ",
               "for group ", dQuote(as.character(group_level)), ".", call. = FALSE)
        }
        list(estimates = pair, headroom = pair_max / density_lane_height)
      })
      density_limits <- c(
        -length(group.levels) * density_lane_height,
        1 + length(group.levels) * density_lane_height
      )

      make_group_density_data <- function(group_index, response) {
        pair <- density_pairs[[group_index]]
        estimate <- pair$estimates[[response + 1L]]
        # Clamp at the nominal lane height to prevent floating-point overshoot at the
        # highest peak from being removed by the exact outer y-scale limit.
        scaled_density <- pmin(estimate$y / pair$headroom, density_lane_height)
        lane_index <- group_index - 1L
        if (response == 0) {
          baseline <- -lane_index * density_lane_height
          ymin <- pmax(baseline - scaled_density, density_limits[1L])
          ymax <- baseline
        } else {
          baseline <- 1 + lane_index * density_lane_height
          ymin <- baseline
          ymax <- pmin(baseline + scaled_density, density_limits[2L])
        }
        data.frame(
          x = estimate$x,
          ymin = ymin,
          ymax = ymax,
          group = factor(
            rep(group.levels[group_index], length(estimate$x)),
            levels = group.levels
          )
        )
      }

      density_y0 <- do.call(rbind, lapply(seq_along(group.levels), function(i) {
        make_group_density_data(i, 0)
      }))
      density_y1 <- do.call(rbind, lapply(seq_along(group.levels), function(i) {
        make_group_density_data(i, 1)
      }))

      grouped_density_mapping <- ggplot2::aes(
        x = .data$x,
        ymin = .data$ymin,
        ymax = .data$ymax,
        fill = .data$group,
        colour = .data$group,
        group = .data$group
      )
      grouped_density_layer <- function(layer_data, outline_type) {
        do.call(
          ggplot2::geom_ribbon,
          .merge_layer_args(
            list(
              data = layer_data,
              mapping = grouped_density_mapping,
              inherit.aes = FALSE,
              alpha = 0.35,
              linewidth = 0.5,
              outline.type = outline_type
            ),
            marginal.args
          )
        )
      }

      density_plot <- p_base +
        grouped_density_layer(density_y0, "lower") +
        grouped_density_layer(density_y1, "upper") +
        fit_layer +
        ggplot2::scale_y_continuous(
          limits = density_limits,
          breaks = seq(0, 1, by = 0.2),
          expand = ggplot2::expansion(add = density_outline_padding)
        ) +
        ggplot2::coord_cartesian(xlim = c(min_x, max_x))

      .add_group_scales(density_plot, group.label, group.levels, group.colors)
    } else {

    densities <- lapply(uy, function(lev) {
      group_x <- data$x[data$y == lev]
      original_level <- bin$levels[lev + 1L]
      if (length(group_x) < 2L) {
        stop("Cannot estimate the marginal density for response level ",
             dQuote(as.character(original_level)), ": at least 2 observations are required; ",
             "found ", length(group_x), ".", call. = FALSE)
      }
      tryCatch(
        stats::density(group_x, from = min_x, to = max_x, adjust = adjust),
        error = function(e) {
          stop("Cannot estimate the marginal density for response level ",
               dQuote(as.character(original_level)), ": ", conditionMessage(e),
               call. = FALSE)
        }
      )
    })

    density_is_finite <- vapply(
      densities,
      function(z) all(is.finite(z$x)) && all(is.finite(z$y)),
      logical(1)
    )
    if (!all(density_is_finite)) {
      bad <- which(!density_is_finite)[1L]
      stop("Density estimation produced non-finite values for response level ",
           dQuote(as.character(bin$levels[bad])), ".", call. = FALSE)
    }
    if (any(vapply(densities, function(z) any(z$y < 0), logical(1)))) {
      stop("Density estimation produced negative values; cannot map the marginal density ",
           "into probability coordinates.", call. = FALSE)
    }

    max_density <- max(vapply(densities, function(z) max(z$y), numeric(1)))
    # Continuous filled ribbons are visually heavier than histogram bars, so cap the tallest
    # density at 15% of the panel on each side (histograms retain their 25% cap).
    density_height <- 0.15
    density_headroom <- max_density / density_height
    if (!is.finite(density_headroom) || density_headroom <= 0) {
      stop("Density estimation produced a non-finite or non-positive maximum density.",
           call. = FALSE)
    }

    density_y0 <- data.frame(
      x = densities[[1L]]$x,
      ymin = 0,
      ymax = densities[[1L]]$y / density_headroom
    )
    density_y1 <- data.frame(
      x = densities[[2L]]$x,
      ymin = 1 - densities[[2L]]$y / density_headroom,
      ymax = 1
    )

    density_mapping <- ggplot2::aes(
      x = .data$x,
      ymin = .data$ymin,
      ymax = .data$ymax
    )
    density_layer <- function(layer_data, outline_type) {
      do.call(
        ggplot2::geom_ribbon,
        .merge_layer_args(
          list(
            data = layer_data,
            mapping = density_mapping,
            inherit.aes = FALSE,
            fill = marginal.color,
            colour = NA,
            alpha = 0.67,
            outline.type = outline_type
          ),
          marginal.args
        )
      )
    }

    density_y0_layer <- density_layer(density_y0, "upper")
    density_y1_layer <- density_layer(density_y1, "lower")

    p_base +
      density_y0_layer +
      density_y1_layer +
      # As in histogram mode, keep the fit visible above the filled marginal layers.
      fit_layer +
      ggplot2::scale_y_continuous(
        limits = c(0, 1),
        breaks = seq(0, 1, by = 0.2),
        expand = ggplot2::expansion(mult = 0)
      ) +
      ggplot2::coord_cartesian(xlim = c(min_x, max_x))
    }
  }
}
