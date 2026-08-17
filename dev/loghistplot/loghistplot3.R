# Plotting logistic regressions with marginal distributions of Y -- v3
#
# GOAL: `logist_plot()`, a general function for plotting a `glm(y ~ x, family = binomial)`
#       fit for a single quantitative predictor, with a representation of the marginal
#       distribution of cases for which y==0 vs. y==1 (histogram, density, or jittered points).
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
#' @param fit.color color of the fitted logistic curve and its confidence band; default: "steelblue"
#' @param marginal.color color of the marginal representation of `x` within each `y` group
#'   (histogram/density fill, or point color for `marginal = "points"`); default: "orange"
#' @param fit.args named list of graphical arguments for the fitted curve and confidence band.
#'   Values override the defaults established by `fit.color`. The fit remains a binomial GLM,
#'   so `data`, `mapping`, `stat`, `position`, `inherit.aes`, `method`, `formula`, and
#'   `method.args` cannot be replaced.
#' @param marginal.args named list of graphical arguments for the active marginal layer.
#'   Values override the defaults established by `marginal.color`. Valid arguments depend on
#'   `marginal`: point aesthetics and `position` for `"points"`, rectangle aesthetics for
#'   `"hist"`, or ribbon aesthetics for `"density"`. Histogram computation remains controlled
#'   by `bins`; density bandwidth remains controlled by `adjust`.
#' @rdname logist_plot
#' @export
logist_plot.default <- function(x, y, marginal = c("hist", "points", "density"),
                                 bins = 30, adjust = 1, xlab = NULL, ylab = NULL,
                                 fit.color = "steelblue", marginal.color = "orange",
                                 fit.args = list(), marginal.args = list(), ...) {
  xlab <- xlab %||% deparse(substitute(x))
  ylab <- ylab %||% deparse(substitute(y))
  .logist_plot_impl(x, y, marginal = marginal, bins = bins, adjust = adjust,
                     xlab = xlab, ylab = ylab,
                     fit.color = fit.color, marginal.color = marginal.color,
                     fit.args = fit.args, marginal.args = marginal.args, ...)
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
                                    fit.args = list(), marginal.args = list(), ...) {
  if (ncol(x) < 2L) {
    stop("`x` must have at least 2 columns.", call. = FALSE)
  }
  xres <- .resolve_col(x, xvar, "xvar")
  yres <- .resolve_col(x, yvar, "yvar")
  .logist_plot_impl(xres$value, yres$value, marginal = marginal, bins = bins, adjust = adjust,
                     xlab = xlab %||% xres$name, ylab = ylab %||% yres$name,
                     fit.color = fit.color, marginal.color = marginal.color,
                     fit.args = fit.args, marginal.args = marginal.args, ...)
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
                                 fit.args = list(), marginal.args = list(), ...) {
  mf <- stats::model.frame(formula, data = data, na.action = stats::na.pass)
  if (ncol(mf) != 2L) {
    stop("`formula` must have exactly one response and one predictor (y ~ x); found ",
         ncol(mf) - 1L, " predictor(s).", call. = FALSE)
  }
  .logist_plot_impl(mf[[2]], mf[[1]], marginal = marginal, bins = bins, adjust = adjust,
                     xlab = xlab %||% names(mf)[2], ylab = ylab %||% names(mf)[1],
                     fit.color = fit.color, marginal.color = marginal.color,
                     fit.args = fit.args, marginal.args = marginal.args, ...)
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
                               fit.args = list(), marginal.args = list(), ...) {
  rlang::check_dots_empty()
  marginal <- match.arg(marginal)

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

  data <- data.frame(x = x, y = y)
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

  min_x <- min(data$x)
  max_x <- max(data$x)
  if (min_x == max_x) {
    stop("`x` has zero range (all values are identical); cannot fit or bin.", call. = FALSE)
  }

  p_base <- ggplot2::ggplot(data, ggplot2::aes(x = .data$x, y = .data$y)) +
    ggplot2::theme_bw(base_size = 16) +
    ggplot2::theme(panel.grid.major = ggplot2::element_blank(),
                   panel.grid.minor = ggplot2::element_blank(),
                   panel.background = ggplot2::element_blank(),
                   plot.background = ggplot2::element_blank()) +
    ggplot2::labs(y = paste0(ylab, "\n"), x = paste0("\n", xlab))

  fit_layer <- do.call(
    ggplot2::geom_smooth,
    .merge_layer_args(
      list(
        method = "glm", formula = y ~ x,
        method.args = list(family = "binomial"),
        se = TRUE, colour = fit.color, fill = fit.color,
        linewidth = 1.5, alpha = 0.3
      ),
      fit.args
    )
  )

  if (marginal == "points") {
    point_layer <- do.call(
      ggplot2::geom_point,
      .merge_layer_args(
        list(
          colour = marginal.color,
          alpha = 0.5,
          position = ggplot2::position_jitter(w = 0, h = 0.02)
        ),
        marginal.args
      )
    )
    p_base +
      fit_layer +
      point_layer +
      ggplot2::coord_cartesian(xlim = c(min_x, max_x), ylim = c(0, 1))
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
