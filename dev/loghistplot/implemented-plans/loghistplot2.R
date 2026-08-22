# Plotting logistic regressions with marginal distributions of Y -- v2
#
# GOAL: `logist_plot()`, a general function for plotting a `glm(y ~ x, family = binomial)`
#       fit for a single quantitative predictor, with a representation of the marginal
#       distribution of cases for which y==0 vs. y==1 (histogram or jittered points).
#
# This is a rewrite of dev/loghistplot.R, addressing its open TODOs and CRAN-readiness
# issues found on review (2026-08-08). The original file is left untouched for reference.
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
#   (all grid calls were from `grid` itself). The marginal="hist" compositing (previously raw
#   grid::viewport overlays, drawn as a side effect, returning invisible(NULL)) is now done with
#   cowplot::ggdraw() + draw_plot(), which returns an actual ggplot object -- verified against the
#   Donner data to render identically to the original grid-viewport version.
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
# - Renamed loop variables `a`, `b`, `c` (the last of which shadowed base::c()) to `p_main`,
#   `p_hist_y0`, `p_hist_y1` (originally `p_top`/`p_bottom`; renamed again, see review notes
#   below, once those names turned out to be backwards from what they render).
#
# Kept, per discussion: the *idea* behind the old loghistplot()/logpointplot() single-purpose
# functions is not dropped -- they're reimplemented, and renamed, as thin convenience wrappers
# `logist_hist()` / `logist_point()` that just call `logist_plot(..., marginal = "hist"/
# "points")`, so they inherit all three calling conventions (vector/data.frame/formula) for
# free instead of duplicating the implementation. The old names themselves are gone; nothing
# in this file is still literally called loghistplot()/logpointplot().

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
# - Question for Michael: should cowplot be in Suggests or Imports? It is required only when a
#   caller explicitly selects marginal = "hist", while marginal = "points" works without it.
#   Use Suggests and keep the requireNamespace() guard if histogram mode is optional; use
#   Imports and remove the guard if histogram mode should always be available. Either way,
#   cowplot must be declared in DESCRIPTION or R CMD check reports a WARNING.
#   [**RESOLVED** (Michael): both ggplot2 (>= 3.4.0) and cowplot moved to Imports in DESCRIPTION;
#   both requireNamespace() guards removed below. .data now imported via
#   @importFrom rlang .data, since it's used unqualified inside aes().]
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
#   implicit-grouping problem is still present in the shared smooth layer; it's very likely
#   being swallowed inside cowplot's grob capture rather than genuinely absent, so don't treat
#   "hist" + logical as safe on that result alone. Bottom line: only numeric 0/1 currently
#   works reliably, contradicting the @param y doc. Fix is as Gavin describes above -- convert
#   y to numeric 0/1 immediately after .check_binary_y(), before it ever reaches ggplot() --
#   not yet applied.]
#
#   [**FIXED** (Michael, 2026-08-08): .check_binary_y() replaced with .to_binary01(), which
#   validates AND canonicalizes in one step (see its own comment for the level-ordering
#   convention per type). .logist_plot_impl() converts data$y to numeric 0/1 immediately after
#   building `data`, before anything reaches ggplot(). Re-run dev/loghist-test.R to confirm --
#   all four y types now render cleanly in both marginal modes. This also fixes the row-order
#   dependency in the next bullet below (level order no longer comes from unique()-encounter
#   order), though the separate p_top/p_bottom naming-vs-rendered-direction question there is
#   still open.]
#
#   [**FIXED** (Michael, 2026-08-08): confirmed via dev/loghist-test.R's
#   .demo_top_bottom_direction() that the logic was already correct -- uy[1]=0 (unreversed
#   scale) really does grow up from the bottom and uy[2]=1 (scale_y_reverse()) really does hang
#   down from the top, matching the intended mirrored-histogram design. Only the variable names
#   were backwards. Renamed p_top/p_bottom -> p_hist_y0/p_hist_y1, naming by response group
#   instead of assumed screen position, plus a comment at the call site stating the
#   grows-up-from-0 / hangs-down-from-1 convention explicitly.]
#
# - Define which response value is the modeled event and use that same ordering for the fit
#   and marginal plots. .check_binary_y() returns unique() order, so reordering rows can swap
#   the two histograms. The p_top/p_bottom names are also opposite the rendered directions:
#   the ordinary scale grows from the bottom and the reversed scale grows from the top.
#   [**FIXED** (Michael, 2026-08-08): row-order independence already fixed above via
#   .to_binary01(); the naming half fixed by the p_hist_y0/p_hist_y1 rename in the bullet above.]
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
#   [**RESOLVED** (Michael, 2026-08-08): option (b) -- ... is now forwarded from all three public
#   methods into .logist_plot_impl(), which calls rlang::check_dots_empty() and errors on
#   anything unconsumed. Not (c): a flat ... can't be routed unambiguously to one of several
#   ggplot layers (geom_smooth vs. the two geom_histogram calls) without colliding names, so
#   future visual-control options (e.g. hist.color) should be added as explicit named params,
#   the way fit.color/marg.color already are -- not as generic passthrough.]
#
# - p_main already has coord_cartesian(); the points branch adds a second coordinate system and
#   reports that the first is being replaced on every call. Construct the coordinate once.
#
#   [**FIXED** (Michael, 2026-08-08): removed coord_cartesian() from p_main's base construction;
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
# - If cowplot remains optional, do not wrap point-only examples in the cowplot availability
#   check; otherwise those examples are skipped even though they need only ggplot2.
#   [**MOOT** (Michael): cowplot is now a hard Imports (see dependency section above), so
#   @examples no longer wraps in any availability check at all -- doesn't apply anymore.]
#
# - @seealso is not required for CRAN, but @seealso [vcd::binreg_plot()] would be useful. With
#   no other help topic in this @family, the family tag currently adds no related-page links.
#   [**FIXED** (Michael, 2026-08-08): added.]
#
# - Add tests for the three interfaces, both marginal modes, the omitted-marginal error,
#   response encodings/event direction, row reordering, NA/Inf/constant x, column selection,
#   formula validation, labels/colors, optional dependencies, and successful plot building.

# ---- public generic + methods ------------------------------------------------------------

#' Plot a fitted logistic regression with marginal distributions of the predictor
#'
#' Plots predicted probabilities from a `glm(y ~ x, family = binomial)` fit for a single
#' quantitative predictor `x` and binary response `y`, and also with the smoothed
#' logistic fit and its confidence band.
#' What this plot method adds is a representation of
#' the marginal distribution of `x` within each `y` group -- mirrored histograms above and
#' below the curve, or jittered points -- as suggested by Smart et al. (2004). These help you
#' see where the data supporting the fit exist; e.g., where the data are "thin", so the confidence band is wide.
#'
#' `logist_plot()` is generic, with methods for a pair of vectors, a data frame, or a
#' model formula. `logist_hist()` and `logist_point()` are convenience wrappers with
#' `marginal=` fixed to `"hist"`/`"points"`, but otherwise accept the same `x`/`...` as
#' `logist_plot()` -- i.e., they also work with a data frame or a formula.
#'
#' @param x a numeric predictor vector or a data frame; see `formula` below for the
#'   model-formula interface
#' @param ... arguments passed to methods, or on to `logist_plot()` from `logist_hist()`/
#'   `logist_point()`. Currently reserved for future visual-control options (e.g. a
#'   `hist.color`); passing any unrecognized argument is an error rather than being
#'   silently ignored.
#'
#' @return A `ggplot` object.
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
#' logist_plot(survived ~ age, data = Donner, marginal = "hist")
#'
#' # convenience wrappers -- marginal= fixed, still get all calling conventions
#' logist_point(survived ~ age, data = Donner)
#' logist_hist(survived ~ age, data = Donner)
#'
#' @importFrom rlang .data
#' @export
logist_plot <- function(x, ...) {
  UseMethod("logist_plot")
}

#' @param y a binary (0/1, or 2-level factor/character/logical) response vector
#' @param marginal character string, how to represent the marginal distribution of x within each y group: one of
#'   "hist", a histogram (default) or "points",jittered points
#' @param bins number of histogram bins, for `marginal = "hist"`; default: 30
#' @param xlab,ylab axis labels; default to the deparsed `x`/`y` expressions
#' @param fit.color color of the fitted logistic curve and its confidence band; default: "steelblue"
#' @param marg.color color of the marginal representation of x within each y group (histogram
#'   fill, or point color for `marginal = "points"`); default: "orange"
#' @rdname logist_plot
#' @export
logist_plot.default <- function(x, y, marginal = c("hist", "points"),
                                 bins = 30, xlab = NULL, ylab = NULL,
                                 fit.color = "steelblue", marg.color = "orange", ...) {
  xlab <- xlab %||% deparse(substitute(x))
  ylab <- ylab %||% deparse(substitute(y))
  .logist_plot_impl(x, y, marginal = marginal, bins = bins, xlab = xlab, ylab = ylab,
                     fit.color = fit.color, marg.color = marg.color, ...)
}

#' @param xvar,yvar which columns of `x` to use as predictor/response -- column name or
#'   position; default to the first two columns (matches the original 2-column-data-frame
#'   calling convention)
#' @rdname logist_plot
#' @export
logist_plot.data.frame <- function(x, xvar = 1L, yvar = 2L,
                                    marginal = c("hist", "points"),
                                    bins = 30, xlab = NULL, ylab = NULL,
                                    fit.color = "steelblue", marg.color = "orange", ...) {
  if (ncol(x) < 2L) {
    stop("`x` must have at least 2 columns.", call. = FALSE)
  }
  xres <- .resolve_col(x, xvar, "xvar")
  yres <- .resolve_col(x, yvar, "yvar")
  .logist_plot_impl(xres$value, yres$value, marginal = marginal, bins = bins,
                     xlab = xlab %||% xres$name, ylab = ylab %||% yres$name,
                     fit.color = fit.color, marg.color = marg.color, ...)
}

#' @param formula a model formula, `y ~ x` -- exactly one response and one predictor;
#'   `formula` method only. The first argument may be passed positionally or as
#'   `formula = y ~ x` (matching base R's `boxplot()`/`lm()` convention) -- unlike the other
#'   methods, it is not named `x`
#' @param data a data frame -- `formula` method only
#' @rdname logist_plot
#' @export
logist_plot.formula <- function(formula, data, marginal = c("hist", "points"),
                                 bins = 30, xlab = NULL, ylab = NULL,
                                 fit.color = "steelblue", marg.color = "orange", ...) {
  mf <- stats::model.frame(formula, data = data, na.action = stats::na.pass)
  if (ncol(mf) != 2L) {
    stop("`formula` must have exactly one response and one predictor (y ~ x); found ",
         ncol(mf) - 1L, " predictor(s).", call. = FALSE)
  }
  .logist_plot_impl(mf[[2]], mf[[1]], marginal = marginal, bins = bins,
                     xlab = xlab %||% names(mf)[2], ylab = ylab %||% names(mf)[1],
                     fit.color = fit.color, marg.color = marg.color, ...)
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
# logist_hist()/logist_point().
.logist_plot_impl <- function(x, y, marginal = c("hist", "points"),
                               bins = 30, xlab = NULL, ylab = NULL,
                               fit.color = "steelblue", marg.color = "orange", ...) {
  rlang::check_dots_empty()
  marginal <- match.arg(marginal)

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

  p_main <- ggplot2::ggplot(data, ggplot2::aes(x = .data$x, y = .data$y)) +
    ggplot2::theme_bw(base_size = 16) +
    ggplot2::geom_smooth(method = "glm", formula = y ~ x,
                          method.args = list(family = "binomial"),
                          se = TRUE, colour = fit.color, fill = fit.color,
                          linewidth = 1.5, alpha = 0.3) +
    ggplot2::theme(panel.grid.major = ggplot2::element_blank(),
                   panel.grid.minor = ggplot2::element_blank(),
                   panel.background = ggplot2::element_blank(),
                   plot.background = ggplot2::element_blank()) +
    ggplot2::labs(y = paste0(ylab, "\n"), x = paste0("\n", xlab))

  if (marginal == "points") {
    p_main +
      ggplot2::geom_point(colour = marg.color, alpha = 0.5,
                           position = ggplot2::position_jitter(w = 0, h = 0.02)) +
      ggplot2::coord_cartesian(xlim = c(min_x, max_x), ylim = c(0, 1))
  } else {
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

    p_main <- p_main +
      ggplot2::scale_y_continuous(
        limits = c(0, 1),
        breaks = seq(0, 1, by = 0.2),
        expand = ggplot2::expansion(mult = 0),
        sec.axis = ggplot2::dup_axis(breaks = count_positions, labels = count_labels, name = "Count")
      ) +
      ggplot2::coord_cartesian(xlim = c(min_x, max_x))

    marginal_hist <- function(lev, reverse) {
      p <- ggplot2::ggplot(data[data$y == lev, ], ggplot2::aes(x = .data$x)) +
        ggplot2::theme_bw(base_size = 16) +
        ggplot2::geom_histogram(fill = marg.color, binwidth = bin_width,
                                 boundary = min_x, closed = "left", alpha = .67) +
        ggplot2::coord_cartesian(xlim = c(min_x, max_x)) +
        ggplot2::theme(panel.grid.major = ggplot2::element_blank(),
                       panel.grid.minor = ggplot2::element_blank(),
                       axis.text = ggplot2::element_text(colour = "transparent"),
                       axis.ticks = ggplot2::element_line(colour = "transparent"),
                       axis.title = ggplot2::element_text(colour = "transparent"),
                       panel.border = ggplot2::element_blank(),
                       panel.background = ggplot2::element_blank(),
                       plot.background = ggplot2::element_blank()) +
        ggplot2::labs(y = paste0(ylab, "\n"), x = paste0("\n", xlab))
      if (reverse) {
        p + ggplot2::scale_y_reverse(limits = c(bin_no, 0),
                                      labels = function(z) rep("0.0", length(z)),
                                      expand = ggplot2::expansion(mult = 0),
                                      sec.axis = ggplot2::dup_axis(breaks = count_ticks,
                                                                    labels = count_ticks,
                                                                    name = "Count"))
      } else {
        p + ggplot2::scale_y_continuous(limits = c(0, bin_no),
                                         labels = function(z) rep("0.0", length(z)),
                                         expand = ggplot2::expansion(mult = 0),
                                         sec.axis = ggplot2::dup_axis(breaks = count_ticks,
                                                                       labels = count_ticks,
                                                                       name = "Count"))
      }
    }

    # y = 0 group: bars grow up from the probability = 0 baseline (unreversed scale).
    # y = 1 group: bars hang down from the probability = 1 baseline (scale_y_reverse()).
    # Named by group, not by screen position -- "top"/"bottom" would describe where each
    # one renders, not what it is, and (as discovered in review) is easy to get backwards.
    p_hist_y0 <- marginal_hist(uy[1], reverse = FALSE)
    p_hist_y1 <- marginal_hist(uy[2], reverse = TRUE)

    cowplot::ggdraw() + cowplot::draw_plot(p_hist_y0) + cowplot::draw_plot(p_hist_y1) +
      cowplot::draw_plot(p_main)
  }
}
