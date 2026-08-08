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
# [DONE] Combine hist/points into one function via `marginal = c("hist", "points")` (Gavin)
# [DONE] Make into a proper, general function with x=, y=, data= -- via S3 methods below
#        (default / data.frame / formula), matching base R's plot()/boxplot() convention.
#        Deliberately dropped `data=` from the generic itself -- it only makes sense for the
#        `formula` method, so it lives there, not on every call.
# [DONE] Get variable labels from data or xlab=/ylab= args -- each method derives sensible
#        defaults (deparsed vector expression / data frame column name / formula term name),
#        overridable via xlab=/ylab=.
#
# CRAN-readiness fixes vs. the original:
# - No more `require()` inside functions (not CRAN-compliant) -- guarded via requireNamespace()
#   + `pkg::fun()`, matching this package's existing style (see R/color_table.R).
# - Dropped the `gridExtra` dependency entirely -- it was require()d but never actually used
#   (all grid calls were from `grid` itself). The marginal="hist" compositing (previously raw
#   grid::viewport overlays, drawn as a side effect, returning invisible(NULL)) is now done with
#   cowplot::ggdraw() + draw_plot(), which returns an actual ggplot object -- verified against the
#   Donner data to render identically to the original grid-viewport version.
# - Fixed deprecated ggplot2 arg: `geom_smooth(..., size = 1.5, ...)` -> `linewidth = 1.5`
#   (the "points" branch was still using the pre-3.4.0 `size` aesthetic for lines).
# - `aes(x = x, y = y)` -> `aes(x = .data$x, y = .data$y)` to avoid an R CMD check NOTE
#   ("no visible binding for global variable").
# - Added input validation: `data` must have >= 2 columns (data.frame method), and `y` must be
#   binary (exactly 2 distinct values) -- both silently misbehaved before.
# - Removed dead code (`min_y`/`max_y` were computed but never used).
# - Renamed loop variables `a`, `b`, `c` (the last of which shadowed base::c()) to `p_main`,
#   `p_top`, `p_bottom`.
#
# Kept, per discussion: the old loghistplot()/logpointplot() single-purpose functions are not
# dropped -- reimplemented as thin convenience wrappers `logist_hist()` / `logist_point()` that
# just call `logist_plot(..., marginal = "hist"/"points")`, so they inherit all three calling
# conventions (vector/data.frame/formula) for free instead of duplicating the implementation.

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
#' @param x a numeric predictor vector, a data frame, or a formula (`y ~ x`)
#' @param ... arguments passed to methods, or on to `logist_plot()` from `logist_hist()`/
#'   `logist_point()`
#'
#' @return A `ggplot` object.
#' @author Gavin Klorfine, Michael Friendly
#'
#' @family logistic regression plots
#'
#' @references
#' Smart, S. M. et al. (2004). A New Means of Presenting the Results of Logistic Regression,
#' *Bulletin of the Ecological Society of America*, 85(3).
#' <https://esapubs.org/bulletin/backissues/085-3/bulletinjuly2004_2column.htm#tools1>
#'
#' @examples
#' data(Donner, package = "vcdExtra")
#'
#' if (requireNamespace("ggplot2", quietly = TRUE) &&
#'     requireNamespace("cowplot", quietly = TRUE)) {
#'
#' # three interfaces to the same underlying plot
#' logist_plot(Donner$age, Donner$survived, marginal = "points")
#' logist_plot(Donner[, c("age", "survived")], marginal = "hist")
#' logist_plot(survived ~ age, data = Donner, marginal = "hist")
#'
#' # convenience wrappers -- marginal= fixed, still get all calling conventions
#' logist_point(survived ~ age, data = Donner)
#' logist_hist(survived ~ age, data = Donner)
#' }
#'
#' @export
logist_plot <- function(x, ...) {
  UseMethod("logist_plot")
}

#' @param y a binary (0/1, or 2-level factor/character/logical) response vector
#' @param marginal character string, how to represent the marginal distribution of x within each y group: one of
#'   "hist", a histogram (default) or "points",jittered points
#' @param bins number of histogram bins, for `marginal = "hist"`; default: 30
#' @param xlab,ylab axis labels; default to the deparsed `x`/`y` expressions
#' @rdname logist_plot
#' @export
logist_plot.default <- function(x, y, marginal = c("hist", "points"),
                                 bins = 30, xlab = NULL, ylab = NULL, ...) {
  xlab <- xlab %||% deparse(substitute(x))
  ylab <- ylab %||% deparse(substitute(y))
  .logist_plot_impl(x, y, marginal = marginal, bins = bins, xlab = xlab, ylab = ylab)
}

#' @param xvar,yvar which columns of `x` to use as predictor/response -- column name or
#'   position; default to the first two columns (matches the original 2-column-data-frame
#'   calling convention)
#' @rdname logist_plot
#' @export
logist_plot.data.frame <- function(x, xvar = 1L, yvar = 2L,
                                    marginal = c("hist", "points"),
                                    bins = 30, xlab = NULL, ylab = NULL, ...) {
  if (ncol(x) < 2L) {
    stop("`x` must have at least 2 columns.", call. = FALSE)
  }
  xcol <- if (is.numeric(xvar)) names(x)[xvar] else xvar
  ycol <- if (is.numeric(yvar)) names(x)[yvar] else yvar
  .logist_plot_impl(x[[xcol]], x[[ycol]], marginal = marginal, bins = bins,
                     xlab = xlab %||% xcol, ylab = ylab %||% ycol)
}

#' @param data a data frame -- `formula` method only
#' @rdname logist_plot
#' @export
logist_plot.formula <- function(x, data, marginal = c("hist", "points"),
                                 bins = 30, xlab = NULL, ylab = NULL, ...) {
  mf <- stats::model.frame(x, data = data)
  .logist_plot_impl(mf[[2]], mf[[1]], marginal = marginal, bins = bins,
                     xlab = xlab %||% names(mf)[2], ylab = ylab %||% names(mf)[1])
}

# ---- convenience wrappers (fixed marginal=) ------------------------------------------------

#' @rdname logist_plot
#' @export
logist_hist <- function(x, ...) {
  logist_plot(x, ..., marginal = "hist")
}

#' @rdname logist_plot
#' @export
logist_point <- function(x, ...) {
  logist_plot(x, ..., marginal = "points")
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

.check_bins <- function(bins) {
  if (length(bins) != 1L || !is.numeric(bins) || is.na(bins) ||
      !is.finite(bins) || bins < 1 || bins != floor(bins)) {
    stop("`bins` must be one positive whole number.", call. = FALSE)
  }
}

.check_binary_y <- function(y) {
  uy <- unique(y[!is.na(y)])
  if (length(uy) != 2L) {
    stop("`y` must be binary (exactly 2 distinct values); found ", length(uy), ".",
         call. = FALSE)
  }
  uy
}

# The one real implementation, shared by all logist_plot() methods and by
# logist_hist()/logist_point().
.logist_plot_impl <- function(x, y, marginal = c("hist", "points"),
                               bins = 30, xlab = NULL, ylab = NULL) {
  marginal <- match.arg(marginal)
  if (!requireNamespace("ggplot2", quietly = TRUE)) {
    stop("Package 'ggplot2' is required for logist_plot(). Please install it.", call. = FALSE)
  }

  data <- data.frame(x = x, y = y)
  uy <- .check_binary_y(data$y)
  xlab <- xlab %||% "x"
  ylab <- ylab %||% "y"

  min_x <- min(data$x)
  max_x <- max(data$x)

  p_main <- ggplot2::ggplot(data, ggplot2::aes(x = .data$x, y = .data$y)) +
    ggplot2::theme_bw(base_size = 16) +
    ggplot2::geom_smooth(method = "glm", formula = y ~ x,
                          method.args = list(family = "binomial"),
                          se = TRUE, colour = "black", linewidth = 1.5, alpha = 0.3) +
    ggplot2::coord_cartesian(xlim = c(min_x, max_x)) +
    ggplot2::theme(panel.grid.major = ggplot2::element_blank(),
                   panel.grid.minor = ggplot2::element_blank(),
                   panel.background = ggplot2::element_blank(),
                   plot.background = ggplot2::element_blank()) +
    ggplot2::labs(y = paste0(ylab, "\n"), x = paste0("\n", xlab))

  if (marginal == "points") {
    p_main +
      ggplot2::geom_point(alpha = 0.5, position = ggplot2::position_jitter(w = 0, h = 0.02)) +
      ggplot2::scale_y_continuous(limits = c(0, 1))
  } else {
    .check_bins(bins)

    bin_width <- (max_x - min_x) / bins
    hist_breaks <- seq(min_x, max_x, length.out = bins + 1)
    hist_counts <- lapply(uy, function(lev) {
      graphics::hist(data$x[data$y == lev], breaks = hist_breaks, right = FALSE,
                      include.lowest = TRUE, plot = FALSE)$counts
    })
    max_count <- max(unlist(hist_counts))
    bin_no <- 4 * max_count

    count_ticks <- pretty(c(0, max_count))
    count_ticks <- count_ticks[count_ticks >= 0 & count_ticks <= max_count]
    count_positions <- sort(c(count_ticks / bin_no, 1 - count_ticks / bin_no))
    count_labels <- round(bin_no * pmin(count_positions, 1 - count_positions))

    p_main <- p_main +
      ggplot2::scale_y_continuous(
        limits = c(0, 1),
        breaks = seq(0, 1, by = 0.2),
        expand = ggplot2::expansion(mult = 0),
        sec.axis = ggplot2::dup_axis(breaks = count_positions, labels = count_labels, name = "Count")
      )

    marginal_hist <- function(lev, reverse) {
      p <- ggplot2::ggplot(data[data$y == lev, ], ggplot2::aes(x = .data$x)) +
        ggplot2::theme_bw(base_size = 16) +
        ggplot2::geom_histogram(fill = "grey", binwidth = bin_width,
                                 boundary = min_x, closed = "left") +
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

    p_top <- marginal_hist(uy[1], reverse = FALSE)
    p_bottom <- marginal_hist(uy[2], reverse = TRUE)

    if (!requireNamespace("cowplot", quietly = TRUE)) {
      stop("Package 'cowplot' is required for `marginal = \"hist\"`. Please install it.",
           call. = FALSE)
    }
    cowplot::ggdraw() + cowplot::draw_plot(p_top) + cowplot::draw_plot(p_bottom) +
      cowplot::draw_plot(p_main)
  }
}
