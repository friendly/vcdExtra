#' Weighted Two-Way (Tukey) Decomposition of Log Odds Ratios
#'
#' For a \eqn{2 \times 2 \times R \times C}{2 x 2 x R x C} table, computes an
#' additive row + column decomposition of the stratum log odds ratios tested
#' by [woolf_test()], following Tukey's two-way ("median polish"-style) fit
#' as implemented in the \pkg{twoway} package.
#'
#' Unlike `twoway::twoway()`,
#' which weights every stratum equally, `woolf_twoway()` weights each
#' stratum's log odds ratio by its inverse variance (the Woolf weight), so
#' that strata with more precise log odds ratios contribute more to the
#' fitted row and column effects.
#'
#' @details
#' `twoway::twoway()` has no `weight` argument, so the additive model
#' \eqn{y_{ij} = \mu + \alpha_i + \beta_j + \epsilon_{ij}} is instead fit
#' directly by weighted least squares (`stats::lm()` with `weights = w` and
#' sum-to-zero contrasts), where \eqn{y_{ij}} is the log odds ratio and
#' \eqn{w_{ij}} its inverse variance in stratum \eqn{(i,j)}, exactly as used
#' internally by [woolf_test()]. The resulting fit is repackaged as an
#' object of class `"twoway"`, so that `print.twoway()` and `plot.twoway()`
#' from \pkg{twoway} (`which = "fit"` or `"diagnose"`) work on it unchanged.
#'
#' Set `weighted = FALSE` to instead get the ordinary (unweighted) Tukey
#' mean-polish fit, via `twoway::twoway()` directly, for comparison.
#'
#' This is a purely descriptive decomposition: it does not attempt to
#' partition the [woolf_test()] homogeneity statistic itself (see
#' `issues/woolf.md` for why a naive row/column/residual split of that
#' statistic is not generally valid). It is intended for visualizing how the
#' log odds ratio varies additively (or not) across the row and column
#' stratifying variables.
#'
#' @param x A \eqn{2 \times 2 \times R \times C}{2 x 2 x R x C} array, as
#'   used by [woolf_test()].
#' @param weighted Logical. If `TRUE` (the default), fit by weighted least
#'   squares using Woolf inverse-variance weights. If `FALSE`, fit an
#'   ordinary (unweighted) Tukey mean-polish via `twoway::twoway()`.
#' @param name A label for the data, used in the `print`/`plot` titles.
#'   Defaults to the deparsed expression passed as `x`.
#' @param responseName Label for the response (log odds ratio) axis.
#' @param varNames Character vector of length 2 giving row/column variable
#'   names. Defaults to the strata variable names from `x`'s dimnames.
#'
#' @return An object of class `"twoway"` (see `twoway::twoway()`), with
#'   components `overall`, `roweff`, `coleff`, `residuals`, `name`,
#'   `rownames`, `colnames`, `method`, `responseName`, `varNames`,
#'   `compValue`, `slope`, and `power`. The weighted fit additionally
#'   includes `weights` (the Woolf weights used) and `fit` (the underlying
#'   `lm` object).
#'
#' @seealso [woolf_test()], `twoway::twoway()`, `twoway::plot.twoway()`
#' @family association tests
#'
#' @examples
#' if (requireNamespace("twoway", quietly = TRUE)) {
#'   data(Fungicide, package = "vcdExtra")
#'
#'   tw <- woolf_twoway(Fungicide)
#'   print(tw)
#'   plot(tw, which = "fit")
#'   plot(tw, which = "diagnose")
#'
#'   # compare with the unweighted (plain mean-polish) decomposition
#'   tw0 <- woolf_twoway(Fungicide, weighted = FALSE)
#'   rbind(mean = tw0$roweff, Woolf = tw$roweff)
#'   rbind(mean = tw0$coleff, Woolf = tw$coleff)
#' }
#'
#' @export
woolf_twoway <- function(x, weighted = TRUE,
                          name = deparse(substitute(x)),
                          responseName = "log odds ratio",
                          varNames = NULL) {
  force(name)

  if (!requireNamespace("twoway", quietly = TRUE)) {
    stop("Package 'twoway' is required for woolf_twoway(). Please install it.")
  }

  dims <- dim(x)
  if (length(dims) != 4 || !all(dims[1:2] == 2)) {
    stop("x must be a 2 x 2 x R x C array")
  }

  wt <- woolf_test(x)
  y <- wt$LOR
  se <- wt$LOR_se
  w <- 1 / se^2

  if (is.null(varNames)) varNames <- wt$strata_vars
  if (is.null(varNames) || any(varNames == "")) varNames <- c("Row", "Col")

  if (!weighted) {
    return(twoway::twoway(y, method = "mean", name = name,
                           responseName = responseName, varNames = varNames))
  }

  R <- nrow(y)
  C <- ncol(y)
  rn <- rownames(y)
  cn <- colnames(y)
  if (is.null(rn)) rn <- as.character(seq_len(R))
  if (is.null(cn)) cn <- as.character(seq_len(C))

  fit_df <- data.frame(
    y = c(y), w = c(w),
    row = factor(rep(rn, times = C), levels = rn),
    col = factor(rep(cn, each  = R), levels = cn)
  )

  fit <- stats::lm(y ~ row + col, weights = w, data = fit_df,
                    contrasts = list(row = "contr.sum", col = "contr.sum"))

  cf <- stats::coef(fit)
  overall <- unname(cf["(Intercept)"])

  roweff <- stats::setNames(numeric(R), rn)
  if (R > 1) {
    roweff[-R] <- cf[paste0("row", seq_len(R - 1))]
    roweff[R] <- -sum(roweff[-R])
  }

  coleff <- stats::setNames(numeric(C), cn)
  if (C > 1) {
    coleff[-C] <- cf[paste0("col", seq_len(C - 1))]
    coleff[C] <- -sum(coleff[-C])
  }

  residuals <- y - (outer(roweff, coleff, "+") + overall)
  dimnames(residuals) <- dimnames(y)

  compValue <- matrix(roweff) %*% (coleff / overall)
  dimnames(compValue) <- dimnames(residuals)
  slope <- unname(stats::coef(stats::lm(c(residuals) ~ c(compValue), weights = c(w)))[2])

  structure(
    list(overall = overall, roweff = roweff, coleff = coleff,
         residuals = residuals, name = name, rownames = rn, colnames = cn,
         method = "Woolf", responseName = responseName, varNames = varNames,
         compValue = compValue, slope = slope, power = 1 - slope,
         weights = w, fit = fit),
    class = "twoway")
}
