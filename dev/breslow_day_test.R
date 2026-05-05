#' Breslow-Day Test for Homogeneity of Odds Ratios
#'
#' Tests for homogeneity of odds ratios across strata in \eqn{2 \times 2 \times k}{2 x 2 x k}
#' tables (i.e., whether a common odds ratio fits all strata). Generalized to handle tables
#' of any dimensionality beyond 3. For 4-dimensional tables, optionally provides a two-way
#' decomposition of the homogeneity test into row effects, column effects, and residual,
#' analogous to [woolf_test()] with `decompose = TRUE`.
#'
#' @details
#' The Breslow-Day test (Breslow & Day, 1980) tests the hypothesis that a common odds ratio
#' \eqn{\psi} fits all \eqn{k} strata. Given a common OR (by default the Mantel-Haenszel
#' estimate), the expected cell count \eqn{\tilde{a}_j} in cell \eqn{[1,1]} of stratum
#' \eqn{j} is found by solving the quadratic:
#'
#' \deqn{(\psi - 1)\tilde{a}_j^2 - [n_{2j} - m_{1j} + \psi(n_{1j} + m_{1j})]\tilde{a}_j
#'       + \psi \, m_{1j} n_{1j} = 0}
#'
#' where \eqn{m_{1j}} and \eqn{m_{2j}} are the row margins and \eqn{n_{1j}} and
#' \eqn{n_{2j}} are the column margins of stratum \eqn{j}. The test statistic is:
#'
#' \deqn{\chi^2_{BD} = \sum_{j=1}^{k} \frac{(a_j - \tilde{a}_j)^2}{\widehat{\text{Var}}(a_j)}}
#'
#' where \eqn{\widehat{\text{Var}}(a_j) = (1/\tilde{a}_j + 1/\tilde{b}_j + 1/\tilde{c}_j +
#' 1/\tilde{d}_j)^{-1}} and \eqn{\tilde{b}_j, \tilde{c}_j, \tilde{d}_j} are the remaining
#' expected cell counts. Under the null hypothesis, \eqn{\chi^2_{BD}} follows a chi-squared
#' distribution with \eqn{k - 1} degrees of freedom.
#'
#' The Tarone (1985) correction subtracts a term to account for estimation of the common OR:
#'
#' \deqn{\chi^2_{BD,T} = \chi^2_{BD} -
#'       \frac{(\sum_j a_j - \sum_j \tilde{a}_j)^2}{\sum_j \widehat{\text{Var}}(a_j)}}
#'
#' \strong{Comparison with the Woolf test:}
#' The Woolf test uses log odds ratios and tests deviation from their weighted mean,
#' whereas the Breslow-Day test works on the cell-count scale against a specified common OR.
#' For large samples they agree closely; Breslow-Day is generally preferred when the
#' Mantel-Haenszel common OR is the quantity of interest.
#'
#' \strong{Decomposition for 4-way tables:}
#' For a \eqn{2 \times 2 \times R \times C} table, when `decompose = TRUE`, the overall
#' test is decomposed as:
#'
#' \deqn{\chi^2_{\text{Total}} = \chi^2_{\text{Rows}} + \chi^2_{\text{Cols}} +
#'       \chi^2_{\text{Residual}}}
#'
#' where the row and column components use the row- and column-marginal (pooled) tables,
#' all tested against the same common OR as the overall test. The residual is defined by
#' subtraction and has \eqn{(R-1)(C-1)} degrees of freedom.
#'
#' @param x A \eqn{2 \times 2 \times k}{2 x 2 x k} table, or more generally a
#'   \eqn{2 \times 2 \times \ldots}{2 x 2 x ...} array where the first two dimensions
#'   are both 2.
#' @param OR The common odds ratio to test against. If `NA` (the default), the
#'   Mantel-Haenszel estimate is used.
#' @param correct Logical. If `TRUE`, the Tarone (1985) correction is applied.
#'   Defaults to `FALSE`.
#' @param decompose Logical. If `TRUE` and `x` is 4-dimensional
#'   (a \eqn{2 \times 2 \times R \times C}{2 x 2 x R x C} table), the test is
#'   decomposed into row effects, column effects, and residual. Defaults to `FALSE`.
#'   Ignored for non-4-dimensional tables.
#'
#' @return A list of class `"breslow_day_test"` (also inheriting from `"htest"`)
#'   containing:
#'   \item{statistic}{the chi-squared test statistic.}
#'   \item{parameter}{degrees of freedom.}
#'   \item{p.value}{\eqn{p}-value.}
#'   \item{method}{character string describing the test.}
#'   \item{data.name}{character string giving the name of the data.}
#'   \item{or_vars}{names of the first two dimensions (the 2x2 table variables).}
#'   \item{strata_vars}{names of the stratifying variables (dimensions 3 and beyond).}
#'   \item{OR}{the common odds ratio used (MH estimate if `OR = NA` was supplied).}
#'   \item{correct}{logical indicating whether Tarone correction was applied.}
#'   \item{observed}{observed \eqn{a_j} counts (cell \eqn{[1,1]} of each stratum).}
#'   \item{expected}{expected \eqn{\tilde{a}_j} counts under the common OR.}
#'   \item{decomposed}{logical indicating if decomposition was performed.}
#'
#'   When `decompose = TRUE` (only for 4-dimensional tables), additional components:
#'   \item{rows}{list with `statistic`, `df`, `p.value` for row effects.}
#'   \item{cols}{list with `statistic`, `df`, `p.value` for column effects.}
#'   \item{residual}{list with `statistic`, `df`, `p.value` for residual (interaction).}
#'
#' @importFrom stats mantelhaen.test pchisq
#' @seealso [stats::mantelhaen.test()], [woolf_test()], [DescTools::BreslowDayTest()]
#' @family association tests
#'
#' @references
#' Breslow, N. E. & Day, N. E. (1980). \emph{Statistical Methods in Cancer Research.
#' Vol. 1: The Analysis of Case-Control Studies}. IARC Scientific Publications No. 32.
#' Lyon: International Agency for Research on Cancer.
#'
#' Tarone, R. E. (1985). On heterogeneity tests based on efficient scores.
#' \emph{Biometrika}, \bold{72}, 91-95.
#'
#' Lachin, J. M. (2000). \emph{Biostatistical Methods: The Assessment of Relative Risks}.
#' Wiley, p. 124-125.
#'
#' @examples
#' # 3-way table
#' data(CoalMiners, package = "vcd")
#' breslow_day_test(CoalMiners)
#' breslow_day_test(CoalMiners, correct = TRUE)   # Tarone correction
#'
#' # Compare with Woolf test
#' woolf_test(CoalMiners)
#'
#' data(Heart, package = "vcdExtra")
#' breslow_day_test(Heart)
#'
#' # 4-way table without decomposition
#' data(Fungicide, package = "vcdExtra")
#' breslow_day_test(Fungicide)
#'
#' # 4-way table with decomposition
#' breslow_day_test(Fungicide, decompose = TRUE)
#'
#' @export
breslow_day_test <- function(x, OR = NA, correct = FALSE, decompose = FALSE) {
  DNAME <- deparse(substitute(x))

  dims <- dim(x)
  if (length(dims) < 3)
    stop("Array must have at least 3 dimensions")
  if (!all(dims[1:2] == 2))
    stop("First two dimensions must be 2x2")

  dimnames_x <- names(dimnames(x))
  if (is.null(dimnames_x))
    dimnames_x <- paste0("Dim", seq_along(dims))
  or_vars     <- dimnames_x[1:2]
  strata_vars <- dimnames_x[-(1:2)]

  if (decompose && length(dims) != 4) {
    warning("decompose = TRUE only applies to 4-dimensional tables; ignoring decompose")
    decompose <- FALSE
  }

  # Compute common OR once from all data (used for all sub-tests in decomposed case)
  if (is.na(OR)) {
    x_mh <- if (length(dims) > 3)
      array(x, dim = c(dims[1:2], prod(dims[3:length(dims)])))
    else x
    or.hat <- as.numeric(mantelhaen.test(x_mh)$estimate)
  } else {
    or.hat <- OR
  }

  # Handle 4-dimensional case with decomposition
  if (length(dims) == 4 && decompose) {
    R <- dims[3]; C <- dims[4]

    x_all  <- array(x, dim = c(2, 2, R * C))
    overall <- bd_internal(x_all, or.hat)

    x_rows <- apply(x, c(1, 2, 3), sum)   # 2 x 2 x R
    rows   <- bd_internal(x_rows, or.hat)

    x_cols <- apply(x, c(1, 2, 4), sum)   # 2 x 2 x C
    cols   <- bd_internal(x_cols, or.hat)

    stat_overall <- if (correct) overall$X2.tarone else overall$X2
    stat_rows    <- if (correct) rows$X2.tarone    else rows$X2
    stat_cols    <- if (correct) cols$X2.tarone    else cols$X2
    stat_resid   <- stat_overall - stat_rows - stat_cols
    df_resid     <- (R - 1L) * (C - 1L)

    suffix <- if (correct) " (with Tarone correction)" else ""
    METHOD <- paste0("Breslow-Day Test on Homogeneity of Odds Ratios", suffix)

    STATISTIC <- stat_overall; names(STATISTIC) <- "X-squared"
    PARAMETER <- R * C - 1L;   names(PARAMETER) <- "df"

    result <- list(
      statistic   = STATISTIC,
      parameter   = PARAMETER,
      p.value     = 1 - pchisq(stat_overall, R * C - 1L),
      method      = METHOD,
      data.name   = DNAME,
      or_vars     = or_vars,
      strata_vars = strata_vars,
      OR          = or.hat,
      correct     = correct,
      observed    = overall$a,
      expected    = overall$tildea,
      decomposed  = TRUE,
      rows = list(statistic = stat_rows,
                  df        = R - 1L,
                  p.value   = 1 - pchisq(stat_rows, R - 1L)),
      cols = list(statistic = stat_cols,
                  df        = C - 1L,
                  p.value   = 1 - pchisq(stat_cols, C - 1L)),
      residual = list(statistic = stat_resid,
                      df        = df_resid,
                      p.value   = 1 - pchisq(stat_resid, df_resid))
    )
    class(result) <- c("breslow_day_test", "htest")
    return(result)
  }

  # Standard case: collapse higher dimensions into a single stratum dimension
  if (length(dims) > 3)
    x <- array(x, dim = c(dims[1:2], prod(dims[3:length(dims)])))

  k   <- dim(x)[3]
  res <- bd_internal(x, or.hat)

  STATISTIC <- if (correct) res$X2.tarone else res$X2
  PARAMETER <- k - 1L
  names(STATISTIC) <- "X-squared"
  names(PARAMETER) <- "df"

  suffix <- if (correct) " (with Tarone correction)" else ""
  METHOD <- paste0("Breslow-Day Test on Homogeneity of Odds Ratios", suffix)

  structure(list(
    statistic   = STATISTIC,
    parameter   = PARAMETER,
    p.value     = 1 - pchisq(STATISTIC, PARAMETER),
    method      = METHOD,
    data.name   = DNAME,
    or_vars     = or_vars,
    strata_vars = strata_vars,
    OR          = or.hat,
    correct     = correct,
    observed    = res$a,
    expected    = res$tildea,
    decomposed  = FALSE
  ), class = c("breslow_day_test", "htest"))
}


# Internal helper: Breslow-Day statistic on a 2x2xk array against a fixed OR.
# Returns both the uncorrected (X2) and Tarone-corrected (X2.tarone) statistics,
# plus the per-stratum observed/expected/variance vectors.
bd_internal <- function(y, or) {
  k <- dim(y)[3]
  X2 <- 0
  a <- tildea <- Var.a <- numeric(k)

  for (j in seq_len(k)) {
    mj <- rowSums(y[, , j])   # row margins
    nj <- colSums(y[, , j])   # col margins

    # Solve for tilde_a_j: expected [1,1] cell count under common OR given margins.
    # polyroot() takes coefficients in ascending order (constant term first).
    coef    <- c(-mj[1] * nj[1] * or,
                 nj[2] - mj[1] + or * (nj[1] + mj[1]),
                 1 - or)
    sols    <- Re(polyroot(coef))
    tildeaj <- sols[(sols > 0) & (sols <= min(nj[1], mj[1]))]

    if (length(tildeaj) != 1L)
      stop(sprintf(
        "No unique valid root in stratum %d -- check for zero or very sparse cells", j))

    tildebj <- mj[1] - tildeaj
    tildecj <- nj[1] - tildeaj
    tildedj <- mj[2] - tildecj

    Var.aj <- (1/tildeaj + 1/tildebj + 1/tildecj + 1/tildedj)^(-1)
    aj     <- y[1, 1, j]

    X2        <- X2 + as.numeric((aj - tildeaj)^2 / Var.aj)
    a[j]      <- aj
    tildea[j] <- tildeaj
    Var.a[j]  <- Var.aj
  }

  # Tarone correction: adjusts for estimation of the common OR
  X2.tarone <- as.numeric(X2 - (sum(a) - sum(tildea))^2 / sum(Var.a))

  list(X2 = X2, X2.tarone = X2.tarone,
       a = a, tildea = tildea, Var.a = Var.a)
}


#' Print method for breslow_day_test objects
#'
#' @param x An object of class `"breslow_day_test"`
#' @param digits Number of significant digits for the common OR. Default 4.
#' @param ... Additional arguments (currently unused).
#' @rdname breslow_day_test
#' @export
print.breslow_day_test <- function(x, digits = 4, ...) {
  cat("\n")
  cat(x$method, "\n\n")

  cat("Data:         ", x$data.name, "\n")
  cat("OR variables: ", paste(x$or_vars,     collapse = ", "), "\n")
  cat("Strata:       ", paste(x$strata_vars, collapse = ", "), "\n")
  cat("Common OR:    ", format(x$OR, digits = digits), "\n\n")

  if (!x$decomposed) {
    cat(sprintf("X-squared = %.4f, df = %d, p-value = %.4g\n",
                x$statistic, x$parameter, x$p.value))
  } else {
    cat("Overall homogeneity test:\n")
    cat(sprintf("  X-squared = %.4f, df = %d, p-value = %.4g\n\n",
                x$statistic, x$parameter, x$p.value))

    cat("Decomposition:\n")
    row_label <- sprintf("Rows (%s):", x$strata_vars[1])
    col_label <- sprintf("Cols (%s):", x$strata_vars[2])
    res_label <- "Residual:"
    max_width <- max(nchar(row_label), nchar(col_label), nchar(res_label))

    cat(sprintf("  %-*s X-squared = %.4f, df = %d, p-value = %.4g\n",
                max_width, row_label,
                x$rows$statistic, x$rows$df, x$rows$p.value))
    cat(sprintf("  %-*s X-squared = %.4f, df = %d, p-value = %.4g\n",
                max_width, col_label,
                x$cols$statistic, x$cols$df, x$cols$p.value))
    cat(sprintf("  %-*s X-squared = %.4f, df = %d, p-value = %.4g\n",
                max_width, res_label,
                x$residual$statistic, x$residual$df, x$residual$p.value))

    cat("\nNote: Overall = Rows + Columns + Residual\n")
  }

  invisible(x)
}
