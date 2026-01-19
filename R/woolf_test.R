# Unified Woolf test for 2 x 2 x ... tables of any dimensionality
# Combines functionality from woolf_test.R, woolf_test_general.R, and woolf_test_2way.R
#
# Main changes compared to vcd::woolf_test():
#
# DONE: Generalized to handle tables of any dimensionality >= 3 (not just 2x2xk)
#       For tables with more than 3 dimensions, collapses dimensions 3+ into single stratum dimension
#
# DONE: Added decompose parameter for 4-dimensional tables (2x2xRxC)
#       When decompose=TRUE, provides two-way decomposition into row effects, column effects, and residual
#       Decomposition analogous to two-way ANOVA: Overall = Rows + Columns + Residual
#
# DONE: Unified return structure for both decomposed and non-decomposed cases
#       Both return class c("woolf_test", "htest") with consistent base components
#       Decomposed case includes additional components: rows, cols, residual
#
# DONE: Added dimension name extraction and display
#       New components: or_vars (names of 2x2 table variables), strata_vars (stratifying variables)
#       If array has no dimension names, creates default names (Dim1, Dim2, etc.)
#
# DONE: Created unified print.woolf_test() method
#       Single print method handles both decomposed and non-decomposed cases
#       Displays data name, OR variables, and strata variables in formatted output
#       For decomposed case, shows which stratum variable corresponds to rows/columns
#
# DONE: Added comprehensive roxygen2 documentation
#       Includes @param, @return, @examples, @references, @seealso
#       Documents all return components including new ones
#
# DONE: Added decomposed logical flag to return object
#       Allows print method and other functions to distinguish between cases
#
# TODO: Do we still need to include class "htest"?

#' Woolf Test for Homogeneity of Odds Ratios
#'
#' Test for homogeneity on \eqn{2 \times 2 \times k}{2 x 2 x k} tables
#' over strata (i.e., whether the log odds ratios are the same in all
#' strata). Generalized to handle tables of any dimensionality beyond 3.
#' For 4-dimensional tables, optionally provides a two-way decomposition
#' of the homogeneity test into row effects, column effects, and residual.
#'
#' @details
#' The Woolf test (Woolf, 1955) tests the hypothesis that the odds ratios
#' \eqn{\theta_i} are equal across all \eqn{k} strata. The test statistic
#' is computed as the weighted sum of squared deviations of the log odds ratios
#' from their weighted mean:
#'
#' \deqn{\chi^2_W = \sum_{i=1}^{k} w_i [\log(\theta_i) - \log(\bar{\theta}_w)]^2
#'       = \sum_{i=1}^{k} w_i \log^2(\theta_i / \bar{\theta}_w)}
#'
#' where \eqn{\theta_i = (n_{11i} n_{22i}) / (n_{12i} n_{21i})} is the odds ratio
#' in stratum \eqn{i}, and \eqn{\bar{\theta}_w} is the weighted average odds ratio
#' (computed as the exponential of the weighted mean of the log odds ratios).
#'
#' The weights \eqn{w_i} are the inverse variances of the log odds ratios:
#' \deqn{w_i = 1 / \text{Var}(\log \theta_i) = 1 / (1/n_{11i} + 1/n_{12i} + 1/n_{21i} + 1/n_{22i})}
#'
#' Under the null hypothesis of homogeneous odds ratios, \eqn{\chi^2_W} follows
#' a chi-squared distribution with \eqn{k - 1} degrees of freedom.
#'
#' \strong{Decomposition for 4-way tables:}
#' For a \eqn{2 \times 2 \times R \times C} table, the strata
#' form an \eqn{R \times C} two-way layout with odds ratios
#' \eqn{\theta_{ij}} for row \eqn{i} and column \eqn{j}. This suggests
#' that overall Woolf test
#' of homogeneity can be decomposed into three components, conceptually
#' analogous to a two-way ANOVA with one observation per cell:
#'
#' \deqn{\chi^2_{\text{W:Total}} = \chi^2_{\text{W:Rows}} + \chi^2_{\text{W:Cols}} + \chi^2_{\text{W:Residual}}}
#'
#' where:
#' \itemize{
#'   \item \eqn{\chi^2_{\text{W:Rows}} } tests whether the odds ratios differ among
#'     row levels (pooling over columns), with \eqn{R - 1} df
#'   \item \eqn{\chi^2_{\text{W:Cols}}} tests whether the odds ratios differ among
#'     column levels (pooling over rows), with \eqn{C - 1} df
#'   \item \eqn{\chi^2_{\text{W:Residual}}} tests the row \eqn{\times}{x} column interaction
#'     (deviation from additivity on the log odds scale), with \eqn{(R-1)(C-1)} df
#' }
#'
#' The row effect test compares the marginal log odds ratios
#' \eqn{\log \bar{\theta}_{i+}} (pooled over columns) to the overall weighted mean.
#' Similarly, the column effect test compares \eqn{\log \bar{\theta}_{+j}}
#' (pooled over rows). The residual tests whether the cell log odds ratios
#' \eqn{\log \theta_{ij}} are additive in the row and column effects.
#'
#' \strong{Note:} The two-way ANOVA-like decomposition for 4-dimensional tables
#' appears to be a novel extension introduced in this package. The existing
#' literature on the Woolf test (and related Breslow-Day test) treats strata
#' as unstructured, testing only whether all \eqn{k} odds ratios are equal.
#' This decomposition exploits the factorial structure of \eqn{R \times C}{R x C}
#' strata to provide more detailed insight into the sources of heterogeneity.
#'
#' @param x A \eqn{2 \times 2 \times k}{2 x 2 x k} table, or more generally,
#'   a \eqn{2 \times 2 \times \ldots}{2 x 2 x ...} array where the first
#'   two dimensions are both 2.
#' @param decompose Logical. If \code{TRUE} and \code{x} is 4-dimensional
#'   (a \eqn{2 \times 2 \times R \times C}{2 x 2 x R x C} table), the test
#'   is decomposed into row effects, column effects, and residual (interaction).
#'   Defaults to \code{FALSE}. Ignored for non-4-dimensional tables.
#'
#' @return A list of class \code{"woolf_test"} (also inheriting from \code{"htest"})
#'   containing the following components:
#'   \item{statistic}{the chi-squared test statistic.}
#'   \item{parameter}{degrees of freedom of the approximate chi-squared
#'     distribution of the test statistic.}
#'   \item{p.value}{\eqn{p}-value for the test.}
#'   \item{method}{a character string indicating the type of test performed.}
#'   \item{data.name}{a character string giving the name of the data.}
#'   \item{or_vars}{names of the first two dimensions (the 2x2 table variables).}
#'   \item{strata_vars}{names of the stratifying variables (dimensions 3 and beyond).}
#'   \item{observed}{the observed log odds ratios.}
#'   \item{expected}{the expected log odds ratio under the null hypothesis (weighted mean).}
#'   \item{decomposed}{logical indicating if decomposition was performed.}
#'
#'   When \code{decompose = TRUE} (only for 4-dimensional tables), additional components:
#'   \item{rows}{list with statistic, df, p.value, observed, expected for row effects.}
#'   \item{cols}{list with statistic, df, p.value, observed, expected for column effects.}
#'   \item{residual}{list with statistic, df, p.value for residual (interaction).}
#'
#' @importFrom stats weighted.mean
#' @seealso \code{\link[stats]{mantelhaen.test}}
#' @family association tests
#'
#' @references
#' Woolf, B. (1955). On estimating the relation between blood group and disease.
#' \emph{Annals of Human Genetics}, \bold{19}, 251-253.
#'
#' @examples
#' # 3-way tables
#' data(CoalMiners, package = "vcd")
#' woolf_test(CoalMiners)
#'
#' data(Heart, package = "vcdExtra")
#' woolf_test(Heart)
#'
#' # 4-way table without decomposition
#' data(Fungicide, package = "vcdExtra")
#' woolf_test(Fungicide)
#'
#' # 4-way table with decomposition
#' woolf_test(Fungicide, decompose = TRUE)
#'
#' # 4-way table, but need to rearrange dimensions
#' # How does association between `Preference` and `M_User` vary over strata?
#' data(Detergent, package = "vcdExtra")
#' dimnames(Detergent) |> names()
#' Detergent <- aperm(Detergent, c(3, 2, 1, 4))
#' woolf_test(Detergent)
#'
#'
#' @export
woolf_test <- function(x, decompose = FALSE) {
  DNAME <- deparse(substitute(x))

  # Check that first two dimensions are 2x2
  dims <- dim(x)
  if (length(dims) < 3) {
    stop("Array must have at least 3 dimensions")
  }
  if (!all(dims[1:2] == 2)) {
    stop("First two dimensions must be 2x2")
  }

  # Extract dimension names
  dimnames_x <- names(dimnames(x))
  if (is.null(dimnames_x)) {
    dimnames_x <- paste0("Dim", 1:length(dims))
  }
  or_vars <- dimnames_x[1:2]
  strata_vars <- dimnames_x[-(1:2)]

  # Check decompose argument validity
  if (decompose && length(dims) != 4) {
    warning("decompose = TRUE only applies to 4-dimensional tables; ignoring decompose")
    decompose <- FALSE
  }

  # Apply continuity correction if needed
  if (any(x == 0))
    x <- x + 1 / 2

  # Internal helper function to compute Woolf test on 2x2xk array
  woolf_internal <- function(y) {
    k <- dim(y)[3]
    or <- apply(y, 3, function(x) (x[1,1] * x[2,2]) / (x[1,2] * x[2,1]))
    w <-  apply(y, 3, function(x) 1 / sum(1 / x))
    o <- log(or)
    e <- weighted.mean(log(or), w)
    statistic <- sum(w * (o - e)^2)
    df <- k - 1
    pval <- 1 - pchisq(statistic, df)
    list(statistic = statistic, df = df, p.value = pval,
         observed = o, expected = e, weights = w)
  }

  # Handle 4-dimensional case with decomposition
  if (length(dims) == 4 && decompose) {
    R <- dims[3]
    C <- dims[4]

    # Overall test (all R*C strata)
    x_all <- array(x, dim = c(2, 2, R * C))
    overall <- woolf_internal(x_all)

    # Row effects: pool across columns (dimension 4) for each row (dimension 3)
    # Result: 2 x 2 x R array
    x_rows <- apply(x, c(1, 2, 3), sum)
    rows <- woolf_internal(x_rows)

    # Column effects: pool across rows (dimension 3) for each column (dimension 4)
    # Result: 2 x 2 x C array
    x_cols <- apply(x, c(1, 2, 4), sum)
    cols <- woolf_internal(x_cols)

    # Residual (interaction): difference between overall and main effects
    residual_stat <- overall$statistic - rows$statistic - cols$statistic
    residual_df <- (R - 1) * (C - 1)
    residual_pval <- 1 - pchisq(residual_stat, residual_df)

    # Create output structure (unified with standard case)
    names(overall$statistic) <- "X-squared"
    names(overall$df) <- "df"

    result <- list(
      statistic = overall$statistic,
      parameter = overall$df,
      p.value = overall$p.value,
      method = "Woolf-test on Homogeneity of Odds Ratios (no 4-way association)",
      data.name = DNAME,
      or_vars = or_vars,
      strata_vars = strata_vars,
      observed = overall$observed,
      expected = overall$expected,
      decomposed = TRUE,
      rows = list(
        statistic = rows$statistic,
        df = rows$df,
        p.value = rows$p.value,
        observed = rows$observed,
        expected = rows$expected
      ),
      cols = list(
        statistic = cols$statistic,
        df = cols$df,
        p.value = cols$p.value,
        observed = cols$observed,
        expected = cols$expected
      ),
      residual = list(
        statistic = residual_stat,
        df = residual_df,
        p.value = residual_pval
      )
    )

    class(result) <- c("woolf_test", "htest")
    return(result)
  }

  # Standard case: general k-way test
  # If more than 3 dimensions, reshape to collapse dimensions 3+ into one
  if (length(dims) > 3) {
    new_dim <- c(dims[1:2], prod(dims[3:length(dims)]))
    x <- array(x, dim = new_dim)
  }

  # Compute standard Woolf test
  k <- dim(x)[3]
  or <- apply(x, 3, function(x) (x[1,1] * x[2,2]) / (x[1,2] * x[2,1]))
  w <-  apply(x, 3, function(x) 1 / sum(1 / x))
  o <- log(or)
  e <- weighted.mean(log(or), w)
  STATISTIC <- sum(w * (o - e)^2)
  PARAMETER <- k - 1
  PVAL <- 1 - pchisq(STATISTIC, PARAMETER)

  # Set method description based on dimensionality
  if (length(dims) == 3) {
    METHOD <- "Woolf-test on Homogeneity of Odds Ratios (no 3-way association)"
  } else {
    METHOD <- paste0("Woolf-test on Homogeneity of Odds Ratios (no ",
                     length(dims), "-way association)")
  }

  names(STATISTIC) <- "X-squared"
  names(PARAMETER) <- "df"

  structure(list(statistic = STATISTIC,
                 parameter = PARAMETER,
                 p.value = PVAL,
                 method = METHOD,
                 data.name = DNAME,
                 or_vars = or_vars,
                 strata_vars = strata_vars,
                 observed = o,
                 expected = e,
                 decomposed = FALSE),
            class = c("woolf_test", "htest"))
}

#' Print method for woolf_test objects
#'
#' @param x An object of class \code{"woolf_test"}
#' @param ... Additional arguments (currently unused)
#' @rdname woolf_test
#' @export
print.woolf_test <- function(x, ...) {
  cat("\n")
  cat(x$method, "\n\n")

  # Display data information
  cat("Data:         ", x$data.name, "\n")
  cat("OR variables: ", paste(x$or_vars, collapse = ", "), "\n")
  cat("Strata:       ", paste(x$strata_vars, collapse = ", "), "\n\n")

  # Standard output (always shown)
  if (!x$decomposed) {
    # Simple case: just show overall test
    cat(sprintf("X-squared = %.4f, df = %d, p-value = %.4g\n",
                x$statistic, x$parameter, x$p.value))
  } else {
    # Decomposed case: show overall + decomposition
    cat("Overall homogeneity test:\n")
    cat(sprintf("  X-squared = %.4f, df = %d, p-value = %.4g\n\n",
                x$statistic, x$parameter, x$p.value))

    cat("Decomposition:\n")
    # Calculate label widths for alignment
    row_label <- sprintf("Rows (%s):", x$strata_vars[1])
    col_label <- sprintf("Cols (%s):", x$strata_vars[2])
    res_label <- "Residual:"
    max_width <- max(nchar(row_label), nchar(col_label), nchar(res_label))

    cat(sprintf("  %-*s X-squared = %.4f, df = %d, p-value = %.4g\n",
                max_width, row_label, x$rows$statistic, x$rows$df, x$rows$p.value))
    cat(sprintf("  %-*s X-squared = %.4f, df = %d, p-value = %.4g\n",
                max_width, col_label, x$cols$statistic, x$cols$df, x$cols$p.value))
    cat(sprintf("  %-*s X-squared = %.4f, df = %d, p-value = %.4g\n",
                max_width, res_label, x$residual$statistic, x$residual$df, x$residual$p.value))

    cat("\nNote: Overall = Rows + Columns + Residual\n")
  }

  invisible(x)
}

# Example usage:
#
# # For a 2 x 2 x k table (3-way):
# woolf_test(x)  # Standard test
#
# # For a 2 x 2 x R x C table (4-way):
# woolf_test(x)                  # Standard test (treats as R*C strata)
# woolf_test(x, decompose = TRUE) # Two-way decomposition
#
# # For a 2 x 2 x ... table (5+ dimensions):
# woolf_test(x)  # Standard test (collapses all dimensions beyond 2)
