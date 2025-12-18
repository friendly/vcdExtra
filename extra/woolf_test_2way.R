# Woolf test with two-way decomposition for 2 x 2 x R x C tables
# Decomposes the homogeneity test into row effects, column effects, and residual

woolf_test_2way <- function(x) {
  DNAME <- deparse(substitute(x))

  # Check dimensions
  dims <- dim(x)
  if (length(dims) != 4) {
    stop("This function requires a 4-dimensional array (2 x 2 x R x C)")
  }
  if (!all(dims[1:2] == 2)) {
    stop("First two dimensions must be 2x2")
  }

  R <- dims[3]
  C <- dims[4]

  # Apply continuity correction if needed
  if (any(x == 0))
    x <- x + 1 / 2

  # Helper function to apply Woolf test to a 2x2xk array
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

  # Create output structure
  result <- list(
    overall = list(
      statistic = overall$statistic,
      df = overall$df,
      p.value = overall$p.value
    ),
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
    ),
    data.name = DNAME
  )

  class(result) <- c("woolf_test_2way", "list")
  result
}

# Print method
print.woolf_test_2way <- function(x, ...) {
  cat("\nWoolf Test with Two-Way Decomposition\n")
  cat("Data:", x$data.name, "\n\n")

  cat("Overall homogeneity test:\n")
  cat(sprintf("  X-squared = %.4f, df = %d, p-value = %.4f\n",
              x$overall$statistic, x$overall$df, x$overall$p.value))

  cat("\nDecomposition:\n")
  cat(sprintf("  Rows:      X-squared = %.4f, df = %d, p-value = %.4f\n",
              x$rows$statistic, x$rows$df, x$rows$p.value))
  cat(sprintf("  Columns:   X-squared = %.4f, df = %d, p-value = %.4f\n",
              x$cols$statistic, x$cols$df, x$cols$p.value))
  cat(sprintf("  Residual:  X-squared = %.4f, df = %d, p-value = %.4f\n",
              x$residual$statistic, x$residual$df, x$residual$p.value))

  cat("\nNote: Overall = Rows + Columns + Residual\n")
  invisible(x)
}

# Example interpretation:
# For a 2x2x3x4 table (e.g., Treatment x Outcome x Age x Region):
# - Overall: Tests homogeneity across all 12 Age-Region combinations (df=11)
# - Rows: Tests if OR varies across 3 Age groups (pooling regions) (df=2)
# - Columns: Tests if OR varies across 4 Regions (pooling ages) (df=3)
# - Residual: Tests Age-Region interaction in OR (df=6)
#

TEST <- TRUE

if(TEST) {
data(Fungicide, package = "vcdExtra")
str(Fungicide)

# rearrange `group` for ease of interpretation, so odds ratio > 1 means worse outcome for Treated group
Fungicide <- Fungicide[2:1, , , ]

#' Woolf test only only handles 2 x 2 x k tables
purrr::safely(woolf_test(Fungicide))

# Handles any number of dimensions > 2
woolf_test_general(Fungicide)

woolf_test_2way(Fungicide)

# what about 5-way table?
data(Dyke, package = "vcdExtra")
woolf_test_general(Dyke)

}
