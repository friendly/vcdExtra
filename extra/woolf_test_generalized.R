# Generalized Woolf test for 2 x 2 x ... tables of any dimensionality
# Works for 2 x 2 x k (original), 2 x 2 x R x C, etc.

woolf_test_general <- function(x) {
  DNAME <- deparse(substitute(x))

  # Check that first two dimensions are 2x2
  dims <- dim(x)
  if (length(dims) < 3) {
    stop("Array must have at least 3 dimensions")
  }
  if (!all(dims[1:2] == 2)) {
    stop("First two dimensions must be 2x2")
  }

  # Apply continuity correction if needed
  if (any(x == 0))
    x <- x + 1 / 2

  # If more than 3 dimensions, reshape to collapse dimensions 3+ into one
  if (length(dims) > 3) {
    new_dim <- c(dims[1:2], prod(dims[3:length(dims)]))
    x <- array(x, dim = new_dim)
  }

  # Now proceed with standard Woolf test on 2 x 2 x k table
  k <- dim(x)[3]
  or <- apply(x, 3, function(x) (x[1,1] * x[2,2]) / (x[1,2] * x[2,1]))
  w <-  apply(x, 3, function(x) 1 / sum(1 / x))
  o <- log(or)
  e <- weighted.mean(log(or), w)
  STATISTIC <- sum(w * (o - e)^2)
  PARAMETER <- k - 1
  PVAL <- 1 - pchisq(STATISTIC, PARAMETER)
  METHOD <- "Woolf-test on Homogeneity of Odds Ratios (no 3-Way assoc.)"
  names(STATISTIC) <- "X-squared"
  names(PARAMETER) <- "df"
  structure(list(statistic = STATISTIC, parameter = PARAMETER,
                 p.value = PVAL, method = METHOD, data.name = DNAME, observed = o,
                 expected = e), class = "htest")
}

# Example usage:
# For a 2 x 2 x 3 x 4 table (e.g., Gender x Response x Age x Region):
# - This creates 3*4 = 12 separate 2x2 tables
# - Tests if the Gender-Response odds ratio is homogeneous across all 12 Age-Region combinations
# - df = 12 - 1 = 11
