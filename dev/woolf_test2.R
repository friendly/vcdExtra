# Prototype: woolf_test2() -- exploring `decompose` as a string, offering
# several row x column decomposition strategies for a 2x2xRxC table, per
# the analysis in issues/woolf.md and the companion derivation in
# GeneralWoolf/planning/woolf-decomposition.md.
#
# woolf_test(x, decompose = TRUE) (the shipped version, R/woolf_test.R) has a
# single, buggy decomposition: it computes Rows/Cols from re-pooled counts and
# defines Residual by subtraction, which can go negative (see issues/woolf.md).
#
# Here, `decompose` becomes a string selecting one of several strategies:
#
#   "none"        ordinary Woolf test, no decomposition (equivalent to the
#                 shipped woolf_test(x, decompose = FALSE), generalized to any
#                 k-way table)
#
#   "naive"       reproduces the CURRENT (buggy) row/col/residual split, for
#                 side-by-side comparison and as a regression check that this
#                 file's "none" and its LOR/LOR_se agree with the shipped
#                 woolf_test(). NOT valid for inference -- Residual can be
#                 negative. Only 2x2xRxC (4-dim) tables.
#
#   "sequential"  valid, additive Type-I-style WLS decomposition: fits
#                 y ~ 1, y ~ row, y ~ col, y ~ row + col by weighted least
#                 squares on the per-stratum log odds ratios (weights =
#                 1/LOR_se^2), in the order given by `order`. Always additive
#                 and nonnegative, but order matters when Woolf weights are
#                 not (at least approximately) separable -- see `order`.
#
#   "partial"     order-independent adjusted (Type III-style) tests,
#                 Q_{R|C}, Q_{C|R}, Q_{R:C}. Does NOT generally sum to the
#                 overall statistic; labeled as partial/adjusted, not additive.
#
#   "symmetric"   symmetric (two-term Shapley) averaged allocation of the two
#                 sequential orderings. Additive and order-free by
#                 construction, but the averaged main-effect terms are not
#                 standard nested-model chi-squared statistics (no exact
#                 reference distribution is claimed for them here).
#
# `order` (for "sequential") picks which main effect enters the WLS fit
# first: "rows" gives Q_R, Q_{C|R}, Q_{R:C}; "cols" gives Q_C, Q_{R|C}, Q_{R:C}.
#
# This file is self-contained (does not call vcdExtra::woolf_test()) so it can
# be sourced and iterated on independently; dev/test-woolf_test2.R exercises it
# against the shipped woolf_test() and the issues/woolf.md counterexample.

#' @keywords internal
woolf_test2 <- function(x,
                         decompose = c("none", "naive", "sequential",
                                       "partial", "symmetric"),
                         order = c("rows", "cols")) {
  decompose <- match.arg(decompose)
  order <- match.arg(order)

  DNAME <- deparse(substitute(x))
  dims <- dim(x)
  if (length(dims) < 3) stop("Array must have at least 3 dimensions")
  if (!all(dims[1:2] == 2)) stop("First two dimensions must be 2x2")

  dimnames_x <- names(dimnames(x))
  if (is.null(dimnames_x)) dimnames_x <- paste0("Dim", seq_along(dims))
  or_vars <- dimnames_x[1:2]
  strata_vars <- dimnames_x[-(1:2)]

  needs_4way <- decompose != "none"
  if (needs_4way && length(dims) != 4) {
    stop("decompose = \"", decompose, "\" requires a 2x2xRxC (4-dimensional) table")
  }

  if (any(x == 0)) x <- x + 1 / 2

  strata_margin <- seq_along(dims)[-(1:2)]
  OR_arr <- apply(x, strata_margin,
                   function(tab) (tab[1, 1] * tab[2, 2]) / (tab[1, 2] * tab[2, 1]))
  W_arr <- apply(x, strata_margin, function(tab) 1 / sum(1 / tab))
  LOR <- log(OR_arr)
  LOR_se <- sqrt(1 / W_arr)

  o <- as.vector(LOR)
  w <- as.vector(W_arr)
  k <- length(o)
  e <- weighted.mean(o, w)
  Q0 <- sum(w * (o - e)^2)
  df0 <- k - 1
  p0 <- 1 - pchisq(Q0, df0)

  result <- list(statistic = Q0, parameter = df0, p.value = p0,
                 method = "Woolf-test on Homogeneity of Odds Ratios",
                 data.name = DNAME, or_vars = or_vars, strata_vars = strata_vars,
                 LOR = LOR, LOR_se = LOR_se, decompose = decompose)

  if (decompose == "none") {
    class(result) <- c("woolf_test2", "htest")
    return(result)
  }

  R <- dims[3]; C <- dims[4]

  if (decompose == "naive") {
    woolf_stat <- function(y_arr) {
      or <- apply(y_arr, 3, function(t) (t[1,1] * t[2,2]) / (t[1,2] * t[2,1]))
      wt <- apply(y_arr, 3, function(t) 1 / sum(1 / t))
      yy <- log(or)
      ee <- weighted.mean(yy, wt)
      kk <- length(yy)
      list(statistic = sum(wt * (yy - ee)^2), df = kk - 1)
    }
    x_rows <- apply(x, c(1, 2, 3), sum)
    x_cols <- apply(x, c(1, 2, 4), sum)
    rows <- woolf_stat(x_rows)
    cols <- woolf_stat(x_cols)
    residual_stat <- Q0 - rows$statistic - cols$statistic
    residual_df <- (R - 1) * (C - 1)

    result$rows <- list(statistic = rows$statistic, df = rows$df,
                         p.value = 1 - pchisq(rows$statistic, rows$df))
    result$cols <- list(statistic = cols$statistic, df = cols$df,
                         p.value = 1 - pchisq(cols$statistic, cols$df))
    result$residual <- list(statistic = residual_stat, df = residual_df,
                             p.value = 1 - pchisq(residual_stat, residual_df))
    class(result) <- c("woolf_test2", "htest")
    return(result)
  }

  # decompose %in% c("sequential", "partial", "symmetric"): fit the four
  # nested WLS models on the SAME per-stratum LOR/weights (never re-pooled
  # counts), per issues/woolf.md Fix 2 / GeneralWoolf's derivation.
  rn <- rownames(LOR); if (is.null(rn)) rn <- as.character(seq_len(R))
  cn <- colnames(LOR); if (is.null(cn)) cn <- as.character(seq_len(C))
  fdf <- data.frame(y = as.vector(LOR), w = as.vector(W_arr),
                     row = factor(rep(rn, times = C), levels = rn),
                     col = factor(rep(cn, each  = R), levels = cn))

  wsse <- function(fit) sum(fit$weights * residuals(fit)^2)
  S0 <- wsse(stats::lm(y ~ 1,         weights = w, data = fdf))
  SR <- wsse(stats::lm(y ~ row,       weights = w, data = fdf))
  SC <- wsse(stats::lm(y ~ col,       weights = w, data = fdf))
  SA <- wsse(stats::lm(y ~ row + col, weights = w, data = fdf))

  Q_RC <- SA; df_RC <- (R - 1) * (C - 1)
  Q_R  <- S0 - SR; df_R <- R - 1
  Q_C  <- S0 - SC; df_C <- C - 1
  Q_CgR <- SR - SA; df_CgR <- C - 1  # Cols | Rows
  Q_RgC <- SC - SA; df_RgC <- R - 1  # Rows | Cols

  mk <- function(stat, df) list(statistic = stat, df = df,
                                 p.value = 1 - pchisq(stat, df))

  if (decompose == "sequential") {
    if (order == "rows") {
      result$order <- "rows first: Rows, then Cols | Rows"
      result$rows      <- mk(Q_R, df_R)
      result$cols_given_rows <- mk(Q_CgR, df_CgR)
    } else {
      result$order <- "cols first: Cols, then Rows | Cols"
      result$cols      <- mk(Q_C, df_C)
      result$rows_given_cols <- mk(Q_RgC, df_RgC)
    }
    result$residual <- mk(Q_RC, df_RC)
  } else if (decompose == "partial") {
    result$rows_given_cols <- mk(Q_RgC, df_RgC)
    result$cols_given_rows <- mk(Q_CgR, df_CgR)
    result$residual <- mk(Q_RC, df_RC)
  } else if (decompose == "symmetric") {
    Q_R_avg <- (Q_R + Q_RgC) / 2
    Q_C_avg <- (Q_C + Q_CgR) / 2
    result$rows <- list(statistic = Q_R_avg, df = df_R)
    result$cols <- list(statistic = Q_C_avg, df = df_C)
    result$residual <- mk(Q_RC, df_RC)
  }

  class(result) <- c("woolf_test2", "htest")
  result
}

#' @keywords internal
print.woolf_test2 <- function(x, ...) {
  cat("\n", x$method, " (decompose = \"", x$decompose, "\")\n\n", sep = "")
  cat("Data:         ", x$data.name, "\n")
  cat("OR variables: ", paste(x$or_vars, collapse = ", "), "\n")
  cat("Strata:       ", paste(x$strata_vars, collapse = ", "), "\n\n")

  cat(sprintf("Overall: X-squared = %.4f, df = %d, p-value = %.4g\n",
              x$statistic, x$parameter, x$p.value))

  if (x$decompose == "none") { cat("\n"); return(invisible(x)) }

  cat("\n")
  # NB: use `[[` (exact match) throughout, never `$` -- with `$`, e.g.
  # x$cols silently partial-matches x$cols_given_rows when "cols" isn't
  # itself a name in x, which produced wrong/duplicate lines here before.
  fmt <- function(label, comp) {
    if (is.null(comp)) return(invisible())
    cat(sprintf("  %-20s X-squared = %8.4f, df = %d, p-value = %.4g\n",
                label, comp$statistic, comp$df, comp$p.value))
  }

  if (x$decompose == "naive") {
    cat("Decomposition (NAIVE -- known to be invalid; see issues/woolf.md):\n")
    fmt("Rows:", x[["rows"]]); fmt("Cols:", x[["cols"]]); fmt("Residual:", x[["residual"]])
    cat("\nNote: Overall = Rows + Cols + Residual (can be negative -- do not trust)\n")
  } else if (x$decompose == "sequential") {
    cat("Sequential decomposition (", x$order, "):\n", sep = "")
    fmt("Rows:", x[["rows"]]); fmt("Cols | Rows:", x[["cols_given_rows"]])
    fmt("Cols:", x[["cols"]]); fmt("Rows | Cols:", x[["rows_given_cols"]])
    fmt("Residual:", x[["residual"]])
    cat("\nNote: Overall = [main effect] + [other | main effect] + Residual, exactly\n")
  } else if (x$decompose == "partial") {
    cat("Partial (order-independent, adjusted) tests -- NOT additive:\n")
    fmt("Rows | Cols:", x[["rows_given_cols"]]); fmt("Cols | Rows:", x[["cols_given_rows"]])
    fmt("Residual:", x[["residual"]])
  } else if (x$decompose == "symmetric") {
    cat("Symmetric averaged attribution (Shapley-style) -- additive, descriptive only:\n")
    cat(sprintf("  %-20s statistic = %8.4f (df = %d, no exact chi-squared reference)\n",
                "Rows (avg):", x[["rows"]]$statistic, x[["rows"]]$df))
    cat(sprintf("  %-20s statistic = %8.4f (df = %d, no exact chi-squared reference)\n",
                "Cols (avg):", x[["cols"]]$statistic, x[["cols"]]$df))
    fmt("Residual:", x[["residual"]])
    cat("\nNote: Overall = Rows(avg) + Cols(avg) + Residual, exactly\n")
  }
  invisible(x)
}
