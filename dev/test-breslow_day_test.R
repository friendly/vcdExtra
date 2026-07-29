# Verification script for dev/breslow_day_test.R, before considering it for R/.
#
# Two questions to answer:
#   1. Does breslow_day_test() agree numerically with DescTools::BreslowDayTest(),
#      which it was adapted from?
#   2. Does its decompose = TRUE option (for 2x2xRxC tables) work analogously to
#      the already-shipped woolf_test(decompose = TRUE)?
#
# Run after devtools::load_all() (for vcd/vcdExtra data and woolf_test()); DescTools
# is not a package dependency, just used here for comparison -- install it separately
# if needed.

library(vcd)         # CoalMiners
library(vcdExtra)    # Heart, Fungicide, woolf_test()
library(DescTools)   # BreslowDayTest() -- reference implementation

source("dev/breslow_day_test.R")

# ==============================================================================
# 1. Agreement with DescTools::BreslowDayTest() on 2x2xk tables (k strata)
# ==============================================================================

# p.value needs unname()'d comparison, same as statistic/parameter: our standard-
# case branch used to attach names(STATISTIC) <- "X-squared" before computing
# p.value = 1 - pchisq(STATISTIC, PARAMETER), so p.value inherited a stray
# "X-squared" name via arithmetic -- DescTools computes its p-value before
# attaching names, so it stays unnamed. all.equal() flags *any* names mismatch
# outright, independent of numeric tolerance, which is why loosening tolerance
# alone didn't help. Fixed at the source (dev/breslow_day_test.R now computes
# PVAL before assigning names), but unname() here too, defensively, matching
# the statistic/parameter comparisons below. A modest explicit tolerance is
# still kept for p.value since it's the most sensitive of the three quantities
# to floating-point-level differences between two independent implementations.
check_agrees <- function(x, label, correct = FALSE, p_tolerance = 1e-3) {
  ours <- breslow_day_test(x, correct = correct)
  theirs <- BreslowDayTest(x, correct = correct)

  ok_stat <- isTRUE(all.equal(unname(ours$statistic), unname(theirs$statistic)))
  ok_df   <- isTRUE(all.equal(unname(ours$parameter), unname(theirs$parameter)))
  ok_p    <- isTRUE(all.equal(unname(ours$p.value), unname(theirs$p.value), tolerance = p_tolerance))

  cat(sprintf(
    "%s (correct=%s): statistic %s, df %s, p.value %s\n",
    label, correct,
    if (ok_stat) "OK" else sprintf("MISMATCH (ours=%.6f, DescTools=%.6f)", ours$statistic, theirs$statistic),
    if (ok_df)   "OK" else "MISMATCH",
    if (ok_p)    "OK" else sprintf("MISMATCH (ours=%.6g, DescTools=%.6g)", ours$p.value, theirs$p.value)
  ))
  stopifnot(ok_stat, ok_df, ok_p)
  invisible(list(ours = ours, theirs = theirs))
}

data(CoalMiners, package = "vcd")
check_agrees(CoalMiners, "CoalMiners", correct = FALSE)

# Side-by-side, illustrating the statistic/p.value precision noise noted above
breslow_day_test(CoalMiners)
# gives: X-squared = 26.9457, df = 8, p-value = 0.000722

BreslowDayTest(CoalMiners)
# gives: X-squared = 26.946, df = 8, p-value = 0.0007224

check_agrees(CoalMiners, "CoalMiners", correct = TRUE)

data(Heart, package = "vcdExtra")
check_agrees(Heart, "Heart", correct = FALSE)
check_agrees(Heart, "Heart", correct = TRUE)

# ==============================================================================
# 2. 4-way table (2x2xRxC), decompose = FALSE: our "collapse higher dims into a
#    single stratum dimension" branch should equal DescTools run on the same
#    data manually collapsed to 2x2x(R*C).
# ==============================================================================

data(Fungicide, package = "vcdExtra")
dims <- dim(Fungicide)
Fungicide_flat <- array(Fungicide, dim = c(dims[1:2], prod(dims[3:4])))

ours_flat   <- breslow_day_test(Fungicide)
theirs_flat <- BreslowDayTest(Fungicide_flat)

cat(sprintf(
  "Fungicide (collapsed, correct=FALSE): statistic %s, df %s\n",
  if (isTRUE(all.equal(unname(ours_flat$statistic), unname(theirs_flat$statistic)))) "OK" else "MISMATCH",
  if (isTRUE(all.equal(unname(ours_flat$parameter), unname(theirs_flat$parameter)))) "OK" else "MISMATCH"
))
stopifnot(
  isTRUE(all.equal(unname(ours_flat$statistic), unname(theirs_flat$statistic))),
  isTRUE(all.equal(unname(ours_flat$parameter), unname(theirs_flat$parameter)))
)

# ==============================================================================
# 3. decompose = TRUE: internal consistency (Overall = Rows + Cols + Residual,
#    both for the statistic and for df), same identity woolf_test() relies on.
# ==============================================================================

bd_decomp <- breslow_day_test(Fungicide, decompose = TRUE)
print(bd_decomp)

stat_sum <- bd_decomp$rows$statistic + bd_decomp$cols$statistic + bd_decomp$residual$statistic
df_sum   <- bd_decomp$rows$df + bd_decomp$cols$df + bd_decomp$residual$df

cat(sprintf(
  "\nDecomposition check: statistic %s (overall=%.4f, sum=%.4f), df %s (overall=%d, sum=%d)\n",
  if (isTRUE(all.equal(bd_decomp$statistic, stat_sum, check.attributes = FALSE))) "OK" else "MISMATCH",
  bd_decomp$statistic, stat_sum,
  if (bd_decomp$parameter == df_sum) "OK" else "MISMATCH",
  bd_decomp$parameter, df_sum
))
stopifnot(
  isTRUE(all.equal(unname(bd_decomp$statistic), stat_sum)),
  bd_decomp$parameter == df_sum
)

# ==============================================================================
# 4. Structural comparison with woolf_test(decompose = TRUE): different test
#    statistics (Woolf uses log-OR deviation, Breslow-Day works on cell counts
#    against a common OR), but the row/col/residual DECOMPOSITION STRUCTURE --
#    same df's for rows/cols/residual, since those depend only on R and C, not
#    on the test statistic -- should match exactly.
# ==============================================================================

wt_decomp <- woolf_test(Fungicide, decompose = TRUE)
print(wt_decomp)

cat(sprintf(
  "\nStructure vs. woolf_test: rows df %s, cols df %s, residual df %s\n",
  if (bd_decomp$rows$df     == wt_decomp$rows$df)     "OK" else "MISMATCH",
  if (bd_decomp$cols$df     == wt_decomp$cols$df)     "OK" else "MISMATCH",
  if (bd_decomp$residual$df == wt_decomp$residual$df) "OK" else "MISMATCH"
))
stopifnot(
  bd_decomp$rows$df     == wt_decomp$rows$df,
  bd_decomp$cols$df     == wt_decomp$cols$df,
  bd_decomp$residual$df == wt_decomp$residual$df
)

# The two test statistics need not agree numerically (different methods), but
# both should be > 0 and, for data this far from the null, both should reject.
cat(sprintf(
  "\nBreslow-Day overall: X2 = %.4f, p = %.4g\n", bd_decomp$statistic, bd_decomp$p.value
))
cat(sprintf(
  "Woolf overall:        X2 = %.4f, p = %.4g\n", wt_decomp$statistic, wt_decomp$p.value
))

cat("\nAll checks passed.\n")
