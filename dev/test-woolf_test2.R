# Verification script for dev/woolf_test2.R, before considering it for R/.
#
# Checks:
#   1. decompose="none" agrees with the shipped woolf_test() (incl. LOR/LOR_se)
#   2. decompose="naive" exactly reproduces the shipped woolf_test(decompose=TRUE)
#      (including its known negative-residual bug -- this is a regression check,
#      not an endorsement)
#   3. decompose="sequential" on the issues/woolf.md counterexample matches the
#      hand-verified numbers in that file, for both order="rows" and order="cols"
#   4. decompose="sequential"/"partial"/"symmetric" behave sanely (additivity,
#      nonnegativity where claimed) on Fungicide, Detergent, and DaytonSurvey

library(vcdExtra)  # Fungicide, Detergent, DaytonSurvey, woolf_test()

source("dev/woolf_test2.R")

# ==============================================================================
# 1. decompose="none" vs shipped woolf_test()
# ==============================================================================

data(Fungicide, package = "vcdExtra")
wt1 <- woolf_test(Fungicide)
wt2 <- woolf_test2(Fungicide, decompose = "none")

stopifnot(isTRUE(all.equal(unname(wt1$statistic), unname(wt2$statistic))))
stopifnot(isTRUE(all.equal(wt1$parameter, wt2$parameter, check.attributes = FALSE)))
stopifnot(isTRUE(all.equal(wt1$LOR, wt2$LOR, check.attributes = FALSE)))
stopifnot(isTRUE(all.equal(wt1$LOR_se, wt2$LOR_se, check.attributes = FALSE)))
cat("1. decompose='none' matches woolf_test(): OK\n")

# ==============================================================================
# 2. decompose="naive" vs shipped woolf_test(decompose=TRUE)
# ==============================================================================

wtd <- woolf_test(Fungicide, decompose = TRUE)
wtn <- woolf_test2(Fungicide, decompose = "naive")

stopifnot(isTRUE(all.equal(unname(wtd$rows$statistic), wtn$rows$statistic)))
stopifnot(isTRUE(all.equal(unname(wtd$cols$statistic), wtn$cols$statistic)))
stopifnot(isTRUE(all.equal(unname(wtd$residual$statistic), wtn$residual$statistic)))
cat("2. decompose='naive' matches woolf_test(decompose=TRUE): OK\n")

data(Detergent, package = "vcdExtra")
Detergent2 <- aperm(Detergent, c(3, 2, 1, 4))
wtd2 <- woolf_test(Detergent2, decompose = TRUE)
wtn2 <- woolf_test2(Detergent2, decompose = "naive")
stopifnot(isTRUE(all.equal(unname(wtd2$residual$statistic), wtn2$residual$statistic)))
stopifnot(wtn2$residual$statistic < 0)  # reproduces the known bug
cat("2b. 'naive' reproduces the known negative residual on Detergent: OK\n")
print(wtn2)

# ==============================================================================
# 3. decompose="sequential" on the issues/woolf.md counterexample
# ==============================================================================

x <- array(0, dim = c(2, 2, 3, 2),
           dimnames = list(Group = c("g1", "g2"), Outcome = c("o1", "o2"),
                            R = c("R1", "R2", "R3"), C = c("C1", "C2")))
x[,, "R1", "C1"] <- matrix(c(51, 34, 138, 129), 2)
x[,, "R2", "C1"] <- matrix(c(43, 187, 26, 143), 2)
x[,, "R3", "C1"] <- matrix(c(186, 29, 152, 170), 2)
x[,, "R1", "C2"] <- matrix(c(48, 39, 24, 181), 2)
x[,, "R2", "C2"] <- matrix(c(40, 83, 90, 163), 2)
x[,, "R3", "C2"] <- matrix(c(43, 1, 29, 78), 2)

seq_rows <- woolf_test2(x, decompose = "sequential", order = "rows")
seq_cols <- woolf_test2(x, decompose = "sequential", order = "cols")
print(seq_rows)
print(seq_cols)

expect <- function(actual, target, tol = 1e-2, label) {
  ok <- isTRUE(all.equal(actual, target, tolerance = tol))
  cat(sprintf("  %-14s actual=%.4f  target=%.4f  %s\n", label, actual, target,
              if (ok) "OK" else "MISMATCH"))
  stopifnot(ok)
}

cat("3. Sequential decomposition vs issues/woolf.md worked illustration:\n")
expect(seq_rows$rows$statistic, 54.620, label = "Q_R")
expect(seq_rows$cols_given_rows$statistic, 8.915, label = "Q_{C|R}")
expect(seq_rows$residual$statistic, 21.695, label = "Q_{R:C}")
expect(seq_cols$cols$statistic, 0.183, label = "Q_C")
expect(seq_cols$rows_given_cols$statistic, 63.352, label = "Q_{R|C}")
expect(seq_cols$residual$statistic, 21.695, label = "Q_{R:C} (cols first)")

# additivity, both orders
stopifnot(isTRUE(all.equal(
  seq_rows$rows$statistic + seq_rows$cols_given_rows$statistic + seq_rows$residual$statistic,
  unname(seq_rows$statistic))))
stopifnot(isTRUE(all.equal(
  seq_cols$cols$statistic + seq_cols$rows_given_cols$statistic + seq_cols$residual$statistic,
  unname(seq_cols$statistic))))
cat("  additivity (both orders): OK\n")

# nonnegativity
stopifnot(all(c(seq_rows$rows$statistic, seq_rows$cols_given_rows$statistic,
                seq_rows$residual$statistic, seq_cols$cols$statistic,
                seq_cols$rows_given_cols$statistic) >= -1e-8))
cat("  nonnegativity: OK\n")

# ==============================================================================
# 4. Sanity across real datasets: Fungicide, Detergent, DaytonSurvey-collapsed
# ==============================================================================

check_dataset <- function(tab, label) {
  cat("\n---", label, "---\n")
  for (ord in c("rows", "cols")) {
    r <- woolf_test2(tab, decompose = "sequential", order = ord)
    print(r)
    comps <- if (ord == "rows")
      c(r$rows$statistic, r$cols_given_rows$statistic, r$residual$statistic)
    else
      c(r$cols$statistic, r$rows_given_cols$statistic, r$residual$statistic)
    stopifnot(isTRUE(all.equal(sum(comps), unname(r$statistic))))
    stopifnot(all(comps >= -1e-8))
  }
  p <- woolf_test2(tab, decompose = "partial")
  print(p)
  a <- woolf_test2(tab, decompose = "symmetric")
  print(a)
  stopifnot(isTRUE(all.equal(a$rows$statistic + a$cols$statistic + a$residual$statistic,
                              unname(a$statistic))))
}

check_dataset(Fungicide, "Fungicide")
check_dataset(Detergent2, "Detergent (rearranged)")

data(DaytonSurvey, package = "vcdExtra")
Dayton.tab <- xtabs(Freq ~ cigarette + alcohol + marijuana + sex + race, data = DaytonSurvey)
Dayton4 <- apply(Dayton.tab, c("cigarette", "alcohol", "sex", "race"), sum)
check_dataset(Dayton4, "DaytonSurvey (collapsed over marijuana)")

cat("\nAll dev/woolf_test2.R checks passed.\n")
