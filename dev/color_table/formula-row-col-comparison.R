# Diagnostic script: how does `formula` assign variables to rows vs columns in
# vcd::structable() (what color_table() actually uses internally) compared to
# stats::ftable()?
#
# Motivation: dev/color_table/test-multi-col-stubs.R found that structable()
# and ftable() disagree for "unbalanced" formulas (e.g. 3 variables on one side
# of `~`, 1 on the other) -- see the Titanic `Class + Sex + Age ~ Survived`
# example there, where structable moved `Sex` into the columns alongside
# `Survived` instead of keeping all three LHS variables as rows like ftable
# does. They seemed to agree for "balanced" (2-vs-2) formulas.
#
# This script is NOT a color_table() test -- it's a diagnostic to map out
# exactly when/how structable()'s row/col assignment diverges from a naive
# reading of `lhs ~ rhs`, using every row/col split of HairEyeColor (3 vars:
# Hair, Eye, Sex) and Titanic (4 vars: Class, Sex, Age, Survived). The goal is
# to find the actual rule structable() follows (a guess: it tries to balance
# the number of row vs column *variables*, or maybe *levels*, regardless of
# which side of `~` they were written on) so we can decide how color_table()
# should handle this -- see the 🚩TODO in R/color_table.R.
#
# To run: library(vcd) is enough for structable()/ftable() themselves; you
# only need vcdExtra loaded (library(vcdExtra) or devtools::load_all()) for
# the color_table() calls in the last section.

library(vcd)

data(HairEyeColor)
data(Titanic)

# ==============================================================================
# Helper: for a given formula + data, show what structable() and ftable() each
# decide are the row variables vs column variables, and the resulting shape.
# ==============================================================================

compare_formula <- function(formula, data, label = deparse(formula)) {
  cat("\n", strrep("=", 78), "\n", sep = "")
  cat("formula:", label, "\n")
  cat(strrep("-", 78), "\n")

  st <- vcd::structable(formula, data = data)
  ft <- stats::ftable(formula, data = data)

  st_row <- names(attr(st, "row.vars"))
  st_col <- names(attr(st, "col.vars"))
  ft_row <- names(attr(ft, "row.vars"))
  ft_col <- names(attr(ft, "col.vars"))

  cat("structable(): row.vars = {", paste(st_row, collapse = ", "), "}",
      "  col.vars = {", paste(st_col, collapse = ", "), "}\n")
  cat("ftable()    : row.vars = {", paste(ft_row, collapse = ", "), "}",
      "  col.vars = {", paste(ft_col, collapse = ", "), "}\n")

  agree <- identical(st_row, ft_row) && identical(st_col, ft_col)
  cat("AGREE:", agree, "\n")

  st_mat <- as.matrix(st)
  cat("structable as.matrix() dim:", paste(dim(st_mat), collapse = " x "),
      " (nrow x ncol)\n")

  cat("\n-- structable print --\n")
  print(st)

  cat("\n-- ftable print --\n")
  print(ft)

  invisible(list(structable = st, ftable = ft, agree = agree))
}

# ==============================================================================
# HairEyeColor: 3 variables (Hair, Eye, Sex). All 6 ways to split 2-vs-1.
# ==============================================================================

cat("\n\n##########  HairEyeColor (Hair, Eye, Sex)  ##########\n")

compare_formula(Hair + Sex ~ Eye, HairEyeColor)
compare_formula(Eye ~ Hair + Sex, HairEyeColor)
compare_formula(Hair + Eye ~ Sex, HairEyeColor)
compare_formula(Sex ~ Hair + Eye, HairEyeColor)
compare_formula(Eye + Sex ~ Hair, HairEyeColor)
compare_formula(Hair ~ Eye + Sex, HairEyeColor)

# ==============================================================================
# Titanic: 4 variables (Class, Sex, Age, Survived).
# Balanced 2-vs-2 splits -- confirmed to AGREE in earlier testing
# ==============================================================================

cat("\n\n##########  Titanic -- balanced 2 vs 2  ##########\n")

compare_formula(Class + Sex ~ Age + Survived, Titanic)
compare_formula(Age + Survived ~ Class + Sex, Titanic)
compare_formula(Class + Age ~ Sex + Survived, Titanic)
compare_formula(Sex + Survived ~ Class + Age, Titanic)
compare_formula(Class + Survived ~ Sex + Age, Titanic)
compare_formula(Sex + Age ~ Class + Survived, Titanic)

# ==============================================================================
# Titanic: unbalanced 3-vs-1 splits, all 8 combinations -- this is where
# structable was seen to rebalance (Class + Sex + Age ~ Survived example)
# ==============================================================================

cat("\n\n##########  Titanic -- unbalanced 3 vs 1  ##########\n")

compare_formula(Class + Sex + Age ~ Survived, Titanic)
compare_formula(Survived ~ Class + Sex + Age, Titanic)
compare_formula(Class + Sex + Survived ~ Age, Titanic)
compare_formula(Age ~ Class + Sex + Survived, Titanic)
compare_formula(Class + Age + Survived ~ Sex, Titanic)
compare_formula(Sex ~ Class + Age + Survived, Titanic)
compare_formula(Sex + Age + Survived ~ Class, Titanic)
compare_formula(Class ~ Sex + Age + Survived, Titanic)

# ==============================================================================
# Summary tables: tabulate agree/disagree and what each function actually
# chose for row vars, across all formulas above, for easy scanning.
# ==============================================================================

summarize <- function(formulas, data) {
  rows <- lapply(formulas, function(f) {
    st <- vcd::structable(f, data = data)
    ft <- stats::ftable(f, data = data)
    st_row <- names(attr(st, "row.vars"))
    ft_row <- names(attr(ft, "row.vars"))
    data.frame(
      formula     = deparse(f),
      lhs         = paste(all.vars(f[[2]]), collapse = "+"),
      rhs         = paste(all.vars(f[[3]]), collapse = "+"),
      st_row_vars = paste(st_row, collapse = "+"),
      ft_row_vars = paste(ft_row, collapse = "+"),
      agree       = identical(st_row, ft_row),
      stringsAsFactors = FALSE
    )
  })
  do.call(rbind, rows)
}

formulas_hec <- list(
  Hair + Sex ~ Eye,
  Eye ~ Hair + Sex,
  Hair + Eye ~ Sex,
  Sex ~ Hair + Eye,
  Eye + Sex ~ Hair,
  Hair ~ Eye + Sex
)

formulas_titanic <- list(
  Class + Sex ~ Age + Survived,
  Age + Survived ~ Class + Sex,
  Class + Age ~ Sex + Survived,
  Sex + Survived ~ Class + Age,
  Class + Survived ~ Sex + Age,
  Sex + Age ~ Class + Survived,
  Class + Sex + Age ~ Survived,
  Survived ~ Class + Sex + Age,
  Class + Sex + Survived ~ Age,
  Age ~ Class + Sex + Survived,
  Class + Age + Survived ~ Sex,
  Sex ~ Class + Age + Survived,
  Sex + Age + Survived ~ Class,
  Class ~ Sex + Age + Survived
)

cat("\n\n##########  SUMMARY: HairEyeColor (all 2-vs-1 splits)  ##########\n")
print(summarize(formulas_hec, HairEyeColor), row.names = FALSE)

cat("\n\n##########  SUMMARY: Titanic (all 2-vs-2 and 3-vs-1 splits)  ##########\n")
print(summarize(formulas_titanic, Titanic), row.names = FALSE)

# ==============================================================================
# Visual: render color_table() itself (uses structable() internally) on a
# couple of the disagreement cases, to see what actually gets displayed.
# Requires color_table() to be available -- library(vcdExtra) or
# devtools::load_all() first.
# ==============================================================================

color_table(Titanic, formula = Class + Sex + Age ~ Survived,
            title = "structable-based: Class + Sex + Age ~ Survived")

color_table(Titanic, formula = Class + Age + Survived ~ Sex,
            title = "structable-based: Class + Age + Survived ~ Sex")
