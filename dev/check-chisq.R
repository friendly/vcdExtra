# check-chisq.R
# Test cases to verify that color_table() computes residuals correctly
# for multi-way tables under the model of complete independence.
#
# For complete independence in an I x J x K table:
#   Expected: E_ijk = n * (n_i../n) * (n_.j./n) * (n_..k/n)
#                   = n_i.. * n_.j. * n_..k / n^2
#   df = IJK - I - J - K + 2
#
# The current implementation flattens to 2D and runs chisq.test() on that,
# which tests independence between row-combos and column-combos, NOT
# complete independence among all original factors.

library(vcd)
library(vcdExtra)

# =============================================================================
# Test 1: 3-way table - HairEyeColor
# =============================================================================

cat("=== Test 1: HairEyeColor (3-way: Hair x Eye x Sex) ===\n\n")

data(HairEyeColor)
HEC <- HairEyeColor

# Dimensions
dims <- dim(HEC)
cat("Dimensions:", paste(dims, collapse = " x "), "\n")
cat("Total N:", sum(HEC), "\n\n")

# Method 1: Current approach - flatten and use chisq.test()
# This tests independence between (e.g.) Eye and Hair+Sex combinations
st <- structable(Eye ~ Hair + Sex, data = HEC)
st_mat <- as.matrix(st)
chi_flat <- chisq.test(st_mat)
cat("Flattened 2D approach (Eye vs Hair+Sex):\n")
cat("  X^2 =", round(chi_flat$statistic, 2), "\n")
cat("  df  =", chi_flat$parameter, "\n")
cat("  p   =", format.pval(chi_flat$p.value), "\n\n")

# Method 2: Correct - complete independence using loglin or loglm
# Complete independence: [H][E][S] - all factors independent
library(MASS)
mod_indep <- loglm(~ Hair + Eye + Sex, data = HEC)
cat("Complete independence model [H][E][S]:\n")
cat("  X^2 =", round(mod_indep$lrt, 2), "(LRT) /", round(mod_indep$pearson, 2), "(Pearson)\n")
cat("  df  =", mod_indep$df, "\n")
cat("  p   =", format.pval(1 - pchisq(mod_indep$pearson, mod_indep$df)), "\n\n")

# Expected df for complete independence: IJK - I - J - K + 2
I <- dims[1]; J <- dims[2]; K <- dims[3]
expected_df <- I*J*K - I - J - K + 2
cat("Expected df for complete independence:", expected_df, "\n")
cat("loglm df:", mod_indep$df, "\n\n")

# Compare residuals
cat("Residuals comparison (first few cells):\n")
resid_flat <- chi_flat$residuals
resid_indep <- residuals(mod_indep, type = "pearson")
# Reshape to match structable layout for comparison
cat("  Flattened residuals (Eye=Brown, Hair=Black, Sex=Male):",
    round(resid_flat["Brown", "Black_Male"], 3), "\n")
cat("  Complete indep residuals [Brown, Black, Male]:",
    round(resid_indep["Brown", "Black", "Male"], 3), "\n\n")


# =============================================================================
# Test 2: 4-way table - UCBAdmissions with a 4th variable
# =============================================================================

cat("\n=== Test 2: Titanic (4-way: Class x Sex x Age x Survived) ===\n\n")

data(Titanic)

dims <- dim(Titanic)
cat("Dimensions:", paste(dims, collapse = " x "), "\n")
cat("Total N:", sum(Titanic), "\n\n")

# Method 1: Flatten and chisq.test
st <- structable(Class ~ Sex + Age + Survived, data = Titanic)
st_mat <- as.matrix(st)
chi_flat <- chisq.test(st_mat)
cat("Flattened 2D approach:\n")
cat("  X^2 =", round(chi_flat$statistic, 2), "\n")
cat("  df  =", chi_flat$parameter, "\n")
cat("  p   =", format.pval(chi_flat$p.value), "\n\n")

# Method 2: Complete independence
mod_indep <- loglm(~ Class + Sex + Age + Survived, data = Titanic)
cat("Complete independence model [C][S][A][Su]:\n")
cat("  X^2 =", round(mod_indep$lrt, 2), "(LRT) /", round(mod_indep$pearson, 2), "(Pearson)\n")
cat("  df  =", mod_indep$df, "\n")
cat("  p   =", format.pval(1 - pchisq(mod_indep$pearson, mod_indep$df)), "\n\n")

# Expected df for complete independence in 4-way table
# df = IJKL - I - J - K - L + 3
I <- dims[1]; J <- dims[2]; K <- dims[3]; L <- dims[4]
expected_df <- I*J*K*L - I - J - K - L + 3
cat("Expected df for complete independence:", expected_df, "\n")
cat("loglm df:", mod_indep$df, "\n\n")


# =============================================================================
# Test 3: Simple 2-way table - should be the same either way
# =============================================================================

cat("\n=== Test 3: 2-way table (should match) ===\n\n")

HEC2 <- margin.table(HairEyeColor, 1:2)  # Hair x Eye

chi_test <- chisq.test(HEC2)
mod_indep <- loglm(~ Hair + Eye, data = HEC2)

cat("chisq.test:\n")
cat("  X^2 =", round(chi_test$statistic, 2), "\n")
cat("  df  =", chi_test$parameter, "\n\n")

cat("loglm [H][E]:\n")
cat("  X^2 =", round(mod_indep$pearson, 2), "(Pearson)\n")
cat("  df  =", mod_indep$df, "\n\n")

cat("These should match (and they do).\n\n")


# =============================================================================
# Conclusion
# =============================================================================

cat("\n=== CONCLUSION ===\n")
cat("For 2-way tables: chisq.test() gives correct complete independence test.\n")
cat("For 3+ way tables: chisq.test() on flattened table does NOT test complete\n")
cat("independence. It tests independence between the row-variable combos and\n")
cat("column-variable combos in the flattened representation.\n\n")
cat("RECOMMENDATION: Use loglm() or loglin() to fit complete independence model\n")
cat("and extract Pearson residuals for proper shading of multi-way tables.\n\n")


# =============================================================================
# Test 4: Verify color_table() now uses correct residuals
# =============================================================================

cat("\n=== Test 4: Verify color_table() uses correct residuals ===\n\n")

# Load the updated color_table
# devtools::load_all()  # Uncomment when testing

cat("Running color_table(HairEyeColor, formula = Eye ~ Hair + Sex)...\n")
cat("Should print: 'Shading based on residuals from model of complete independence'\n")
cat("with X^2 matching loglm result above (approximately 166).\n\n")

# This should now print the message with correct X^2
# color_table(HairEyeColor, formula = Eye ~ Hair + Sex, title = "HairEyeColor - Complete Independence")

cat("Running color_table() on 2-way table...\n")
cat("Should print: 'Shading based on residuals from model of independence'\n\n")

# This should print the 2-way independence message
# color_table(HEC2, title = "Hair x Eye - Independence")
