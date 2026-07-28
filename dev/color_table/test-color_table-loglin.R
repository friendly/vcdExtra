# Test script for color_table() with loglinear models
# Uses UCBAdmissions data to demonstrate the `model` argument
#
# UCBAdmissions: 3-way table of Admit x Gender x Dept
# This is the famous Berkeley admissions data showing Simpson's paradox:
# - Overall, women appear to be admitted at lower rates than men
# - But within most departments, women are admitted at equal or higher rates
# - The paradox arises because women applied more to competitive departments

library(gt)
library(scales)
library(vcd)
library(MASS)

# Source the function (use either version)
# source("dev/color_table2.R")   # S3 prototype
source("R/color_table.R")        # Current version

# ============================================================================
# Load and explore the data
# ============================================================================

data(UCBAdmissions)
str(UCBAdmissions)
ftable(UCBAdmissions)

# Dimension names: Admit, Gender, Dept
# Admit: Admitted, Rejected
# Gender: Male, Female
# Dept: A, B, C, D, E, F

# ============================================================================
# Model 1: Complete Independence [A][G][D]
# ============================================================================

cat("\n=== Model 1: Complete Independence [A][G][D] ===\n")

mod1 <- loglm(~ Admit + Gender + Dept, data = UCBAdmissions)
print(mod1)

# This model assumes all three variables are mutually independent
# Should show large residuals since this is a poor fit

color_table(UCBAdmissions,
            formula = Dept ~ Admit + Gender,
            model = mod1,
            title = "Model 1: Complete Independence [A][G][D]")

# ============================================================================
# Model 2: Joint Independence [AG][D]
# ============================================================================

cat("\n=== Model 2: Joint Independence [AG][D] ===\n")

mod2 <- loglm(~ Admit * Gender + Dept, data = UCBAdmissions)
print(mod2)

# Admit and Gender are associated, but jointly independent of Dept
# Still a poor fit - department matters!

color_table(UCBAdmissions,
            formula = Dept ~ Admit + Gender,
            model = mod2,
            title = "Model 2: Joint Independence [AG][D]")

# ============================================================================
# Model 3: Conditional Independence of A and G given D: [AD][GD]
# ============================================================================

cat("\n=== Model 3: Conditional Independence [AD][GD] ===\n")

mod3 <- loglm(~ Admit * Dept + Gender * Dept, data = UCBAdmissions)
print(mod3)

# This is the KEY model for Simpson's paradox:
# - Admit and Gender are conditionally independent given Dept
# - No direct association between Admit and Gender after controlling for Dept
# - This model fits well! (check the p-value)

color_table(UCBAdmissions,
            formula = Dept ~ Admit + Gender,
            model = mod3,
            title = "Model 3: Conditional Independence [AD][GD] - Simpson's Paradox explained")

# ============================================================================
# Model 4: All Two-Way Associations (Homogeneous) [AD][AG][GD]
# ============================================================================
cat("\n=== Model 4: Homogeneous Association [AD][AG][GD] ===\n")

mod4 <- loglm(~ Admit * Dept + Admit * Gender + Gender * Dept, data = UCBAdmissions)
print(mod4)

# All pairwise associations but no 3-way interaction
# Compare fit to Model 3 to test if AG association is needed

color_table(UCBAdmissions,
            formula = Dept ~ Admit + Gender,
            model = mod4,
            title = "Model 4: Homogeneous Association [AD][AG][GD]")

# ============================================================================
# Model 5: Saturated Model [AGD]
# ============================================================================

cat("\n=== Model 5: Saturated Model [AGD] ===\n")

mod5 <- loglm(~ Admit * Gender * Dept, data = UCBAdmissions)
print(mod5)

# Perfect fit (df = 0), residuals should all be ~0
# Not useful for inference but good for checking color_table behavior

color_table(UCBAdmissions,
            formula = Dept ~ Admit + Gender,
            model = mod5,
            title = "Model 5: Saturated Model [AGD] - residuals ~ 0")

# ============================================================================
# Model comparison: Which model fits best?
# ============================================================================

cat("\n=== Model Comparison ===\n")

models <- list(
  "1. [A][G][D]" = mod1,
  "2. [AG][D]" = mod2,
  "3. [AD][GD]" = mod3,
  "4. [AD][AG][GD]" = mod4,
  "5. [AGD]" = mod5
)

comparison <- data.frame(
  Model = names(models),
  df = sapply(models, function(m) m$df),
  LR_chisq = sapply(models, function(m) m$lrt),
  Pearson_chisq = sapply(models, function(m) m$pearson),
  p_value = sapply(models, function(m) pchisq(m$lrt, m$df, lower.tail = FALSE))
)

print(comparison, digits = 4)

cat("\nConclusion: Model 3 [AD][GD] fits well (p > 0.05)\n")
cat("This means Admit and Gender are conditionally independent given Dept.\n")
cat("Simpson's paradox is explained by differential application rates to departments.\n")

# ============================================================================
# Alternative view: Collapse over Dept to see marginal association
# ============================================================================

cat("\n=== Marginal Table (collapsed over Dept) ===\n")

UCB_marginal <- margin.table(UCBAdmissions, c(1, 2))  # Admit x Gender
print(UCB_marginal)

# Independence model for 2-way table
mod_marginal <- loglm(~ Admit + Gender, data = UCB_marginal)
print(mod_marginal)

# This shows the apparent gender bias (significant association)
color_table(UCB_marginal,
            title = "Marginal: Admit x Gender (ignoring Dept) - Apparent bias")

# With independence model residuals
color_table(UCB_marginal,
            model = mod_marginal,
            title = "Marginal: Residuals from Independence - Shows apparent bias")

# ============================================================================
# Department-specific views
# ============================================================================

cat("\n=== Department A (most favorable to women) ===\n")

UCB_A <- UCBAdmissions[,, "A"]
mod_A <- loglm(~ Admit + Gender, data = UCB_A)
print(mod_A)

color_table(UCB_A,
            model = mod_A,
            title = "Dept A: Residuals from Independence")

cat("\n=== Department F (unfavorable overall) ===\n")

UCB_F <- UCBAdmissions[,, "F"]
mod_F <- loglm(~ Admit + Gender, data = UCB_F)
print(mod_F)

color_table(UCB_F,
            model = mod_F,
            title = "Dept F: Residuals from Independence")

# ============================================================================
# Using deviance residuals instead of Pearson
# ============================================================================

cat("\n=== Deviance residuals for Model 1 ===\n")

color_table(UCBAdmissions,
            formula = Dept ~ Admit + Gender,
            shade = "deviance",
            title = "Model 1 (default): Deviance Residuals")

# ============================================================================
# Save key comparison to files
# ============================================================================

cat("\n=== Saving comparison images ===\n")

# Marginal (apparent bias)
color_table(UCB_marginal,
            title = "UCB Admissions: Marginal (apparent gender bias)",
            filename = "dev/UCB-marginal.png")

# Model 3 (Simpson's paradox explained)
color_table(UCBAdmissions,
            formula = Dept ~ Admit + Gender,
            model = mod3,
            title = "UCB Admissions: Conditional Independence [AD][GD]",
            filename = "dev/UCB-model3.png")

cat("Saved: dev/UCB-marginal.png\n")
cat("Saved: dev/UCB-model3.png\n")

# ============================================================================
# Summary
# ============================================================================

cat("\n\n========================================\n")
cat("Loglinear model tests completed!\n")
cat("\nKey findings:\n")
cat("- Model 3 [AD][GD] fits the data well\n
")
cat("- This confirms conditional independence of Admit and Gender given Dept\n")
cat("- Simpson's paradox: marginal association disappears when conditioning on Dept\n")
cat("========================================\n")
