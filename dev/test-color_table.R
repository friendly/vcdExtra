# Test script for color_table()
# Run this script to test the color_table function

# Load required packages
library(gt)
library(scales)
library(vcd)

# Source the function
source("dev/color_table.R")

# ============================================================================
# Example 1: Basic 2-way table - Hair x Eye
# ============================================================================

data(HairEyeColor)
HEC <- margin.table(HairEyeColor, 2:1)  # Eye x Hair
HEC

# Shade by residuals (default) - should match haireye-residual-shading.png
color_table(HEC, title = "Hair color - Eye color data")

# Shade by frequencies
color_table(HEC, shade = "freq", title = "Hair x Eye (shaded by frequency)")

# ============================================================================
# Example 2: 3-way table - Heart data (Sex x Occupation x Disease)
# ============================================================================

data(Heart, package = "vcdExtra")
Heart

# Convert to table form
Heart.tab <- xtabs(Freq ~ Gender + Occup + Disease, data = Heart)

# Use formula to specify layout: Occup as rows, Sex x Disease as columns
# This should approximate SexOccHeart-residual-shading.png
color_table(Heart.tab,
            formula = Occup ~ Gender + Disease,
            title = "Sex, occupation and heart disease")

# ============================================================================
# Example 3: Using a specific model
# ============================================================================

# Fit a model other than independence
#library(MASS)
mod_joint <- MASS::loglm(~ Hair + Eye, data = HEC)
mod_joint

# Show residuals from joint independence model
color_table(HEC, model = mod_joint,
            title = "Residuals from independence model")

# ============================================================================
# Example 4: Without margins
# ============================================================================

color_table(HEC, margins = FALSE, title = "Without marginal totals")

# ============================================================================
# Example 5: Custom palette
# ============================================================================

# Use a different diverging palette
color_table(HEC,
            palette = c("purple", "white", "orange"),
            title = "Custom color palette")

# ============================================================================
# Example 6: Deviance residuals
# ============================================================================

color_table(HEC, shade = "deviance", title = "Shaded by deviance residuals")

# ============================================================================
# Example 7: UCBAdmissions data
# ============================================================================

data(UCBAdmissions)

# Admit x Gender, collapsed over Dept
UCB <- margin.table(UCBAdmissions, c(1, 2))
color_table(UCB, title = "UC Berkeley Admissions by Gender")

# Full table with Dept
color_table(UCBAdmissions,
            formula = Dept ~ Admit + Gender,
            title = "UC Berkeley Admissions")

# ============================================================================
# Example 8: Larger table - Titanic
# ============================================================================

data(Titanic)
Titanic

# Survival by Class and Sex
Titanic2 <- margin.table(Titanic, c(1, 4))  # Class x Survived
color_table(Titanic2, title = "Titanic: Class x Survival")

# More complex layout
color_table(Titanic,
            formula = Class ~ Sex + Survived,
            title = "Titanic: Class by Sex and Survival")

# ============================================================================
# Example 9: vcd::Suicide - Age x Sex x Method
# ============================================================================

data(Suicide, package = "vcd")
str(Suicide)

# 2-way: Sex x Method
Suicide2 <- xtabs(Freq ~ sex + method, data = Suicide)
color_table(Suicide2, title = "Suicide: Sex x Method")

# 3-way with formula
Suicide3 <- xtabs(Freq ~ age.group + sex + method, data = Suicide)
color_table(Suicide3,
            formula = age.group ~ sex + method,
            title = "Suicide: Age group by Sex and Method")

# ============================================================================
# Example 10: vcd::PreSex - Premarital sex survey
# ============================================================================

data(PreSex, package = "vcd")
str(PreSex)

# Collapse to 2-way: MaritalStatus x PremaritalSex
PreSex2 <- margin.table(PreSex, c(1, 2))
color_table(PreSex2, title = "Premarital Sex: Marital Status x Premarital Sex")

# Full 4-way table with formula
color_table(PreSex,
            formula = MaritalStatus + Gender ~ PremaritalSex + ExtramaritalSex,
            title = "PreSex: Marital Status & Gender by Sexual Behavior")

# ============================================================================
# Example 11: vcdExtra::Abortion - Abortion opinion data
# ============================================================================

data(Abortion, package = "vcdExtra")
str(Abortion)

# 2-way table
Abortion2 <- xtabs(Freq ~ Sex + Status, data = Abortion)
color_table(Abortion2, title = "Abortion Opinion: Sex x Support Status")

# 3-way with Education
Abortion3 <- xtabs(Freq ~ Education + Sex + Status, data = Abortion)
color_table(Abortion3,
            formula = Education ~ Sex + Status,
            title = "Abortion: Education by Sex and Support Status")

# ============================================================================
# Save an example to HTML
# ============================================================================

# gt tables can be saved to HTML
tbl <- color_table(HEC, title = "Hair color - Eye color data")
# gt::gtsave(tbl, "dev/haireye-color_table.html")

cat("\n\nAll tests completed successfully!\n")
cat("Review the output tables in the Viewer pane.\n")
