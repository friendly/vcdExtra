# Test script for color_table2.R (S3 prototype)
# Run this script to test the S3 refactored color_table function

# Load required packages
library(gt)
library(scales)
library(vcd)

# Source the S3 prototype
source("dev/color_table2.R")

# ============================================================================
# Test 1: table method (basic 2-way table)
# ============================================================================

cat("Test 1: table method (2-way table)\n")

data(HairEyeColor)
HEC <- margin.table(HairEyeColor, 2:1)  # Eye x Hair
print(class(HEC))  # Should be "table"

color_table(HEC, title = "Test 1: table method - Hair x Eye")

# ============================================================================
# Test 2: table method with xtabs (also returns "table" class)
# ============================================================================

cat("\nTest 2: xtabs (should use table method)\n")

data(Suicide, package = "vcd")
Suicide2 <- xtabs(Freq ~ sex + method, data = Suicide)
print(class(Suicide2))  # Should be c("xtabs", "table")

color_table(Suicide2, title = "Test 2: xtabs - Suicide: Sex x Method")

# ============================================================================
# Test 3: table method with formula (multi-way table)
# ============================================================================

cat("\nTest 3: table method with formula (multi-way)\n")

color_table(HairEyeColor,
            formula = Eye ~ Hair + Sex,
            title = "Test 3: multi-way table with formula")

# ============================================================================
# Test 4: ftable method
# ============================================================================

cat("\nTest 4: ftable method\n")

ft <- ftable(Titanic, row.vars = 1:2, col.vars = 3:4)
print(class(ft))  # Should be "ftable"

color_table(ft, title = "Test 4: ftable - Titanic")

# ============================================================================
# Test 5: structable method
# ============================================================================

cat("\nTest 5: structable method\n")

st <- vcd::structable(Eye ~ Hair + Sex, data = HairEyeColor)
print(class(st))  # Should be "structable"

color_table(st, title = "Test 5: structable - Hair+Sex ~ Eye")

# ============================================================================
# Test 6: data.frame method (frequency form)
# ============================================================================

cat("\nTest 6: data.frame method (frequency form)\n")

# Convert table to data.frame (frequency form)
HEC_df <- as.data.frame(HEC)
print(head(HEC_df))
print(class(HEC_df))  # Should be "data.frame"
print(names(HEC_df))  # Should have Eye, Hair, Freq

color_table(HEC_df, title = "Test 6: data.frame - Hair x Eye (from frequency form)")

# ============================================================================
# Test 7: data.frame with "count" column instead of "Freq"
# ============================================================================

cat("\nTest 7: data.frame with 'count' column\n")

HEC_df2 <- HEC_df
names(HEC_df2)[names(HEC_df2) == "Freq"] <- "count"
print(names(HEC_df2))  # Should have Eye, Hair, count

color_table(HEC_df2, title = "Test 7: data.frame with 'count' column")

# ============================================================================
# Test 8: data.frame with custom freq_col
# ============================================================================

cat("\nTest 8: data.frame with custom freq_col\n")

HEC_df3 <- HEC_df
names(HEC_df3)[names(HEC_df3) == "Freq"] <- "n"
print(names(HEC_df3))

color_table(HEC_df3, freq_col = "n", title = "Test 8: data.frame with freq_col='n'")

# ============================================================================
# Test 9: matrix method
# ============================================================================

cat("\nTest 9: matrix method\n")

HEC_mat <- as.matrix(HEC)
print(class(HEC_mat))  # Should be "matrix" "array"

color_table(HEC_mat, title = "Test 9: matrix - Hair x Eye")

# ============================================================================
# Test 10: default method (should error)
# ============================================================================

cat("\nTest 10: default method (should produce error)\n")

tryCatch({
  color_table(list(a = 1, b = 2))
}, error = function(e) {
  cat("Expected error:", conditionMessage(e), "\n")
})

# ============================================================================
# Test 11: shade = "freq" option
# ============================================================================

cat("\nTest 11: shade by frequency\n")

color_table(HEC, shade = "freq", title = "Test 11: Shaded by frequency")

# ============================================================================
# Test 12: margins = FALSE option
# ============================================================================

cat("\nTest 12: without margins\n")

color_table(HEC, margins = FALSE, title = "Test 12: Without margins")

# ============================================================================
# Test 13: filename argument (save to file)
# ============================================================================

cat("\nTest 13: Save to file\n")

color_table(HEC,
            title = "Test 13: Saved to PNG",
            filename = "dev/test-color_table2-output.png")

cat("Saved to dev/test-color_table2-output.png\n")

# ============================================================================
# Test 14: 3-way data.frame with formula
# ============================================================================

cat("\nTest 14: 3-way data.frame with formula\n")

HEC3_df <- as.data.frame(HairEyeColor)
print(head(HEC3_df))

color_table(HEC3_df,
            formula = Eye ~ Hair + Sex,
            title = "Test 14: 3-way data.frame with formula")

# ============================================================================
# Summary
# ============================================================================

cat("\n\n========================================\n")
cat("All tests completed!\n")
cat("Review the output tables in the Viewer pane.\n")
cat("========================================\n")
