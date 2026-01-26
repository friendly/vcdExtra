# Diagnostic test for color_table messages
# Run this after devtools::load_all()

library(vcdExtra)
# Or use: devtools::load_all()

cat("=== Testing color_table messages ===\n\n")

# Test 1: 2-way table
cat("Test 1: 2-way table (HEC2)\n")
cat("Expected: message about 'model of independence'\n")
HEC2 <- margin.table(HairEyeColor, 1:2)
result1 <- color_table(HEC2)
cat("\n")

# Test 2: 3-way table with formula
cat("Test 2: 3-way table with formula\n")
cat("Expected: message about 'model of complete independence'\n")
result2 <- color_table(HairEyeColor, formula = Eye ~ Hair + Sex)
cat("\n")

# Test 3: 3-way table WITHOUT formula
cat("Test 3: 3-way table WITHOUT formula\n")
cat("Expected: message about 'model of complete independence' + 'Use formula for better control'\n")
result3 <- color_table(HairEyeColor)
cat("\n")

# Test 4: shade = "freq" (no message expected)
cat("Test 4: shade = 'freq'\n")
cat("Expected: NO message\n")
result4 <- color_table(HEC2, shade = "freq")
cat("\n")

# Test 5: data.frame input
cat("Test 5: data.frame input\n")
cat("Expected: message about 'model of independence'\n")
hec_df <- as.data.frame(HEC2)
result5 <- color_table(hec_df)
cat("\n")

cat("=== Tests complete ===\n")
