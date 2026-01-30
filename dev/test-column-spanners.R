# Test script for column spanners feature
# Run this after sourcing the updated color_table.R

library(vcd)
library(gt)
data(PreSex)

# Source the updated function
source("R/color_table.R")

# Test 1: Two column variables
cat("\n=== Test 1: Two column variables ===\n")
cat("Formula: PremaritalSex + ExtramaritalSex ~ MaritalStatus\n\n")
gt1 <- color_table(PreSex,
                   formula = PremaritalSex + ExtramaritalSex ~ MaritalStatus,
                   shade = "freq",
                   title = "Two column vars: PremaritalSex x ExtramaritalSex")
print(gt1)

# Test 2: Two column variables with margins
cat("\n=== Test 2: Two column variables with margins ===\n")
gt2 <- color_table(PreSex,
                   formula = PremaritalSex + ExtramaritalSex ~ MaritalStatus,
                   shade = "freq",
                   margins = TRUE,
                   title = "Two column vars with margins")
print(gt2)

# Test 3: Three column variables
cat("\n=== Test 3: Three column variables ===\n")
cat("Formula: MaritalStatus + PremaritalSex + ExtramaritalSex ~ Gender\n\n")
gt3 <- color_table(PreSex,
                   formula = MaritalStatus + PremaritalSex + ExtramaritalSex ~ Gender,
                   shade = "freq",
                   title = "Three column vars")
print(gt3)

# Test 4: Single column variable (should use old behavior)
cat("\n=== Test 4: Single column variable (baseline) ===\n")
gt4 <- color_table(PreSex,
                   formula = PremaritalSex ~ MaritalStatus + Gender,
                   shade = "freq",
                   title = "Single column var - should work as before")
print(gt4)

# Test 5: Two column variables with residual shading
cat("\n=== Test 5: Two column variables with residual shading ===\n")
gt5 <- color_table(PreSex,
                   formula = PremaritalSex + ExtramaritalSex ~ MaritalStatus,
                   shade = "residuals",
                   title = "Two column vars with residual shading")
print(gt5)

# Test 6: Direct structable input
cat("\n=== Test 6: Direct structable input ===\n")
st <- structable(MaritalStatus + Gender ~ PremaritalSex + ExtramaritalSex, data = PreSex)
gt6 <- color_table(st,
                   shade = "freq",
                   title = "From structable directly")
print(gt6)

cat("\n=== All tests completed ===\n")
cat("Check the Viewer pane for rendered tables.\n")
