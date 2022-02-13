# test the fix to CMHtest

# libraries
library(tidyverse)
library(vcdExtra)

# input patched file
source("issues/CMHtest-new.R")

# data input
r_cmh <- tibble::tribble(
  ~x,    ~k,    ~y,   ~Freq,
  "low", "yes", "yes",    11,
  "low", "yes",  "no",    43,
  "low",  "no", "yes",    42,
  "low",  "no",  "no",   169,
  "med", "yes", "yes",    14,
  "med", "yes",  "no",   104,
  "med",  "no", "yes",    20,
  "med",  "no",  "no",   132,
  "high", "yes", "yes",     8,
  "high", "yes",  "no",   196,
  "high",  "no", "yes",     2,
  "high",  "no",  "no",    59
)

# CMH analysis: OK
CMHtest(Freq~x+y|k, data=r_cmh, overall=TRUE, details=TRUE)$ALL$table

## Test 1 - types = "ALL" [OK]
# In this test case, we repeat the analysis but specify the `types` parameter to be equal to `ALL`. 
# According to the package documentation, this should again produce all 3 (and a fourth) sets of results.

CMHtest(Freq~x+y|k, data=r_cmh, overall=TRUE, details=TRUE, types = "ALL")$ALL$table

## Test 2 - types = "rmeans" [Not OK]
# In this test case, the `row means` test statistic is of particular interest and specify it directly.

CMHtest(Freq~x+y|k, data=r_cmh, overall=TRUE, details=TRUE, types = "rmeans")$ALL$table

# The df and p-value are still returned as NA.

# Chisq    Df Prob
# rmeans 26.02779 NA NA  

## Test 3 - random order [OK]
# In this final example, we chose random orders when requesting which test statistics should be computed.

# Order: cor, general, rmeans
CMHtest(Freq~x+y|k, data=r_cmh, overall=TRUE, details=TRUE, types=c("cor","general","rmeans"))$ALL$table

# Order: rmeans, general, cor
CMHtest(Freq~x+y|k, data=r_cmh, overall=TRUE, details=TRUE, types=c("rmeans","general","cor"))$ALL$table


