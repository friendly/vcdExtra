# Improved print/summary methods for `vcd::assocstats()` with higher-way tables

## Problem

`vcd::assocstats()` computes association statistics (chi-squared tests, phi, contingency
coefficient, Cramer's V) for two-way tables. For tables with 3+ dimensions, it
applies itself recursively to each 2-way slice via `apply(x, 3:l, FUN = assocstats)`,
returning a named list of `assocstats` objects.

The problem is that this returned list has **no class**, so it prints using the
default list method, producing ugly output like:

```
$`Sex:Male`
                 X^2 df P(> X^2)
Likelihood Ratio 166.20  9  0.00000
Pearson          106.66  9  0.00000

Phi-Coefficient   : NA
Contingency Coeff.: 0.486
Cramer's V        : 0.32

$`Sex:Female`
                 X^2 df P(> X^2)
Likelihood Ratio 116.52  9  0.00000
Pearson           82.60  9  0.00000

Phi-Coefficient   : NA
Contingency Coeff.: 0.454
Cramer's V        : 0.292
```

## Issues

Issues with this output:

1. Uses raw `$name` list notation rather than informative headings
2. No overall header describing the table or the stratifying variable(s)
3. No way to compare statistics across strata at a glance
4. For 4+-way tables, names like `"Var3:level1|Var4:level2"` are hard to parse
5. No `summary()` method that works across strata

## Relevant source code

In `vcd/R/assocstats.R`, the recursive case (lines 2-14) returns a plain
named list/vector with no class:

```r
assocstats <- function(x) {
  if(!is.matrix(x)) {
      l <- length(dim(x))
      str <- apply(x, 3 : l, FUN = assocstats)
      if (l == 3) {
          names(str) <- paste(names(dimnames(x))[3], names(str), sep = ":")
      } else {
          dn <- dimnames(str)
          dim(str) <- NULL
          names(str) <-
              apply(expand.grid(dn), 1,
                    function(x) paste(names(dn), x, sep = ":", collapse = "|"))
      }
      return(str)
  }
  # ... 2-way case returns classed "assocstats" object
}
```

The `print.assocstats()` and `summary.assocstats()` methods only handle the
single 2-way case. There are no methods for the list returned by the recursive case.

## Proposed solution for vcdExtra

Since we don't have maintainer access to `vcd`, we can provide enhanced methods
in `vcdExtra`. There are two main components:

### 1. A wrapper or class for the list result

Create a small wrapper function or S3 class (e.g., `"assocstats_list"`) with
`print()` and `summary()` methods:

```r
# Option A: A wrapper that re-classes the vcd result
assocstats_list <- function(x) {
  result <- vcd::assocstats(x)
  if (is.list(result) && !inherits(result, "assocstats")) {
    class(result) <- "assocstats_list"
    attr(result, "dimnames_orig") <- dimnames(x)
  }
  result
}
```

Alternatively, register `print` and `summary` methods that check for an
unnamed list of `assocstats` objects.

### 2. `print.assocstats_list()` -- formatted stratum-by-stratum output

Replace the raw `$name` list notation with clear headings:

```
## Association Statistics for HairEyeColor
## Stratified by: Sex

--- Sex: Male ---
                 X^2 df P(> X^2)
Likelihood Ratio 166.20  9  0.00000
Pearson          106.66  9  0.00000

Phi-Coefficient   : NA
Contingency Coeff.: 0.486
Cramer's V        : 0.32

--- Sex: Female ---
                 X^2 df P(> X^2)
...
```

### 3. `summary.assocstats_list()` -- comparison table across strata

This is the most valuable addition. Collect key statistics into a data frame
for easy comparison:

```
## Summary of association statistics across strata

                Pearson X^2  df  p-value  Cramer's V  Contingency
Sex:Male             106.66   9  < 0.001       0.320        0.486
Sex:Female            82.60   9  < 0.001       0.292        0.454
```

This could return a data frame (or tibble) of class `"summary.assocstats_list"`
with a custom `print()` method for clean formatting. The data frame structure
also makes it easy for users to further process the results (e.g., plotting
Cramer's V across strata).

### 4. Optional: `as.data.frame()` method

A convenience method to extract the summary statistics as a plain data frame
for downstream use:

```r
as.data.frame.assocstats_list <- function(x, ...) {
  data.frame(
    stratum      = names(x),
    X2_LR        = sapply(x, function(a) a$chisq_tests[1, 1]),
    X2_Pearson   = sapply(x, function(a) a$chisq_tests[2, 1]),
    df           = sapply(x, function(a) a$chisq_tests[2, 2]),
    p_value      = sapply(x, function(a) a$chisq_tests[2, 3]),
    phi          = sapply(x, function(a) a$phi),
    contingency  = sapply(x, function(a) a$contingency),
    cramer_v     = sapply(x, function(a) a$cramer),
    row.names    = NULL
  )
}
```

## Implementation notes

- The wrapper approach avoids any conflict with `vcd` since we define a new class
- All methods would be exported from `vcdExtra` and documented together
- The `summary()` data frame output pairs well with `color_table()` for visual display
- For 4+-way tables, the stratum names from `vcd` (e.g., `"Var3:a|Var4:b"`) should
  be parsed into separate columns in the data frame for clarity
