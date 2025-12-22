# Woolf Test for Homogeneity of Odds Ratios

Test for homogeneity on \\2 \times 2 \times k\\ tables over strata
(i.e., whether the log odds ratios are the same in all strata).
Generalized to handle tables of any dimensionality beyond 3. For
4-dimensional tables, optionally provides a two-way decomposition of the
homogeneity test into row effects, column effects, and residual.

## Usage

``` r
woolf_test(x, decompose = FALSE)

# S3 method for class 'woolf_test'
print(x, ...)
```

## Arguments

- x:

  An object of class `"woolf_test"`

- decompose:

  Logical. If `TRUE` and `x` is 4-dimensional (a \\2 \times 2 \times R
  \times C\\ table), the test is decomposed into row effects, column
  effects, and residual (interaction). Defaults to `FALSE`. Ignored for
  non-4-dimensional tables.

- ...:

  Additional arguments (currently unused)

## Value

A list of class `"woolf_test"` (also inheriting from `"htest"`)
containing the following components:

- statistic:

  the chi-squared test statistic.

- parameter:

  degrees of freedom of the approximate chi-squared distribution of the
  test statistic.

- p.value:

  \\p\\-value for the test.

- method:

  a character string indicating the type of test performed.

- data.name:

  a character string giving the name of the data.

- or_vars:

  names of the first two dimensions (the 2x2 table variables).

- strata_vars:

  names of the stratifying variables (dimensions 3 and beyond).

- observed:

  the observed log odds ratios.

- expected:

  the expected log odds ratio under the null hypothesis (weighted mean).

- decomposed:

  logical indicating if decomposition was performed.

When `decompose = TRUE` (only for 4-dimensional tables), additional
components:

- rows:

  list with statistic, df, p.value, observed, expected for row effects.

- cols:

  list with statistic, df, p.value, observed, expected for column
  effects.

- residual:

  list with statistic, df, p.value for residual (interaction).

## References

Woolf, B. (1955). On estimating the relation between blood group and
disease. *Annals of Human Genetics*, **19**, 251-253.

## See also

[`mantelhaen.test`](https://rdrr.io/r/stats/mantelhaen.test.html)

Other association tests:
[`CMHtest()`](https://friendly.github.io/vcdExtra/reference/CMHtest.md),
[`GKgamma()`](https://friendly.github.io/vcdExtra/reference/GKgamma.md),
[`HLtest()`](https://friendly.github.io/vcdExtra/reference/HLtest.md),
[`zero.test()`](https://friendly.github.io/vcdExtra/reference/zero.test.md)

## Examples

``` r
# 3-way tables
data(CoalMiners, package = "vcd")
woolf_test(CoalMiners)
#> 
#> Woolf-test on Homogeneity of Odds Ratios (no 3-way association) 
#> 
#> Data:          CoalMiners 
#> OR variables:  Breathlessness, Wheeze 
#> Strata:        Age 
#> 
#> X-squared = 26.2034, df = 8, p-value = 0.0009694

data(Heart, package = "vcdExtra")
woolf_test(Heart)
#> 
#> Woolf-test on Homogeneity of Odds Ratios (no 3-way association) 
#> 
#> Data:          Heart 
#> OR variables:  Disease, Gender 
#> Strata:        Occup 
#> 
#> X-squared = 81.0807, df = 2, p-value = 0

# 4-way table without decomposition
data(Fungicide, package = "vcdExtra")
woolf_test(Fungicide)
#> 
#> Woolf-test on Homogeneity of Odds Ratios (no 4-way association) 
#> 
#> Data:          Fungicide 
#> OR variables:  group, outcome 
#> Strata:        sex, strain 
#> 
#> X-squared = 0.8548, df = 3, p-value = 0.8363

# 4-way table with decomposition
woolf_test(Fungicide, decompose = TRUE)
#> 
#> Woolf-test on Homogeneity of Odds Ratios (no 4-way association) 
#> 
#> Data:          Fungicide 
#> OR variables:  group, outcome 
#> Strata:        sex, strain 
#> 
#> Overall homogeneity test:
#>   X-squared = 0.8548, df = 3, p-value = 0.8363
#> 
#> Decomposition:
#>   Rows (sex):  X-squared = 0.0086, df = 1, p-value = 0.9261
#>   Cols (strain):  X-squared = 0.8257, df = 1, p-value = 0.3635
#>   Residual:       X-squared = 0.0205, df = 1, p-value = 0.8861
#> 
#> Note: Overall = Rows + Columns + Residual

# 4-way table, but need to rearrange dimensions
# How does association between `Preference` and `M_User` vary over strata?
data(Detergent, package = "vcdExtra")
dimnames(Detergent) |> names()
#> [1] "Temperature"    "M_User"         "Preference"     "Water_softness"
Detergent <- aperm(Detergent, c(3, 2, 1, 4))
woolf_test(Detergent)
#> 
#> Woolf-test on Homogeneity of Odds Ratios (no 4-way association) 
#> 
#> Data:          Detergent 
#> OR variables:  Preference, M_User 
#> Strata:        Temperature, Water_softness 
#> 
#> X-squared = 8.0132, df = 5, p-value = 0.1555

```
