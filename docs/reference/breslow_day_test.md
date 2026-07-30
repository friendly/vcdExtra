# Breslow-Day Test for Homogeneity of Odds Ratios

Tests for homogeneity of odds ratios across strata in \\2 \times 2
\times k\\ tables (i.e., whether a common odds ratio fits all strata).
Generalized to handle tables of any dimensionality beyond 3. For
4-dimensional tables, optionally provides a two-way decomposition of the
homogeneity test into row effects, column effects, and residual,
analogous to
[`woolf_test()`](https://friendly.github.io/vcdExtra/reference/woolf_test.md)
with `decompose = TRUE`.

## Usage

``` r
breslow_day_test(x, OR = NA, correct = FALSE, decompose = FALSE)

# S3 method for class 'breslow_day_test'
print(x, digits = 4, ...)
```

## Arguments

- x:

  An object of class `"breslow_day_test"`

- OR:

  The common odds ratio to test against. If `NA` (the default), the
  Mantel-Haenszel estimate is used.

- correct:

  Logical. If `TRUE`, the Tarone (1985) correction is applied. Defaults
  to `FALSE`.

- decompose:

  Logical. If `TRUE` and `x` is 4-dimensional (a \\2 \times 2 \times R
  \times C\\ table), the test is decomposed into row effects, column
  effects, and residual. Defaults to `FALSE`. Ignored for
  non-4-dimensional tables.

- digits:

  Number of significant digits for the common OR. Default 4.

- ...:

  Additional arguments (currently unused).

## Value

A list of class `"breslow_day_test"` (also inheriting from `"htest"`)
containing:

- statistic:

  the chi-squared test statistic.

- parameter:

  degrees of freedom.

- p.value:

  \\p\\-value.

- method:

  character string describing the test.

- data.name:

  character string giving the name of the data.

- or_vars:

  names of the first two dimensions (the 2x2 table variables).

- strata_vars:

  names of the stratifying variables (dimensions 3 and beyond).

- OR:

  the common odds ratio used (MH estimate if `OR = NA` was supplied).

- correct:

  logical indicating whether Tarone correction was applied.

- observed:

  observed \\a_j\\ counts (cell \\\[1,1\]\\ of each stratum).

- expected:

  expected \\\tilde{a}\_j\\ counts under the common OR.

- decomposed:

  logical indicating if decomposition was performed.

When `decompose = TRUE` (only for 4-dimensional tables), additional
components:

- rows:

  list with `statistic`, `df`, `p.value` for row effects.

- cols:

  list with `statistic`, `df`, `p.value` for column effects.

- residual:

  list with `statistic`, `df`, `p.value` for residual (interaction).

## Details

The Breslow-Day test (Breslow & Day, 1980) tests the hypothesis that a
common odds ratio \\\psi\\ fits all \\k\\ strata. Given a common OR (by
default the Mantel-Haenszel estimate), the expected cell count
\\\tilde{a}\_j\\ in cell \\\[1,1\]\\ of stratum \\j\\ is found by
solving the quadratic:

\$\$(\psi - 1)\tilde{a}\_j^2 - \[n\_{2j} - m\_{1j} + \psi(n\_{1j} +
m\_{1j})\]\tilde{a}\_j + \psi \\ m\_{1j} n\_{1j} = 0\$\$

where \\m\_{1j}\\ and \\m\_{2j}\\ are the row margins and \\n\_{1j}\\
and \\n\_{2j}\\ are the column margins of stratum \\j\\. The test
statistic is:

\$\$\chi^2\_{BD} = \sum\_{j=1}^{k} \frac{(a_j -
\tilde{a}\_j)^2}{\widehat{\text{Var}}(a_j)}\$\$

where \\\widehat{\text{Var}}(a_j) = (1/\tilde{a}\_j + 1/\tilde{b}\_j +
1/\tilde{c}\_j + 1/\tilde{d}\_j)^{-1}\\ and \\\tilde{b}\_j,
\tilde{c}\_j, \tilde{d}\_j\\ are the remaining expected cell counts.
Under the null hypothesis, \\\chi^2\_{BD}\\ follows a chi-squared
distribution with \\k - 1\\ degrees of freedom.

The Tarone (1985) correction subtracts a term to account for estimation
of the common OR:

\$\$\chi^2\_{BD,T} = \chi^2\_{BD} - \frac{(\sum_j a_j - \sum_j
\tilde{a}\_j)^2}{\sum_j \widehat{\text{Var}}(a_j)}\$\$

**Comparison with the Woolf test:** The Woolf test uses log odds ratios
and tests deviation from their weighted mean, whereas the Breslow-Day
test works on the cell-count scale against a specified common OR. For
large samples they agree closely; Breslow-Day is generally preferred
when the Mantel-Haenszel common OR is the quantity of interest.

**Decomposition for 4-way tables:** For a \\2 \times 2 \times R \times
C\\ table, when `decompose = TRUE`, the overall test is decomposed as:

\$\$\chi^2\_{\text{Total}} = \chi^2\_{\text{Rows}} +
\chi^2\_{\text{Cols}} + \chi^2\_{\text{Residual}}\$\$

where the row and column components use the row- and column-marginal
(pooled) tables, all tested against the same common OR as the overall
test. The residual is defined by subtraction and has \\(R-1)(C-1)\\
degrees of freedom.

## References

Breslow, N. E. & Day, N. E. (1980). *Statistical Methods in Cancer
Research. Vol. 1: The Analysis of Case-Control Studies*. IARC Scientific
Publications No. 32. Lyon: International Agency for Research on Cancer.

Tarone, R. E. (1985). On heterogeneity tests based on efficient scores.
*Biometrika*, **72**, 91-95.

Lachin, J. M. (2000). *Biostatistical Methods: The Assessment of
Relative Risks*. Wiley, p. 124-125.

## See also

[`stats::mantelhaen.test()`](https://rdrr.io/r/stats/mantelhaen.test.html),
[`woolf_test()`](https://friendly.github.io/vcdExtra/reference/woolf_test.md),
`DescTools::BreslowDayTest()`

Other association tests:
[`CMHtest()`](https://friendly.github.io/vcdExtra/reference/CMHtest.md),
[`GKgamma()`](https://friendly.github.io/vcdExtra/reference/GKgamma.md),
[`HLtest()`](https://friendly.github.io/vcdExtra/reference/HLtest.md),
[`woolf_test()`](https://friendly.github.io/vcdExtra/reference/woolf_test.md),
[`zero.test()`](https://friendly.github.io/vcdExtra/reference/zero.test.md)

## Author

Andri Signorell, Michael Friendly (enhancements)

## Examples

``` r
# 3-way table
data(CoalMiners, package = "vcd")
breslow_day_test(CoalMiners)
#> 
#> Breslow-Day Test on Homogeneity of Odds Ratios 
#> 
#> Data:          CoalMiners 
#> OR variables:  Breathlessness, Wheeze 
#> Strata:        Age 
#> Common OR:     16.33 
#> 
#> X-squared = 26.9457, df = 8, p-value = 0.0007224
breslow_day_test(CoalMiners, correct = TRUE)   # Tarone correction
#> 
#> Breslow-Day Test on Homogeneity of Odds Ratios (with Tarone correction) 
#> 
#> Data:          CoalMiners 
#> OR variables:  Breathlessness, Wheeze 
#> Strata:        Age 
#> Common OR:     16.33 
#> 
#> X-squared = 26.3787, df = 8, p-value = 0.0009045

# Compare with Woolf test
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
breslow_day_test(Heart)
#> 
#> Breslow-Day Test on Homogeneity of Odds Ratios 
#> 
#> Data:          Heart 
#> OR variables:  Disease, Gender 
#> Strata:        Occup 
#> Common OR:     4.622 
#> 
#> X-squared = 98.4871, df = 2, p-value = 0

# 4-way table without decomposition
data(Fungicide, package = "vcdExtra")
breslow_day_test(Fungicide)
#> 
#> Breslow-Day Test on Homogeneity of Odds Ratios 
#> 
#> Data:          Fungicide 
#> OR variables:  group, outcome 
#> Strata:        sex, strain 
#> Common OR:     0.3248 
#> 
#> X-squared = 0.8659, df = 3, p-value = 0.8337

# 4-way table with decomposition
breslow_day_test(Fungicide, decompose = TRUE)
#> 
#> Breslow-Day Test on Homogeneity of Odds Ratios 
#> 
#> Data:          Fungicide 
#> OR variables:  group, outcome 
#> Strata:        sex, strain 
#> Common OR:     0.3248 
#> 
#> Overall homogeneity test:
#>   X-squared = 0.8659, df = 3, p-value = 0.8337
#> 
#> Decomposition:
#>   Rows (sex):    X-squared = 0.0086, df = 1, p-value = 0.9261
#>   Cols (strain): X-squared = 0.8336, df = 1, p-value = 0.3612
#>   Residual:      X-squared = 0.0236, df = 1, p-value = 0.8779
#> 
#> Note: Overall = Rows + Columns + Residual
```
