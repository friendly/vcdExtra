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

## Details

The Woolf test (Woolf, 1955) tests the hypothesis that the odds ratios
\\\theta_i\\ are equal across all \\k\\ strata. The test statistic is
computed as the weighted sum of squared deviations of the log odds
ratios from their weighted mean:

\$\$\chi^2_W = \sum\_{i=1}^{k} w_i \[\log(\theta_i) -
\log(\bar{\theta}\_w)\]^2 = \sum\_{i=1}^{k} w_i \log^2(\theta_i /
\bar{\theta}\_w)\$\$

where \\\theta_i = (n\_{11i} n\_{22i}) / (n\_{12i} n\_{21i})\\ is the
odds ratio in stratum \\i\\, and \\\bar{\theta}\_w\\ is the weighted
average odds ratio (computed as the exponential of the weighted mean of
the log odds ratios).

The weights \\w_i\\ are the inverse variances of the log odds ratios:
\$\$w_i = 1 / \text{Var}(\log \theta_i) = 1 / (1/n\_{11i} + 1/n\_{12i} +
1/n\_{21i} + 1/n\_{22i})\$\$

Under the null hypothesis of homogeneous odds ratios, \\\chi^2_W\\
follows a chi-squared distribution with \\k - 1\\ degrees of freedom.

**Decomposition for 4-way tables:** For a \\2 \times 2 \times R \times
C\\ table, the strata form an \\R \times C\\ two-way layout with odds
ratios \\\theta\_{ij}\\ for row \\i\\ and column \\j\\. This suggests
that overall Woolf test of homogeneity can be decomposed into three
components, conceptually analogous to a two-way ANOVA with one
observation per cell:

\$\$\chi^2\_{\text{W:Total}} = \chi^2\_{\text{W:Rows}} +
\chi^2\_{\text{W:Cols}} + \chi^2\_{\text{W:Residual}}\$\$

where:

- \\\chi^2\_{\text{W:Rows}} \\ tests whether the odds ratios differ
  among row levels (pooling over columns), with \\R - 1\\ df

- \\\chi^2\_{\text{W:Cols}}\\ tests whether the odds ratios differ among
  column levels (pooling over rows), with \\C - 1\\ df

- \\\chi^2\_{\text{W:Residual}}\\ tests the row \\\times\\ column
  interaction (deviation from additivity on the log odds scale), with
  \\(R-1)(C-1)\\ df

The row effect test compares the marginal log odds ratios \\\log
\bar{\theta}\_{i+}\\ (pooled over columns) to the overall weighted mean.
Similarly, the column effect test compares \\\log \bar{\theta}\_{+j}\\
(pooled over rows). The residual tests whether the cell log odds ratios
\\\log \theta\_{ij}\\ are additive in the row and column effects.

**Note:** The two-way ANOVA-like decomposition for 4-dimensional tables
appears to be a novel extension introduced in this package. The existing
literature on the Woolf test (and related Breslow-Day test) treats
strata as unstructured, testing only whether all \\k\\ odds ratios are
equal. This decomposition exploits the factorial structure of \\R \times
C\\ strata to provide more detailed insight into the sources of
heterogeneity.

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
#>   Rows (sex):    X-squared = 0.0086, df = 1, p-value = 0.9261
#>   Cols (strain): X-squared = 0.8257, df = 1, p-value = 0.3635
#>   Residual:      X-squared = 0.0205, df = 1, p-value = 0.8861
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
