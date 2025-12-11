# Cross-classification of job satisfaction by income

This data set is a contingency table of job satisfaction by income for a
small sample of black males from the 1996 General Social Survey, as used
by Agresti (2002) for an example.

## Format

A 4 x 4 contingency table of `income` by `satisfaction`, with the
following structure:

      table [1:4, 1:4] 1 2 1 0 3 3 6 1 10 10 ...
      - attr(*, "dimnames")=List of 2
      ..$ income      : chr [1:4] "< 15k" "15-25k" "25-40k" "> 40k"
      ..$ satisfaction: chr [1:4] "VeryD" "LittleD" "ModerateS" "VeryS"

## Source

Agresti, A. Categorical Data Analysis John Wiley & Sons, 2002, Table
2.8, p. 57.

## Details

Both `income` and `satisfaction` are ordinal variables, and are so
ordered in the table. Measures of association, visualizations, and
models should take ordinality into account.

## Examples

``` r
data(JobSat)
assocstats(JobSat)
#>                     X^2 df P(> X^2)
#> Likelihood Ratio 6.7641  9  0.66167
#> Pearson          5.9655  9  0.74336
#> 
#> Phi-Coefficient   : NA 
#> Contingency Coeff.: 0.242 
#> Cramer's V        : 0.144 
GKgamma(JobSat)
#> gamma        : 0.221 
#> std. error   : 0.117 
#> CI           : -0.009 0.451 
```
