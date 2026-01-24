# Calculate Goodman-Kruskal Gamma for ordered tables

The Goodman-Kruskal \\\gamma\\ statistic is a measure of association for
ordinal factors in a two-way table proposed by Goodman and Kruskal
(1954).

## Usage

``` r
GKgamma(x, level = 0.95)

# S3 method for class 'GKgamma'
print(x, digits = 3, ...)
```

## Arguments

- x:

  A two-way frequency table, in matrix or table form. The rows and
  columns are considered to be ordinal factors

- level:

  Confidence level for a significance test of \\\gamma \ne =\\

- digits:

  number to digits to use in the print method

- ...:

  other arguments (unused), for conformity with the print generic

## Value

Returns an object of class `"GKgamma"` with 6 components, as follows

- gamma:

  The gamma statistic

- C:

  Total number of concordant pairs in the table

- D:

  Total number of discordant pairs in the table

- sigma:

  Standard error of gamma

- CIlevel:

  Confidence level

- CI:

  Confidence interval

## References

Agresti, A. *Categorical Data Analysis*. John Wiley & Sons, 2002, pp.
57–59.

Goodman, L. A., & Kruskal, W. H. (1954). Measures of association for
cross classifications. *Journal of the American Statistical
Association*, 49, 732-764.

Goodman, L. A., & Kruskal, W. H. (1963). Measures of association for
cross classifications III: Approximate sampling theory. *Journal of the
American Statistical Association*, 58, 310-364.

## See also

[`assocstats`](https://rdrr.io/pkg/vcd/man/assocstats.html),
[Kappa](https://rdrr.io/pkg/vcd/man/Kappa.html)

Other association tests:
[`CMHtest()`](https://friendly.github.io/vcdExtra/reference/CMHtest.md),
[`HLtest()`](https://friendly.github.io/vcdExtra/reference/HLtest.md),
[`woolf_test()`](https://friendly.github.io/vcdExtra/reference/woolf_test.md),
[`zero.test()`](https://friendly.github.io/vcdExtra/reference/zero.test.md)

## Author

Michael Friendly; original version by Laura Thompson

## Examples

``` r
data(JobSat)
GKgamma(JobSat)
#> gamma        : 0.221 
#> std. error   : 0.117 
#> CI           : -0.009 0.451 
```
