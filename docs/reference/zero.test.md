# Score test for zero inflation in Poisson data

Carries out a simple score test (van den Broek, 1995) for excess zeros
in an otherwise Poisson distribution of counts. It gives a \\\chi^2_1\\
statistic on one degree of freedom.

## Usage

``` r
zero.test(x)
```

## Arguments

- x:

  A vector of non-negative counts, or a one-way frequency table of such
  counts.

## Value

Returns invisibly a list of three elements:

- `statistic`:

  Value of the test statistic

- `df`:

  Degrees of freedom

- `pvalue`:

  Upper tail p-value

## Details

The test first calculates the rate estimate from the mean,
\\\hat{\lambda} = \bar{x}\\. The number of observed zeros, \\n_0\\ is
then compared with the expected number, \\n \hat{p_0}\\, where
\\\hat{p}\_0=\exp\[-\hat{\lambda}\]\\. Then the test statistic is
calculated by the formula: \$\$\frac{(n_0 -
n\hat{p}\_0)^2}{n\hat{p}\_0(1-\hat{p}\_0) - n\bar{x}\hat{p}\_0^2}\$\$ .
This test statistic has a \\\chi^2_1\\ distribution.

## References

The original R code came from a Stackexchange question,
<https://stats.stackexchange.com/questions/118322/how-to-test-for-zero-inflation-in-a-dataset>

Van den Broek, J. (1995). A Score Test for Zero Inflation in a Poisson
Distribution. *Biometrics*, **51**(2), 738-743.
https://www.jstor.org/stable/2532959

Yang, Zhao, James W. Hardin, and Cheryl L. Addy (2010). Score Tests for
Zero-Inflation in Overdispersed Count Data. *Communications in
Statistics - Theory and Methods* **39** (11) 2008-2030. DOI:
10.1080/03610920902948228

## See also

Other association tests:
[`CMHtest()`](https://friendly.github.io/vcdExtra/reference/CMHtest.md),
[`GKgamma()`](https://friendly.github.io/vcdExtra/reference/GKgamma.md),
[`HLtest()`](https://friendly.github.io/vcdExtra/reference/HLtest.md),
[`woolf_test()`](https://friendly.github.io/vcdExtra/reference/woolf_test.md)

## Author

Michael Friendly

## Examples

``` r
# synthetic tests
zero.test(rpois(100, 1))
#> Score test for zero inflation
#> 
#>      Chi-square = 1.32458 
#>      df = 1
#>      pvalue: 0.24977 
zero.test(rpois(100, 5))
#> Score test for zero inflation
#> 
#>      Chi-square = 0.5101 
#>      df = 1
#>      pvalue: 0.4751 
# add some extra zeros
zero.test(c(rep(0, 20), rpois(100, 5)))
#> Score test for zero inflation
#> 
#>      Chi-square = 178.18317 
#>      df = 1
#>      pvalue: < 2.22e-16 

# Articles by Phd candidates
data(PhdPubs, package="vcdExtra")
zero.test(PhdPubs$articles)
#> Score test for zero inflation
#> 
#>      Chi-square = 133.91825 
#>      df = 1
#>      pvalue: < 2.22e-16 

phd.tab <- table(PhdPubs$articles)
zero.test(phd.tab)
#> Score test for zero inflation
#> 
#>      Chi-square = 133.91825 
#>      df = 1
#>      pvalue: < 2.22e-16 

```
