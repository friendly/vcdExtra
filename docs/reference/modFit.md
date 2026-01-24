# Brief Summary of Model Fit for a glm or loglm Object

Formats a brief summary of model fit for a `glm` or `loglm` object,
showing the likelihood ratio Chisq (df) value and or AIC. Useful for
inclusion in a plot title or annotation.

## Usage

``` r
modFit(x, ...)

# S3 method for class 'glm'
modFit(x, stats = "chisq", digits = 2, ...)

# S3 method for class 'loglm'
modFit(x, stats = "chisq", digits = 2, ...)
```

## Arguments

- x:

  A `glm` or `loglm` object

- ...:

  Arguments passed down

- stats:

  statistics to print: one or more of `"chisq"`, `"aic"`

- digits:

  number to digits to use in the print method

## Value

A character string containing the formatted values of the chosen
statistics.

## See also

[`LRstats`](https://friendly.github.io/vcdExtra/reference/LRstats.md)

## Author

Michael Friendly

## Examples

``` r
data(Mental)
require(MASS)
(Mental.tab <- xtabs(Freq ~ ses + mental, data=Mental))
#>    mental
#> ses Well Mild Moderate Impaired
#>   1   64   94       58       46
#>   2   57   94       54       40
#>   3   57  105       65       60
#>   4   72  141       77       94
#>   5   36   97       54       78
#>   6   21   71       54       71
(Mental.mod <- loglm(~ses + mental, Mental.tab))
#> Call:
#> loglm(formula = ~ses + mental, data = Mental.tab)
#> 
#> Statistics:
#>                       X^2 df     P(> X^2)
#> Likelihood Ratio 47.41785 15 3.155408e-05
#> Pearson          45.98526 15 5.345771e-05
Mental.mod
#> Call:
#> loglm(formula = ~ses + mental, data = Mental.tab)
#> 
#> Statistics:
#>                       X^2 df     P(> X^2)
#> Likelihood Ratio 47.41785 15 3.155408e-05
#> Pearson          45.98526 15 5.345771e-05
modFit(Mental.mod)
#> [1] "G^2(15)=47.42"

# use to label mosaic()
mosaic(Mental.mod, main=paste("Independence model,", modFit(Mental.mod)))
#> Error in eval(expr, p): object 'Mental.tab' not found
```
