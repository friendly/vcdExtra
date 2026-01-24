# Brief Summary of Model Fit for glm and loglm Models

For `glm` objects, the `print` and `summary` methods give too much
information if all one wants to see is a brief summary of model goodness
of fit, and there is no easy way to display a compact comparison of
model goodness of fit for a collection of models fit to the same data.
All `loglm` models have equivalent glm forms, but the `print` and
`summary` methods give quite different results.

## Usage

``` r
Summarise(object, ...)

# S3 method for class 'glmlist'
Summarise(object, ..., saturated = NULL, sortby = NULL)

# S3 method for class 'loglmlist'
Summarise(object, ..., saturated = NULL, sortby = NULL)

# Default S3 method
Summarise(object, ..., saturated = NULL, sortby = NULL)
```

## Arguments

- object:

  a fitted model object for which there exists a logLik method to
  extract the corresponding log-likelihood

- ...:

  optionally more fitted model objects

- saturated:

  saturated model log likelihood reference value (use 0 if deviance is
  not available)

- sortby:

  either a numeric or character string specifying the column in the
  result by which the rows are sorted (in decreasing order)

## Value

A data frame (also of class `anova`) with columns
`c("AIC", "BIC", "LR Chisq", "Df", "Pr(>Chisq)")`. Row names are taken
from the names of the model object(s).

## Details

`Summarise` provides a brief summary for one or more models fit to the
same dataset for which `logLik` and `nobs` methods exist (e.g., `glm`
and `loglm` models).

The function relies on residual degrees of freedom for the LR chisq test
being available in the model object. This is true for objects inheriting
from `lm`, `glm`, `loglm`, `polr` and `negbin`.

## See also

[`logLik`](https://rdrr.io/r/stats/logLik.html),
[`glm`](https://rdrr.io/r/stats/glm.html),
[`loglm`](https://rdrr.io/pkg/MASS/man/loglm.html),

[`logLik.loglm`](https://friendly.github.io/vcdExtra/reference/logLik.loglm.md),
[`modFit`](https://friendly.github.io/vcdExtra/reference/modFit.md),
[`LRstats`](https://friendly.github.io/vcdExtra/reference/LRstats.md)

## Author

Achim Zeileis

## Examples

``` r
data(Mental)
indep <- glm(Freq ~ mental+ses,
                family = poisson, data = Mental)
Summarise(indep)
#> Likelihood summary table:
#>          AIC    BIC LR Chisq Df Pr(>Chisq)    
#> indep 209.59 220.19   47.418 15  3.155e-05 ***
#> ---
#> Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
Cscore <- as.numeric(Mental$ses)
Rscore <- as.numeric(Mental$mental)

coleff <- glm(Freq ~ mental + ses + Rscore:ses,
                family = poisson, data = Mental)
roweff <- glm(Freq ~ mental + ses + mental:Cscore,
                family = poisson, data = Mental)
linlin <- glm(Freq ~ mental + ses + Rscore:Cscore,
                family = poisson, data = Mental)

# compare models
Summarise(indep, coleff, roweff, linlin)
#> Likelihood summary table:
#>           AIC    BIC LR Chisq Df Pr(>Chisq)    
#> indep  209.59 220.19   47.418 15  3.155e-05 ***
#> coleff 179.00 195.50    6.829 10     0.7415    
#> roweff 174.45 188.59    6.281 12     0.9013    
#> linlin 174.07 185.85    9.895 14     0.7698    
#> ---
#> Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

```
