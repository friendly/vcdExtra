# Brief Summary of Model Fit for glm and loglm Models

For `glm` objects, the `print` and `summary` methods give too much
information if all one wants to see is a brief summary of model goodness
of fit, and there is no easy way to display a compact comparison of
model goodness of fit for a collection of models fit to the same data.
All `loglm` models have equivalent glm forms, but the `print` and
`summary` methods give quite different results.

## Usage

``` r
LRstats(object, ...)

# S3 method for class 'glmlist'
LRstats(
  object,
  ...,
  saturated = NULL,
  sortby = NULL,
  label = c("name", "formula"),
  label.args = list()
)

# S3 method for class 'loglmlist'
LRstats(
  object,
  ...,
  saturated = NULL,
  sortby = NULL,
  label = c("name", "formula"),
  label.args = list()
)

# Default S3 method
LRstats(
  object,
  ...,
  saturated = NULL,
  sortby = NULL,
  label = c("name", "formula"),
  label.args = list()
)
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

- label:

  character string specifying how to label the rows: `"name"` (default)
  uses the model object names; `"formula"` uses model formulas or
  bracket notation obtained from
  [`get_models`](https://friendly.github.io/vcdExtra/reference/glmlist.md)
  (for `glmlist` and `loglmlist` objects) or
  [`get_model`](https://friendly.github.io/vcdExtra/reference/get_model.md)
  (for individual model objects passed to the default method).

- label.args:

  a list of additional arguments passed to
  [`get_models`](https://friendly.github.io/vcdExtra/reference/glmlist.md)
  when `label = "formula"`. Useful arguments include `abbrev` (logical
  or integer) to abbreviate factor names and `sep` to change the
  separator in bracket notation.

## Value

A data frame (also of class `anova`) with columns
` c("AIC", "BIC", "LR Chisq", "Df", "Pr(>Chisq)")`. Row names are taken
from the names of the model object(s) or their model formulas.

## Details

`LRstats` provides a brief summary for one or more models fit to the
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
[`get_models`](https://friendly.github.io/vcdExtra/reference/glmlist.md),
[`get_model`](https://friendly.github.io/vcdExtra/reference/get_model.md)

Other glmlist functions:
[`Kway()`](https://friendly.github.io/vcdExtra/reference/Kway.md),
[`glmlist()`](https://friendly.github.io/vcdExtra/reference/glmlist.md),
[`mosaic.glmlist()`](https://friendly.github.io/vcdExtra/reference/mosaic.glmlist.md)

## Author

Achim Zeileis, Michael Friendly

## Examples

``` r
data(Mental)
indep <- glm(Freq ~ mental+ses,
                family = poisson, data = Mental)
LRstats(indep)
#> Likelihood summary table:
#>          AIC    BIC LR Chisq Df Pr(>Chisq)    
#> indep 209.59 220.19   47.418 15  3.155e-05 ***
#> ---
#> Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
Cscore <- as.numeric(Mental$ses)
Rscore <- as.numeric(Mental$mental)

coleff <- glm(Freq ~ mental + ses + Rscore:ses,
                family = poisson, data = Mental)
roweff <- glm(Freq ~ mental + ses + mental:Cscore,
                family = poisson, data = Mental)
linlin <- glm(Freq ~ mental + ses + Rscore:Cscore,
                family = poisson, data = Mental)

# compare models using object names (default)
LRstats(indep, coleff, roweff, linlin)
#> Likelihood summary table:
#>           AIC    BIC LR Chisq Df Pr(>Chisq)    
#> indep  209.59 220.19   47.418 15  3.155e-05 ***
#> coleff 179.00 195.50    6.829 10     0.7415    
#> roweff 174.45 188.59    6.281 12     0.9013    
#> linlin 174.07 185.85    9.895 14     0.7698    
#> ---
#> Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

# compare models in a glmlist, using formula labels
mods <- glmlist(indep, coleff, roweff, linlin)
LRstats(mods, label = "formula")
#> Likelihood summary table:
#>                                 AIC    BIC LR Chisq Df Pr(>Chisq)    
#> mental + ses                 209.59 220.19   47.418 15  3.155e-05 ***
#> mental + ses + Rscore:ses    179.00 195.50    6.829 10     0.7415    
#> mental + ses + mental:Cscore 174.45 188.59    6.281 12     0.9013    
#> mental + ses + Rscore:Cscore 174.07 185.85    9.895 14     0.7698    
#> ---
#> Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

# loglmlist example with bracket notation labels
data(Titanic)
tit.joint <- seq_loglm(Titanic, type = "joint")
LRstats(tit.joint)
#> Likelihood summary table:
#>            AIC    BIC LR Chisq Df Pr(>Chisq)    
#> joint.1 509.95 509.33   475.81  3  < 2.2e-16 ***
#> joint.2 478.75 479.14   412.60  3  < 2.2e-16 ***
#> joint.3 257.88 264.83   159.10  7  < 2.2e-16 ***
#> joint.4 833.36 858.28   671.96 15  < 2.2e-16 ***
#> ---
#> Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
LRstats(tit.joint, label = "formula")
#> Likelihood summary table:
#>                               AIC    BIC LR Chisq Df Pr(>Chisq)    
#> = Class                    509.95 509.33   475.81  3  < 2.2e-16 ***
#> (Class) (Sex)              478.75 479.14   412.60  3  < 2.2e-16 ***
#> (Class,Sex) (Age)          257.88 264.83   159.10  7  < 2.2e-16 ***
#> [Class,Sex,Age] [Survived] 833.36 858.28   671.96 15  < 2.2e-16 ***
#> ---
#> Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
LRstats(tit.joint, label = "formula", label.args = list(abbrev = TRUE))
#> Likelihood summary table:
#>                AIC    BIC LR Chisq Df Pr(>Chisq)    
#> = Class     509.95 509.33   475.81  3  < 2.2e-16 ***
#> (C) (S)     478.75 479.14   412.60  3  < 2.2e-16 ***
#> (C,S) (A)   257.88 264.83   159.10  7  < 2.2e-16 ***
#> [C,S,A] [S] 833.36 858.28   671.96 15  < 2.2e-16 ***
#> ---
#> Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
```
