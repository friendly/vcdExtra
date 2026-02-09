# Sequential Loglinear Models for an N-way Table

This function takes an n-way contingency table and fits a series of
sequential models to the 1-, 2-, ... n-way marginal tables,
corresponding to a variety of types of loglinear models.

## Usage

``` r
seq_loglm(
  x,
  type = c("joint", "conditional", "mutual", "markov", "saturated"),
  marginals = 1:nf,
  vorder = 1:nf,
  k = NULL,
  prefix = NULL,
  fitted = TRUE,
  ...
)
```

## Arguments

- x:

  a contingency table in array form, with optional category labels
  specified in the dimnames(x) attribute, or else a data.frame in
  frequency form, with the frequency variable named `"Freq"`.

- type:

  type of sequential model to fit, a character string. One of `"joint"`,
  `"conditional"`, `"mutual"`, `"markov"`, or `"saturated"`.

- marginals:

  which marginal sub-tables to fit? A vector of a (sub)set of the
  integers, `1:nf` where `nf` is the number of factors in the full n-way
  table.

- vorder:

  order of variables, a permutation of the integers `1:nf`, used to
  reorder the variables in the original table for the purpose of fitting
  sequential marginal models.

- k:

  conditioning variable(s) for `type` = `"joint"`, `"conditional"` or
  Markov chain order for `type` = `"markov"`

- prefix:

  prefix used to give names to the sequential models. If `NULL` (the
  default), uses an abbreviation of `type`: `"joint"`, `"cond"`,
  `"mutual"`, `"markov"`, or `"sat"`.

- fitted:

  argument passed to `loglm` to store the fitted values in the model
  objects

- ...:

  other arguments, passed down

## Value

An object of class `"loglmlist"`, each of which is a class `"loglm"`
object

## Details

Sequential marginal models for an n-way tables begin with the model of
equal-probability for the one-way margin (equivalent to a
[`chisq.test`](https://rdrr.io/r/stats/chisq.test.html)) and add
successive variables one at a time in the order specified by `vorder`.

All model types give the same result for the two-way margin, namely the
test of independence for the first two factors.

Sequential models of *joint independence* (`type="joint"`) have a
particularly simple interpretation, because they decompose the
likelihood ratio test for the model of mutual independence in the full
n-way table, and hence account for "total" association in terms of
portions attributable to the conditional probabilities of each new
variable, given all prior variables.

## Note

One-way marginal tables are a bit of a problem here, because they cannot
be fit directly using
[`loglm`](https://rdrr.io/pkg/MASS/man/loglm.html). The present version
uses [`loglin`](https://rdrr.io/r/stats/loglin.html), and repairs the
result to look like a `loglm` object (sort of).

## References

These functions were inspired by the original SAS implementation of
mosaic displays, described in the *User's Guide*,
<http://www.datavis.ca/mosaics/mosaics.pdf>

## See also

[`loglin-utilities`](https://friendly.github.io/vcdExtra/reference/loglin-utilities.md)
for descriptions of sequential models,
[`conditional`](https://friendly.github.io/vcdExtra/reference/loglin-utilities.md),
[`joint`](https://friendly.github.io/vcdExtra/reference/loglin-utilities.md),
[`mutual`](https://friendly.github.io/vcdExtra/reference/loglin-utilities.md),
...

[`loglmlist`](https://friendly.github.io/vcdExtra/reference/glmlist.md)

Other loglinear models:
[`get_model()`](https://friendly.github.io/vcdExtra/reference/get_model.md),
[`glmlist()`](https://friendly.github.io/vcdExtra/reference/glmlist.md),
[`joint()`](https://friendly.github.io/vcdExtra/reference/loglin-utilities.md)

## Author

Michael Friendly

## Examples

``` r
data(Titanic, package="datasets")
# variables are in the order Class, Sex, Age, Survived

# Models of joint independence
tit.joint <- seq_loglm(Titanic, type = "joint")

# compare the models
LRstats(tit.joint)
#> Likelihood summary table:
#>            AIC    BIC LR Chisq Df Pr(>Chisq)    
#> joint.1 509.95 509.33   475.81  3  < 2.2e-16 ***
#> joint.2 478.75 479.14   412.60  3  < 2.2e-16 ***
#> joint.3 257.88 264.83   159.10  7  < 2.2e-16 ***
#> joint.4 833.36 858.28   671.96 15  < 2.2e-16 ***
#> ---
#> Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

#' # Models of conditional independence
tit.cond <- seq_loglm(Titanic, type = "conditional")

# compare the models
LRstats(tit.cond)
#> Likelihood summary table:
#>           AIC    BIC LR Chisq Df Pr(>Chisq)    
#> cond.1 509.95 509.33   475.81  3  < 2.2e-16 ***
#> cond.2 478.75 479.14   412.60  3  < 2.2e-16 ***
#> cond.3 500.87 508.60   400.09  6  < 2.2e-16 ***
#> cond.4 760.13 777.72   608.73 20  < 2.2e-16 ***
#> ---
#> Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1


```
