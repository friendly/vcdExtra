# Bivariate Logits and Log Odds Ratio

This function calculates the log odds and log odds ratio for two binary
responses classified by one or more stratifying variables.

## Usage

``` r
blogits(Y, add, colnames, row.vars, rev = FALSE)
```

## Arguments

- Y:

  A four-column matrix or data frame whose columns correspond to the 2 x
  2 combinations of two binary responses.

- add:

  Constant added to all cells to allow for zero frequencies. The default
  is 0.5 if `any(Y)==0` and 0 otherwise.

- colnames:

  Names for the columns of the results. The default is
  `c("logit1", "logit2", "logOR")`. If less than three names are
  supplied, the remaining ones are filled in from the default.

- row.vars:

  A data frame or matrix giving the factor levels of one or more factors
  corresponding to the rows of `Y`

- rev:

  A logical, indicating whether the order of the columns in `Y` should
  be reversed.

## Value

A data frame with `nrow(Y)` rows and `3 + ncol(row.vars)` columns

## Details

It is useful for plotting the results of bivariate logistic regression
models, such as those fit using
[`vglm`](https://rdrr.io/pkg/VGAM/man/vglm.html) in the VGAM.

For two binary variables with levels 0,1 the logits are calculated
assuming the columns in `Y` are given in the order 11, 10, 01, 00, so
the logits give the log odds of the 1 response compared to 0. If this is
not the case, either use `rev=TRUE` or supply `Y[,4:1]` as the first
argument.

## References

Friendly, M. and Meyer, D. (2016). *Discrete Data Analysis with R:
Visualization and Modeling Techniques for Categorical and Count Data*.
Boca Raton, FL: Chapman & Hall/CRC. <http://ddar.datavis.ca>.

## See also

[`vglm`](https://rdrr.io/pkg/VGAM/man/vglm.html)

## Author

Michael Friendly

## Examples

``` r
data(Toxaemia)
tox.tab <- xtabs(Freq~class + smoke + hyper + urea, Toxaemia)

# reshape to 4-column matrix
toxaemia <- t(matrix(aperm(tox.tab), 4, 15))
colnames(toxaemia) <- c("hu", "hU", "Hu", "HU")
rowlabs <- expand.grid(smoke=c("0", "1-19", "20+"), class=factor(1:5))
toxaemia <- cbind(toxaemia, rowlabs)

# logits for H and U
logitsTox <- blogits(toxaemia[,4:1], 
                     add=0.5, 
                     colnames=c("logitH", "logitW"), 
                     row.vars=rowlabs)
logitsTox
#>        logitH    logitW      logOR smoke class
#> 1  -1.0205696 -1.998774  1.5267914     0     1
#> 2  -0.9426080 -2.166453  1.0710243  1-19     1
#> 3  -1.0296194 -2.140066  2.4485390   20+     1
#> 4  -0.9504026 -2.515797  1.4619602     0     2
#> 5  -1.0469879 -2.498258  0.8640134  1-19     2
#> 6  -0.8649974 -2.525729 -1.1457908   20+     2
#> 7  -0.8793862 -2.263924  1.5805648     0     3
#> 8  -1.3826171 -2.362718  1.3736951  1-19     3
#> 9  -1.3393374 -2.273598  0.7442513   20+     3
#> 10 -0.9398380 -2.014903  1.3135126     0     4
#> 11 -1.4394042 -2.251292  1.3426023  1-19     4
#> 12 -1.2992830 -2.322388  0.8846854   20+     4
#> 13 -0.9995915 -1.996554  1.0036228     0     5
#> 14 -1.3002198 -1.938363  1.0347544  1-19     5
#> 15 -1.1574528 -1.897120  2.0187271   20+     5

```
