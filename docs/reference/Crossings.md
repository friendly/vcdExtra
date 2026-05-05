# Crossings Interaction of Factors

Given two ordered factors in a square, n x n frequency table,
`Crossings` creates an n-1 column matrix corresponding to different
degrees of difficulty in crossing from one level to the next, as
described by Goodman (1972).

## Usage

``` r
Crossings(...)
```

## Arguments

- ...:

  Two factors

## Value

For two factors of `n` levels, returns a binary indicator matrix of
`n*n` rows and `n-1` columns.

## Details

Instead of treating all mobility as equal, this model posits that the
difficulty of moving between categories increases with the number of
boundaries (or "crossings") that must be crossed, and that associations
between categories decrease with their separation.

## References

Goodman, L. (1972). Some multiplicative models for the analysis of
cross-classified data. In: *Proceedings of the Sixth Berkeley Symposium
on Mathematical Statistics and Probability*, Berkeley, CA: University of
California Press, pp. 649-696.

## See also

[`glm`](https://rdrr.io/r/stats/glm.html),
[`gnm`](https://rdrr.io/pkg/gnm/man/gnm.html) for model fitting
functions for frequency tables;
[`Diag`](https://rdrr.io/pkg/gnm/man/Diag.html),
[`Mult`](https://rdrr.io/pkg/gnm/man/Mult.html),
[`Symm`](https://rdrr.io/pkg/gnm/man/Symm.html),
[`Topo`](https://rdrr.io/pkg/gnm/man/Topo.html) for similar extensions
to terms in model formulas.

## Author

Michael Friendly and Heather Turner

## Examples

``` r

data(Hauser79)
# display table
structable(~Father + Son, data=Hauser79)
#>        Son UpNM LoNM  UpM  LoM Farm
#> Father                             
#> UpNM       1414  521  302  643   40
#> LoNM        724  524  254  703   48
#> UpM         798  648  856 1676  108
#> LoM         756  914  771 3325  237
#> Farm        409  357  441 1611 1832

hauser.indep <- gnm(Freq ~ Father + Son,
                    data=Hauser79,
                    family=poisson)

hauser.CR <- update(hauser.indep,
                    ~ . + Crossings(Father,Son))
LRstats(hauser.CR)
#> Likelihood summary table:
#>              AIC    BIC LR Chisq Df Pr(>Chisq)    
#> hauser.CR 318.63 334.47   89.914 12  5.131e-14 ***
#> ---
#> Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

hauser.CRdiag <- update(hauser.indep,
                        ~ . + Crossings(Father,Son) + Diag(Father,Son))
LRstats(hauser.CRdiag)
#> Likelihood summary table:
#>                  AIC    BIC LR Chisq Df Pr(>Chisq)    
#> hauser.CRdiag 298.95 318.45   64.237  9   2.03e-10 ***
#> ---
#> Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

# what does Crossings do?
cr <- with(Hauser79, Crossings(Father, Son))
head(cr)
#>          C1 C2 C3 C4
#> UpNMUpNM  0  0  0  0
#> UpNMLoNM  1  0  0  0
#> UpNMUpM   1  1  0  0
#> UpNMLoM   1  1  1  0
#> UpNMFarm  1  1  1  1
#> LoNMUpNM  1  0  0  0
# Show the codings for varying Crossings levels
matrix(cr[,1], nrow=5)
#>      [,1] [,2] [,3] [,4] [,5]
#> [1,]    0    1    1    1    1
#> [2,]    1    0    0    0    0
#> [3,]    1    0    0    0    0
#> [4,]    1    0    0    0    0
#> [5,]    1    0    0    0    0
matrix(cr[,2], nrow=5)
#>      [,1] [,2] [,3] [,4] [,5]
#> [1,]    0    0    1    1    1
#> [2,]    0    0    1    1    1
#> [3,]    1    1    0    0    0
#> [4,]    1    1    0    0    0
#> [5,]    1    1    0    0    0
matrix(cr[,3], nrow=5)
#>      [,1] [,2] [,3] [,4] [,5]
#> [1,]    0    0    0    1    1
#> [2,]    0    0    0    1    1
#> [3,]    0    0    0    1    1
#> [4,]    1    1    1    0    0
#> [5,]    1    1    1    0    0
matrix(cr[,4], nrow=5)
#>      [,1] [,2] [,3] [,4] [,5]
#> [1,]    0    0    0    0    1
#> [2,]    0    0    0    0    1
#> [3,]    0    0    0    0    1
#> [4,]    0    0    0    0    1
#> [5,]    1    1    1    1    0
```
