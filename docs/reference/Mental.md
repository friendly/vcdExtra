# Mental Impairment and Parents SES

A 6 x 4 contingency table representing the cross-classification of
mental health status (`mental`) of 1660 young New York residents by
their parents' socioeconomic status (`ses`).

## Format

A data frame frequency table with 24 observations on the following 3
variables.

- `ses`:

  an ordered factor with levels `1` \< `2` \< `3` \< `4` \< `5` \< `6`

- `mental`:

  an ordered factor with levels `Well` \< `Mild` \< `Moderate` \<
  `Impaired`

- `Freq`:

  cell frequency: a numeric vector

## Source

Haberman, S. J. *The Analysis of Qualitative Data: New Developments*,
Academic Press, 1979, Vol. II, p. 375.

Srole, L.; Langner, T. S.; Michael, S. T.; Kirkpatrick, P.; Opler, M. K.
& Rennie, T. A. C. *Mental Health in the Metropolis: The Midtown
Manhattan Study*, NYU Press, 1978, p. 289

## Details

Both `ses` and `mental` can be treated as ordered factors or integer
scores. For `ses`, 1="High" and 6="Low".

## References

Friendly, M. *Visualizing Categorical Data*, Cary, NC: SAS Institute,
2000, Appendix B.7.

## Examples

``` r
data(Mental)
str(Mental)
#> 'data.frame':    24 obs. of  3 variables:
#>  $ ses   : Ord.factor w/ 6 levels "1"<"2"<"3"<"4"<..: 1 1 1 1 2 2 2 2 3 3 ...
#>  $ mental: Ord.factor w/ 4 levels "Well"<"Mild"<..: 1 2 3 4 1 2 3 4 1 2 ...
#>  $ Freq  : int  64 94 58 46 57 94 54 40 57 105 ...
(Mental.tab <- xtabs(Freq ~ ses + mental, data=Mental))
#>    mental
#> ses Well Mild Moderate Impaired
#>   1   64   94       58       46
#>   2   57   94       54       40
#>   3   57  105       65       60
#>   4   72  141       77       94
#>   5   36   97       54       78
#>   6   21   71       54       71

# mosaic and sieve plots
mosaic(Mental.tab, gp=shading_Friendly)

sieve(Mental.tab, gp=shading_Friendly)


if(require(ca)){
  plot(ca(Mental.tab), main="Mental impairment & SES", lines=TRUE)
}


# fit linear x linear (uniform) association model, using integer scores
# for rows/cols
indep <- glm(Freq ~ mental + ses, family = poisson, data = Mental)
Cscore <- as.numeric(Mental$ses)
Rscore <- as.numeric(Mental$mental)

linlin <- glm(Freq ~ mental + ses + Rscore:Cscore,
              family = poisson, data = Mental)
anova(linlin, test = "Chisq")
#> Analysis of Deviance Table
#> 
#> Model: poisson, link: log
#> 
#> Response: Freq
#> 
#> Terms added sequentially (first to last)
#> 
#> 
#>               Df Deviance Resid. Df Resid. Dev  Pr(>Chi)    
#> NULL                             23    217.400              
#> mental         3  113.525        20    103.875 < 2.2e-16 ***
#> ses            5   56.457        15     47.418 6.543e-11 ***
#> Rscore:Cscore  1   37.523        14      9.895 9.035e-10 ***
#> ---
#> Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

# use update.glm method to fit other models
linlin <- update(indep, . ~ . + Rscore:Cscore)
roweff <- update(indep, . ~ . + mental:Cscore)
coleff <- update(indep, . ~ . + Rscore:ses)
rowcol <- update(indep, . ~ . + Rscore:ses + mental:Cscore)

# compare models
LRstats(indep, linlin, roweff, coleff, rowcol)
#> Likelihood summary table:
#>           AIC    BIC LR Chisq Df Pr(>Chisq)    
#> indep  209.59 220.19   47.418 15  3.155e-05 ***
#> linlin 174.07 185.85    9.895 14     0.7698    
#> roweff 174.45 188.59    6.281 12     0.9013    
#> coleff 179.00 195.50    6.829 10     0.7415    
#> rowcol 179.22 198.07    3.045  8     0.9315    
#> ---
#> Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

# tests of nested models
anova(indep, linlin, roweff, test = "Chisq")
#> Analysis of Deviance Table
#> 
#> Model 1: Freq ~ mental + ses
#> Model 2: Freq ~ mental + ses + Rscore:Cscore
#> Model 3: Freq ~ mental + ses + mental:Cscore
#>   Resid. Df Resid. Dev Df Deviance  Pr(>Chi)    
#> 1        15     47.418                          
#> 2        14      9.895  1   37.523 9.035e-10 ***
#> 3        12      6.281  2    3.614    0.1641    
#> ---
#> Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
```
