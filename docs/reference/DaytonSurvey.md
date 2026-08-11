# Dayton Student Survey on Substance Use

This data, from Agresti (2002), Table 9.1, gives the result of a 1992
survey in Dayton Ohio of 2276 high school seniors on whether they had
ever used alcohol, cigarettes and marijuana.

## Format

A frequency data frame with 32 observations on the following 6
variables.

- `cigarette`:

  a factor with levels `Yes` `No`

- `alcohol`:

  a factor with levels `Yes` `No`

- `marijuana`:

  a factor with levels `Yes` `No`

- `sex`:

  a factor with levels `female` `male`

- `race`:

  a factor with levels `white` `other`

- `Freq`:

  a numeric vector

## Source

Agresti, A. (2002). *Categorical Data Analysis*, 2nd Ed., New York:
Wiley-Interscience, Table 9.1, p. 362.

## Details

Agresti uses the letters G (`sex`), R (`race`), A (`alcohol`), C
(`cigarette`), M (`marijuana`) to refer to the table variables, and this
usage is followed in the examples below.

Background variables include `sex` and `race` of the respondent (GR),
typically treated as explanatory, so that any model for the full table
should include the term `sex:race`. Models for the reduced table,
collapsed over `sex` and `race` are not entirely unreasonable, but don't
permit the estimation of the effects of these variables on the
responses.

The full 5-way table contains a number of cells with counts of 0 or 1,
as well as many cells with large counts, and even the ACM table
collapsed over GR has some small cell counts. Consequently, residuals
for these models in mosaic displays are best represented as standardized
(adjusted) residuals.

## References

Thompson, L. (2009). *R (and S-PLUS) Manual to Accompany Agresti's
Categorical Data*, http://www.stat.ufl.edu/~aa/cda/Thompson_manual.pdf

## Examples

``` r
data(DaytonSurvey)

# mutual independence
mod.0  <- glm(Freq ~ ., data=DaytonSurvey, family=poisson)

# mutual independence + GR
mod.GR <- glm(Freq ~ . + sex*race, data=DaytonSurvey, family=poisson)
anova(mod.GR, test = "Chisq")
#> Analysis of Deviance Table
#> 
#> Model: poisson, link: log
#> 
#> Response: Freq
#> 
#> Terms added sequentially (first to last)
#> 
#> 
#>           Df Deviance Resid. Df Resid. Dev  Pr(>Chi)    
#> NULL                         31     4818.1              
#> cigarette  1   227.81        30     4590.2 < 2.2e-16 ***
#> alcohol    1  1281.71        29     3308.5 < 2.2e-16 ***
#> marijuana  1    55.91        28     3252.6 7.575e-14 ***
#> sex        1     0.57        27     3252.0    0.4505    
#> race       1  1926.11        26     1325.9 < 2.2e-16 ***
#> sex:race   1     0.79        25     1325.1    0.3746    
#> ---
#> Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

# all two-way terms
mod.all2way <- glm(Freq ~ .^2, data=DaytonSurvey, family=poisson)
anova(mod.all2way, test = "Chisq")
#> Analysis of Deviance Table
#> 
#> Model: poisson, link: log
#> 
#> Response: Freq
#> 
#> Terms added sequentially (first to last)
#> 
#> 
#>                     Df Deviance Resid. Df Resid. Dev  Pr(>Chi)    
#> NULL                                   31     4818.1              
#> cigarette            1   227.81        30     4590.2 < 2.2e-16 ***
#> alcohol              1  1281.71        29     3308.5 < 2.2e-16 ***
#> marijuana            1    55.91        28     3252.6 7.575e-14 ***
#> sex                  1     0.57        27     3252.0  0.450480    
#> race                 1  1926.11        26     1325.9 < 2.2e-16 ***
#> cigarette:alcohol    1   442.19        25      883.7 < 2.2e-16 ***
#> cigarette:marijuana  1   751.81        24      131.9 < 2.2e-16 ***
#> cigarette:sex        1     0.04        23      131.9  0.837426    
#> cigarette:race       1     2.34        22      129.5  0.126405    
#> alcohol:marijuana    1    91.64        21       37.9 < 2.2e-16 ***
#> alcohol:sex          1     2.22        20       35.7  0.136357    
#> alcohol:race         1     6.50        19       29.2  0.010769 *  
#> marijuana:sex        1     9.62        18       19.6  0.001926 ** 
#> marijuana:race       1     3.39        17       16.2  0.065758 .  
#> sex:race             1     0.84        16       15.3  0.359722    
#> ---
#> Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

# compare models
LRstats(mod.0, mod.GR, mod.all2way)
#> Likelihood summary table:
#>                 AIC     BIC LR Chisq Df Pr(>Chisq)    
#> mod.0       1474.17 1482.96  1325.93 26     <2e-16 ***
#> mod.GR      1475.38 1485.64  1325.14 25     <2e-16 ***
#> mod.all2way  183.58  207.03    15.34 16     0.4999    
#> ---
#> Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

# collapse over sex and race
Dayton.ACM <- aggregate(Freq ~ cigarette+alcohol+marijuana,
                        data=DaytonSurvey,
                        FUN=sum)
Dayton.ACM
#>   cigarette alcohol marijuana Freq
#> 1       Yes     Yes       Yes  911
#> 2        No     Yes       Yes   44
#> 3       Yes      No       Yes    3
#> 4        No      No       Yes    2
#> 5       Yes     Yes        No  538
#> 6        No     Yes        No  456
#> 7       Yes      No        No   43
#> 8        No      No        No  279
```
