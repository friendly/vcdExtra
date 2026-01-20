# Fit All K-way Models in a GLM

Generate and fit all 0-way, 1-way, 2-way, ... k-way terms in a glm.

## Usage

``` r
Kway(formula, family = poisson, data, ..., order = nt, prefix = "kway")
```

## Arguments

- formula:

  a two-sided formula for the 1-way effects in the model. The LHS should
  be the response, and the RHS should be the first-order terms connected
  by `+` signs.

- family:

  a description of the error distribution and link function to be used
  in the model. This can be a character string naming a family function,
  a family function or the result of a call to a family function. (See
  [`family`](https://rdrr.io/r/stats/family.html) for details of family
  functions.)

- data:

  an optional data frame, list or environment (or object coercible by
  [`as.data.frame`](https://rdrr.io/r/base/as.data.frame.html) to a data
  frame) containing the variables in the model. If not found in data,
  the variables are taken from `environment(formula)`, typically the
  environment from which `glm` is called.

- ...:

  Other arguments passed to `glm`

- order:

  Highest order interaction of the models generated. Defaults to the
  number of terms in the model formula.

- prefix:

  Prefix used to label the models fit in the `glmlist` object.

## Value

An object of class `glmlist`, of length `order+1` containing the 0-way,
1-way, ... models up to degree `order`.

## Details

This function is designed mainly for hierarchical loglinear models (or
`glm`s in the poisson family), where it is desired to find the
highest-order terms necessary to achieve a satisfactory fit.

Using [`anova`](https://rdrr.io/r/stats/anova.html) on the resulting
[`glmlist`](https://friendly.github.io/vcdExtra/reference/glmlist.md)
object will then give sequential tests of the pooled contributions of
all terms of degree \\k+1\\ over and above those of degree \\k\\.

This function is also intended as an example of a generating function
for
[`glmlist`](https://friendly.github.io/vcdExtra/reference/glmlist.md)
objects, to facilitate model comparison, extraction, summary and
plotting of model components, etc., perhaps using `lapply` or similar.

With `y` as the response in the `formula`, the 0-way (null) model is
`y ~ 1`. The 1-way ("main effects") model is that specified in the
`formula` argument. The k-way model is generated using the formula
`. ~ .^k`. With the default `order = nt`, the final model is the
saturated model.

As presently written, the function requires a two-sided formula with an
explicit response on the LHS. For frequency data in table form (e.g.,
produced by `xtabs`) you the `data` argument is coerced to a data.frame,
so you should supply the `formula` in the form `Freq ~ ` ....

## See also

[`glmlist`](https://friendly.github.io/vcdExtra/reference/glmlist.md),
[`Summarise`](https://friendly.github.io/vcdExtra/reference/Summarise.md)
(soon to be deprecated),
[`LRstats`](https://friendly.github.io/vcdExtra/reference/LRstats.md)

Other glmlist functions:
[`LRstats()`](https://friendly.github.io/vcdExtra/reference/LRstats.md),
[`glmlist()`](https://friendly.github.io/vcdExtra/reference/glmlist.md),
[`mosaic.glmlist()`](https://friendly.github.io/vcdExtra/reference/mosaic.glmlist.md)

## Author

Michael Friendly and Heather Turner

## Examples

``` r
## artificial data
factors <- expand.grid(A=factor(1:3),
                       B=factor(1:2),
                       C=factor(1:3),
                       D=factor(1:2))
Freq <- rpois(nrow(factors), lambda=40)
df <- cbind(factors, Freq)

mods3 <- Kway(Freq ~ A + B + C, data=df, family=poisson)
LRstats(mods3)
#> Likelihood summary table:
#>           AIC    BIC LR Chisq Df Pr(>Chisq)  
#> kway.0 249.69 251.28   49.216 35    0.05601 .
#> kway.1 256.21 265.71   45.735 30    0.03293 *
#> kway.2 256.64 278.81   30.168 22    0.11444  
#> kway.3 258.52 287.02   24.042 18    0.15366  
#> ---
#> Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
mods4 <- Kway(Freq ~ A + B + C + D, data=df, family=poisson)
LRstats(mods4)
#> Likelihood summary table:
#>           AIC    BIC LR Chisq Df Pr(>Chisq)    
#> kway.0 249.69 251.28   49.216 35    0.05601 .  
#> kway.1 258.17 269.25   45.690 29    0.02518 *  
#> kway.2 255.38 287.05   16.902 16    0.39196    
#> kway.3 265.50 316.18    3.028  4    0.55313    
#> kway.4 270.48 327.48    0.000  0    < 2e-16 ***
#> ---
#> Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

# JobSatisfaction data
data(JobSatisfaction, package="vcd")
modSat <- Kway(Freq ~ management+supervisor+own,
               data=JobSatisfaction,
               family=poisson, prefix="JobSat")
LRstats(modSat)
#> Likelihood summary table:
#>              AIC     BIC LR Chisq Df Pr(>Chisq)    
#> JobSat.0 260.251 260.330  208.775  7     <2e-16 ***
#> JobSat.1 175.472 175.790  117.997  4     <2e-16 ***
#> JobSat.2  63.541  64.097    0.065  1     0.7989    
#> JobSat.3  65.476  66.111    0.000  0     <2e-16 ***
#> ---
#> Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
anova(modSat, test="Chisq")
#> Analysis of Deviance Table
#> 
#> Model 1: Freq ~ 1
#> Model 2: Freq ~ management + supervisor + own
#> Model 3: Freq ~ management + supervisor + own + management:supervisor + 
#>     management:own + supervisor:own
#> Model 4: Freq ~ management + supervisor + own + management:supervisor + 
#>     management:own + supervisor:own + management:supervisor:own
#>   Resid. Df Resid. Dev Df Deviance Pr(>Chi)    
#> 1         7    208.775                         
#> 2         4    117.997  3   90.778   <2e-16 ***
#> 3         1      0.065  3  117.932   <2e-16 ***
#> 4         0      0.000  1    0.065   0.7989    
#> ---
#> Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

# Rochdale data: very sparse, in table form
data(Rochdale, package="vcd")
if (FALSE) { # \dontrun{
modRoch <- Kway(Freq~EconActive + Age + HusbandEmployed + Child +
                     Education + HusbandEducation + Asian + HouseholdWorking,
                data=Rochdale, family=poisson)
LRstats(modRoch)
} # }
```
