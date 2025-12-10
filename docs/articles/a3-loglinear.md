# 3. Loglinear Models

You can use the [`loglm()`](https://rdrr.io/pkg/MASS/man/loglm.html)
function in the `MASS` package to fit log-linear models. Equivalent
models can also be fit (from a different perspective) as generalized
linear models with the [`glm()`](https://rdrr.io/r/stats/glm.html)
function using the `family='poisson'` argument, and the `gnm` package
provides a wider range of generalized *nonlinear* models, particularly
for testing structured associations.

The visualization methods for these models were originally developed for
models fit using [`loglm()`](https://rdrr.io/pkg/MASS/man/loglm.html),
so this approach is emphasized here. Some extensions of these methods
for models fit using [`glm()`](https://rdrr.io/r/stats/glm.html) and
[`gnm()`](https://rdrr.io/pkg/gnm/man/gnm.html) are contained in the
`vcdExtra` package and illustrated in (**ref?**)(sec:glm).

Assume we have a 3-way contingency table based on variables A, B, and C.
The possible different forms of loglinear models for a 3-way table are
shown in the table below. @(tab:loglin-3way) The **Model formula**
column shows how to express each model for
[`loglm()`](https://rdrr.io/pkg/MASS/man/loglm.html) in R. [^1] In the
**Interpretation** column, the symbol “$`\perp`$” is to be read as “is
independent of,” and “$`\;|\;`$” means “conditional on,” or “adjusting
for,” or just “given”.

| **Model** | **Model formula** | **Symbol** | **Interpretation** |
|:---|:---|:---|:---|
| Mutual independence | `~A + B + C` | $`[A][B][C]`$ | $`A \perp B \perp C`$ |
| Joint independence | `~A*B + C` | $`[AB][C]`$ | $`(A \: B) \perp C`$ |
| Conditional independence | `~(A+B)*C` | $`[AC][BC]`$ | $`(A \perp B) \;|\; C`$ |
| All two-way associations | `~A*B + A*C + B*C` | $`[AB][AC][BC]`$ | homogeneous association |
| Saturated model | `~A*B*C` | $`[ABC]`$ | 3-way association |

For example, the formula `~A + B + C` specifies the model of *mutual
independence* with no associations among the three factors. In standard
notation for the expected frequencies $`m_{ijk}`$, this corresponds to

``` math
  \log ( m_{ijk} ) = \mu + \lambda_i^A + \lambda_j^B + \lambda_k^C \equiv  A + B + C 
```

The parameters $`\lambda_i^A , \lambda_j^B`$ and $`\lambda_k^C`$ pertain
to the differences among the one-way marginal frequencies for the
factors A, B and C.

Similarly, the model of *joint independence*, $`(A \: B) \perp C`$,
allows an association between A and B, but specifies that C is
independent of both of these and their combinations,

``` math
  \log ( m_{ijk} ) = \mu + \lambda_i^A + \lambda_j^B + \lambda_k^C + \lambda_{ij}^{AB} \equiv A * B + C 
```

where the parameters $`\lambda_{ij}^{AB}`$ pertain to the overall
association between A and B (collapsing over C).

In the literature or text books, you will often find these models
expressed in shorthand symbolic notation, using brackets, `[ ]` to
enclose the *high-order terms* in the model. Thus, the joint
independence model can be denoted `[AB][C]`, as shown in the **Symbol**
column in the table. @(tab:loglin-3way).

Models of *conditional independence* allow (and fit) two of the three
possible two-way associations. There are three such models, depending on
which variable is conditioned upon. For a given conditional independence
model, e.g., `[AB][AC]`, the given variable is the one common to all
terms, so this example has the interpretation $`(B \perp C) \;|\; A`$.

## Fitting with `loglm()`

For example, we can fit the model of mutual independence among hair
color, eye color and sex in `HairEyeColor` as

``` r
library(MASS)
## Independence model of hair and eye color and sex.  
hec.1 <- loglm(~Hair+Eye+Sex, data=HairEyeColor)
hec.1
## Call:
## loglm(formula = ~Hair + Eye + Sex, data = HairEyeColor)
## 
## Statistics:
##                       X^2 df P(> X^2)
## Likelihood Ratio 166.3001 24        0
## Pearson          164.9247 24        0
```

Similarly, the models of conditional independence and joint independence
are specified as

``` r
## Conditional independence
hec.2 <- loglm(~(Hair + Eye) * Sex, data=HairEyeColor)
hec.2
## Call:
## loglm(formula = ~(Hair + Eye) * Sex, data = HairEyeColor)
## 
## Statistics:
##                       X^2 df P(> X^2)
## Likelihood Ratio 156.6779 18        0
## Pearson          147.9440 18        0
```

``` r
## Joint independence model.  
hec.3 <- loglm(~Hair*Eye + Sex, data=HairEyeColor)
hec.3
## Call:
## loglm(formula = ~Hair * Eye + Sex, data = HairEyeColor)
## 
## Statistics:
##                       X^2 df  P(> X^2)
## Likelihood Ratio 19.85656 15 0.1775045
## Pearson          19.56712 15 0.1891745
```

Note that printing the model gives a brief summary of the goodness of
fit. A set of models can be compared using the
[`anova()`](https://rdrr.io/r/stats/anova.html) function.

``` r
anova(hec.1, hec.2, hec.3)
## LR tests for hierarchical log-linear models
## 
## Model 1:
##  ~Hair + Eye + Sex 
## Model 2:
##  ~(Hair + Eye) * Sex 
## Model 3:
##  ~Hair * Eye + Sex 
## 
##            Deviance df Delta(Dev) Delta(df) P(> Delta(Dev)
## Model 1   166.30014 24                                    
## Model 2   156.67789 18    9.62225         6        0.14149
## Model 3    19.85656 15  136.82133         3        0.00000
## Saturated   0.00000  0   19.85656        15        0.17750
```

## Fitting with `glm()` and `gnm()`

The [`glm()`](https://rdrr.io/r/stats/glm.html) approach, and extensions
of this in the `gnm` package allows a much wider class of models for
frequency data to be fit than can be handled by
[`loglm()`](https://rdrr.io/pkg/MASS/man/loglm.html). Of particular
importance are models for ordinal factors and for square tables, where
we can test more structured hypotheses about the patterns of association
than are provided in the tests of general association under
[`loglm()`](https://rdrr.io/pkg/MASS/man/loglm.html). These are similar
in spirit to the non-parametric CMH tests described in @ref(sec:CMH).

***Example***: The data `Mental` in the `vcdExtra` package gives a
two-way table in frequency form classifying young people by their mental
health status and parents’ socioeconomic status (SES), where both of
these variables are ordered factors.

``` r
data(Mental, package = "vcdExtra")
str(Mental)
## 'data.frame':    24 obs. of  3 variables:
##  $ ses   : Ord.factor w/ 6 levels "1"<"2"<"3"<"4"<..: 1 1 1 1 2 2 2 2 3 3 ...
##  $ mental: Ord.factor w/ 4 levels "Well"<"Mild"<..: 1 2 3 4 1 2 3 4 1 2 ...
##  $ Freq  : int  64 94 58 46 57 94 54 40 57 105 ...
xtabs(Freq ~ mental + ses, data=Mental)   # display the frequency table
##           ses
## mental       1   2   3   4   5   6
##   Well      64  57  57  72  36  21
##   Mild      94  94 105 141  97  71
##   Moderate  58  54  65  77  54  54
##   Impaired  46  40  60  94  78  71
```

Simple ways of handling ordinal variables involve assigning scores to
the table categories, and the simplest cases are to use integer scores,
either for the row variable
(`column effects'' model), the column variable (`row effects’’ model),
or both (\`\`uniform association’’ model).

``` r
indep <- glm(Freq ~ mental + ses, family = poisson, data = Mental)  # independence model
```

To fit more parsimonious models than general association, we can define
numeric scores for the row and column categories

``` r
# Use integer scores for rows/cols 
Cscore <- as.numeric(Mental$ses)
Rscore <- as.numeric(Mental$mental) 
```

Then, the row effects model, the column effects model, and the uniform
association model can be fit as follows. The essential idea is to
replace a factor variable with its numeric equivalent in the model
formula for the association term.

``` r
# column effects model (ses)
coleff <- glm(Freq ~ mental + ses + Rscore:ses, family = poisson, data = Mental)

# row effects model (mental)
roweff <- glm(Freq ~ mental + ses + mental:Cscore, family = poisson, data = Mental)

# linear x linear association
linlin <- glm(Freq ~ mental + ses + Rscore:Cscore, family = poisson, data = Mental)
```

The
[`LRstats()`](https://friendly.github.io/vcdExtra/reference/LRstats.md)
function in `vcdExtra` provides a nice, compact summary of the fit
statistics for a set of models, collected into a *glmlist* object.
Smaller is better for AIC and BIC.

``` r
# compare models using AIC, BIC, etc
vcdExtra::LRstats(glmlist(indep, roweff, coleff, linlin))
## Likelihood summary table:
##           AIC    BIC LR Chisq Df Pr(>Chisq)    
## indep  209.59 220.19   47.418 15  3.155e-05 ***
## roweff 174.45 188.59    6.281 12     0.9013    
## coleff 179.00 195.50    6.829 10     0.7415    
## linlin 174.07 185.85    9.895 14     0.7698    
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
```

For specific model comparisons, we can also carry out tests of *nested*
models with [`anova()`](https://rdrr.io/r/stats/anova.html) when those
models are listed from smallest to largest. Here, there are two separate
paths from the most restrictive (independence) model through the model
of uniform association, to those that allow only one of row effects or
column effects.

``` r
anova(indep, linlin, coleff, test="Chisq")  
## Analysis of Deviance Table
## 
## Model 1: Freq ~ mental + ses
## Model 2: Freq ~ mental + ses + Rscore:Cscore
## Model 3: Freq ~ mental + ses + Rscore:ses
##   Resid. Df Resid. Dev Df Deviance  Pr(>Chi)    
## 1        15     47.418                          
## 2        14      9.895  1   37.523 9.035e-10 ***
## 3        10      6.829  4    3.066    0.5469    
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
anova(indep, linlin, roweff, test="Chisq")  
## Analysis of Deviance Table
## 
## Model 1: Freq ~ mental + ses
## Model 2: Freq ~ mental + ses + Rscore:Cscore
## Model 3: Freq ~ mental + ses + mental:Cscore
##   Resid. Df Resid. Dev Df Deviance  Pr(>Chi)    
## 1        15     47.418                          
## 2        14      9.895  1   37.523 9.035e-10 ***
## 3        12      6.281  2    3.614    0.1641    
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
```

The model of linear by linear association seems best on all accounts.
For comparison, one might try the CMH tests on these data:

``` r
CMHtest(xtabs(Freq~ses+mental, data=Mental))
## Cochran-Mantel-Haenszel Statistics for ses by mental 
## 
##                  AltHypothesis  Chisq Df       Prob
## cor        Nonzero correlation 37.156  1 1.0907e-09
## rmeans  Row mean scores differ 40.297  5 1.3012e-07
## cmeans  Col mean scores differ 40.666  3 7.6971e-09
## general    General association 45.958 15 5.4003e-05
```

## Non-linear terms

The strength of the `gnm` package is that it handles a wide variety of
models that handle non-linear terms, where the parameters enter the
model beyond a simple linear function. The simplest example is the
Goodman RC(1) model (Goodman, 1979), which allows a multiplicative term
to account for the association of the table variables. In the notation
of generalized linear models with a log link, this can be expressed as

``` math
 \log \mu_{ij} = \alpha_i + \beta_j + \gamma_{i} \delta_{j}   ,
```

where the row-multiplicative effect parameters $`\gamma_i`$ and
corresponding column parameters $`\delta_j`$ are estimated from the
data.% [^2]

Similarly, the RC(2) model adds two multiplicative terms to the
independence model,

``` math
 \log \mu_{ij} = \alpha_i + \beta_j + \gamma_{i1} \delta_{j1} + \gamma_{i2} \delta_{j2} . 
```

In the `gnm` package, these models may be fit using the
[`Mult()`](https://rdrr.io/pkg/gnm/man/Mult.html) to specify the
multiplicative term, and
[`instances()`](https://rdrr.io/pkg/gnm/man/instances.html) to specify
several such terms.

***Example***: For the `Mental` data, we fit the RC(1) and RC(2) models,
and compare these with the independence model.

``` r
RC1 <- gnm(Freq ~ mental + ses + Mult(mental,ses), data=Mental, 
             family=poisson, verbose=FALSE)
RC2 <- gnm(Freq ~ mental+ses + instances(Mult(mental,ses),2), data=Mental, 
             family=poisson, verbose=FALSE)
anova(indep, RC1, RC2, test="Chisq")
## Analysis of Deviance Table
## 
## Model 1: Freq ~ mental + ses
## Model 2: Freq ~ mental + ses + Mult(mental, ses)
## Model 3: Freq ~ mental + ses + Mult(mental, ses, inst = 1) + Mult(mental, 
##     ses, inst = 2)
##   Resid. Df Resid. Dev Df Deviance  Pr(>Chi)    
## 1        15     47.418                          
## 2         8      3.571  7   43.847 2.288e-07 ***
## 3         3      0.523  5    3.048    0.6926    
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
```

## References

Goodman, L. A. (1979). Simple models for the analysis of association in
cross-classifications having ordered categories. *Journal of the
American Statistical Association*, *74*, 537–552.

[^1]: For [`glm()`](https://rdrr.io/r/stats/glm.html), or
    [`gnm()`](https://rdrr.io/pkg/gnm/man/gnm.html), with the data in
    the form of a frequency data.frame, the same model is specified in
    the form `glm(Freq` $`\sim`$`..., family="poisson")`, where `Freq`
    is the name of the cell frequency variable and `...` specifies the
    *Model formula*.

[^2]: This is similar in spirit to a correspondence analysis with a
    single dimension, but as a statistical model.
