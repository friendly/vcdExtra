# tidyCat: Tidy Methods For Categorical Data Analysis

![](fig/tidyCat-logo.png)

> Frequency tables need some Tidy Love ❤️

Tidy methods for quantitative data & models have advanced considerably,
but there hasn’t been much development of similar ideas for “categorical
data”, by which I mean data that is often compactly represented as
\\n\\-way frequency tables, cross classified by one or more discrete
factors.

What would it take to implement a tidy framework for such data? These
notes are, in effect, a call for participation in developing a `tidyCat`
package for this purpose. Other possible names for this: `tidyCDA`,
`tidyfreq` but `tidyCat` makes for a nice logo!

``` r
library(MASS)
library(vcdExtra)
```

I see three areas that could be developed here:

## Constructing categorical data sets

Current non-tidy data forms and operations, following (Friendly & Meyer,
2016) are described in the vignette [Creating and manipulating frequency
tables](https://friendly.github.io/vcdExtra/articles/a1-creating.html)

It seems clear that the most flexible and general form, and one that
most closely matches a tidy data frame is **case form**, because this
allows for numeric variables as well. Thus:

> Among these, tidy categorical data should best be represented as a
> `tibble` in **case form**. A tibble is convenient for its’ printing.

### Manipulating categorical data sets

The methods [`xtabs()`](https://rdrr.io/r/stats/xtabs.html),
[`table()`](https://rdrr.io/r/base/table.html) and
[`expand.dft()`](https://friendly.github.io/vcdExtra/reference/expand.dft.md)
described in that vignette allow conversion from one form to another.
Tidy equivalents might be:

- `as_table()`, `as_matrix()`, `as_array()` to convert from any form to
  table/array form

- Similarly, perhaps `as_caseform()`, `as_freqform()` to convert to
  those. There is already `as.data.frame(table)` to convert to frequency
  form, and
  [`expand.dft()`](https://friendly.github.io/vcdExtra/reference/expand.dft.md)
  converts that to case form

``` r
data("HairEyeColor")
hec.df <- as.data.frame(HairEyeColor)
head(hec.df)
##    Hair   Eye  Sex Freq
## 1 Black Brown Male   32
## 2 Brown Brown Male   53
## 3   Red Brown Male   10
## 4 Blond Brown Male    3
## 5 Black  Blue Male   11
## 6 Brown  Blue Male   50
# expand to case form
expand.dft(hec.df) |> head()
##    Hair   Eye  Sex
## 1 Black Brown Male
## 2 Black Brown Male
## 3 Black Brown Male
## 4 Black Brown Male
## 5 Black Brown Male
## 6 Black Brown Male
```

- [`vcd::structable()`](https://rdrr.io/pkg/vcd/man/structable.html)
  (and [`stats::ftable()`](https://rdrr.io/r/stats/ftable.html)) produce
  a ‘flat’ representation of a high-dimensional contingency table
  constructed by recursive splits (similar to the construction of mosaic
  displays). One can be constructed from a table or from a data frame
  with a formula method,

``` r
structable(Titanic)
##             Sex      Male     Female    
##             Survived   No Yes     No Yes
## Class Age                               
## 1st   Child             0   5      0   1
##       Adult           118  57      4 140
## 2nd   Child             0  11      0  13
##       Adult           154  14     13  80
## 3rd   Child            35  13     17  14
##       Adult           387  75     89  76
## Crew  Child             0   0      0   0
##       Adult           670 192      3  20
structable(Sex + Class ~ Survived + Age, data = Titanic)
##                Sex   Male              Female             
##                Class  1st 2nd 3rd Crew    1st 2nd 3rd Crew
## Survived Age                                              
## No       Child          0   0  35    0      0   0  17    0
##          Adult        118 154 387  670      4  13  89    3
## Yes      Child          5  11  13    0      1  13  14    0
##          Adult         57  14  75  192    140  80  76   20
```

and there are a suite of methods for indexing and selecting parts of an
\\n\\-way table.

- The methods in the `plyr` package (now retired) provided a coherent
  set of tools for a split-apply-combine strategy that works nicely with
  multidimensional arrays. Perhaps there are some useful ideas for
  frequency tables that could be resurrected here.

- There is also a role for `purrr` methods and thinking here: \\n\\-way
  tables as nested lists/arrays? The ideas of mapping over these?

## Manipulating factor levels

Also needed:

- methods for **recoding and collapsing** the levels of a factor:
  `forcats::fct_recode()`, `forcats::fct_collapse()`,
  `forcats::fct_lump_min()` are useful here.

- methods for **reordering the levels** of a factor, either manually or
  for some analysis purpose. For example, Data from Glass (1954) gave
  this 5 x 5 table on the occupations of 3500 British fathers and their
  sons, where the occupational categories are listed in alphabetic
  order.

``` r
data(Glass, package="vcdExtra")
str(Glass)
## 'data.frame':    25 obs. of  3 variables:
##  $ father: Factor w/ 5 levels "Managerial","Professional",..: 2 2 2 2 2 1 1 1 1 1 ...
##  $ son   : Factor w/ 5 levels "Managerial","Professional",..: 2 1 4 3 5 2 1 4 3 5 ...
##  $ Freq  : int  50 45 8 18 8 28 174 84 154 55 ...
(glass.tab <- xtabs(Freq ~ father + son, data=Glass))
##               son
## father         Managerial Professional Skilled Supervisory Unskilled
##   Managerial          174           28     154          84        55
##   Professional         45           50      18           8         8
##   Skilled             150           14     714         185       447
##   Supervisory          78           11     223         110        96
##   Unskilled            42            3     320          72       411
```

This can be reordered manually by indexing, to arrange the categories by
**status**, giving an order `Professional` down to `Unskilled`:

``` r
# reorder by status
ord <- c(2, 1, 4, 3, 5) 
glass.tab[ord, ord]
##               son
## father         Professional Managerial Supervisory Skilled Unskilled
##   Professional           50         45           8      18         8
##   Managerial             28        174          84     154        55
##   Supervisory            11         78         110     223        96
##   Skilled                14        150         185     714       447
##   Unskilled               3         42          72     320       411
```

A more general method is to permute the row and column categories in the
order implied by correspondence analysis dimensions. This is implemented
in the [`seriation`
package](https://cran.r-project.org/package=seriation) using the `CA`
method of
[`seriation::seriate()`](https://rdrr.io/pkg/seriation/man/seriate.html)
and applying
[`permute()`](https://rdrr.io/pkg/seriation/man/permute.html) to the
result.

``` r
library(seriation)
order <- seriate(glass.tab, method = "CA")
# the permuted row and column labels
rownames(glass.tab)[order[[1]]]
## [1] "Professional" "Managerial"   "Supervisory"  "Skilled"      "Unskilled"

# reorder rows and columns
permute(glass.tab, order)
##               son
## father         Professional Managerial Supervisory Skilled Unskilled
##   Professional           50         45           8      18         8
##   Managerial             28        174          84     154        55
##   Supervisory            11         78         110     223        96
##   Skilled                14        150         185     714       447
##   Unskilled               3         42          72     320       411
```

What are tidy ways to do these things?

## Models

The standard analysis of frequency data is in the form of loglinear
models fit by [`MASS::loglm()`](https://rdrr.io/pkg/MASS/man/loglm.html)
or with more flexible versions fit with
[`glm()`](https://rdrr.io/r/stats/glm.html) or
[`gnm::gnm()`](https://rdrr.io/pkg/gnm/man/gnm.html). These are
essentially linear models for the log of frequency. In `vcdExtra`, there
are several methods for a list of such models, of class `"glmlist"` and
`"loglmlist"`, and these should be accommodated in tidy methods.

What is needed are `broom` methods for `loglm` models. The information
required is accessible from standard functions, but not in a tidy form.

- `glance.loglm()` – **model level** statistics. These are given in the
  output of the [`print()`](https://rdrr.io/r/base/print.html) method,
  and available from the [`print()`](https://rdrr.io/r/base/print.html)
  method. A complication is both LR and Pearson \\\chi^2\\ are reported,
  so these would need to be made to appear in separate columns. There
  are also related
  [`LRstats()`](https://friendly.github.io/vcdExtra/reference/LRstats.md)
  functions in `vcdExtra`, which report `AIC` and `BIC`.

``` r
hec.indep <- loglm(~Hair+Eye+Sex, data=HairEyeColor)
hec.indep
## Call:
## loglm(formula = ~Hair + Eye + Sex, data = HairEyeColor)
## 
## Statistics:
##                       X^2 df P(> X^2)
## Likelihood Ratio 166.3001 24        0
## Pearson          164.9247 24        0
# extract test statistics
summary(hec.indep)$tests
##                       X^2 df P(> X^2)
## Likelihood Ratio 166.3001 24        0
## Pearson          164.9247 24        0
LRstats(hec.indep)
## Likelihood summary table:
##              AIC   BIC LR Chisq Df Pr(>Chisq)    
## hec.indep 321.18 332.9    166.3 24  < 2.2e-16 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
```

- `tidy.loglm()` — **coefficient level** statistics. These are available
  from `coef.loglm()`. They would need to assembled into a long format.
  Standard errors & p-values might be a problem.

``` r
coef(hec.indep)
## $`(Intercept)`
## [1] 2.646879
## 
## $Hair
##       Black       Brown         Red       Blond 
## -0.17911627  0.79474431 -0.59856762 -0.01706041 
## 
## $Eye
##      Brown       Blue      Hazel      Green 
##  0.5296905  0.5067010 -0.3313375 -0.7050540 
## 
## $Sex
##       Male     Female 
## -0.0574957  0.0574957
```

- `augment.loglm()` — should give **case level** statistics: fitted
  values, residuals, …

``` r
fitted(hec.indep)
## Re-fitting to get fitted values
## , , Sex = Male
## 
##        Eye
## Hair       Brown     Blue     Hazel     Green
##   Black 18.91504 18.48515  7.995903  5.502557
##   Brown 50.08982 48.95142 21.174335 14.571585
##   Red   12.43489 12.15228  5.256566  3.617421
##   Blond 22.24268 21.73717  9.402589  6.470599
## 
## , , Sex = Female
## 
##        Eye
## Hair       Brown     Blue     Hazel     Green
##   Black 21.22010 20.73782  8.970314  6.173119
##   Brown 56.19396 54.91682 23.754719 16.347334
##   Red   13.95025 13.63320  5.897151  4.058254
##   Blond 24.95326 24.38614 10.548424  7.259131
residuals(hec.indep)
## Re-fitting to get frequencies and fitted values
## , , Sex = Male
## 
##        Eye
## Hair         Brown       Blue      Hazel      Green
##   Black  2.7349421 -1.8843321  0.6818522 -1.1685504
##   Brown  0.4073036  0.1493413  0.8080654  0.1116873
##   Red   -0.7150915 -0.6371217  0.7233138  1.5738248
##   Blond -5.1444108  1.6747424 -1.5778804  0.5796238
## 
## , , Sex = Female
## 
##        Eye
## Hair         Brown       Blue      Hazel      Green
##   Black  2.9150045 -2.9069608 -1.4476878 -1.9590840
##   Brown  1.2726041 -3.0381608  1.0398490 -0.5953638
##   Red    0.5361173 -1.9834384  0.4409912  1.3223880
##   Blond -5.2211917  6.6539759 -1.9056398  0.2704898
```

What about `hatvalues`? Not implemented, but shouldn’t be too hard.

``` r
hatvalues(hec.indep)
## Error in `UseMethod()`:
## ! no applicable method for 'hatvalues' applied to an object of class "loglm"
```

## Graphical methods

The most common graphical methods are those implemented in `vcd`:
[`mosaic()`](https://rdrr.io/pkg/vcd/man/mosaic.html) association plots
([`assoc()`](https://rdrr.io/pkg/vcd/man/assoc.html)), …, which rely on
[`vcd::strucplot()`](https://rdrr.io/pkg/vcd/man/strucplot.html)
described in (Meyer, Zeileis, & Hornik, 2006).

Is there a tidy analog that might work with `ggplot2`? The [`ggmosaic`
package](https://github.com/haleyjeppson/ggmosaic) implements basic
marimeko-style mosaic plots. They are not very general, in that they
cannot do residual-based shading to show the patterns of association.

However, they are based on a
[productplots](https://github.com/hadley/productplots) package by
Hadley, which seems to provide some basic structure for constructing
such displays of nested rectangles.

## References

Friendly, M., & Meyer, D. (2016). *Discrete data analysis with R:
Visualization and modeling techniques for categorical and count data*.
Boca Raton, FL: Chapman & Hall/CRC.

Glass, D. V. (1954). *Social mobility in britain*. Glencoe, IL: The Free
Press.

Meyer, D., Zeileis, A., & Hornik, K. (2006). The strucplot framework:
Visualizing multi-way contingency tables with. *Journal of Statistical
Software*, *17*(3), 1–48. Retrieved from
<https://www.jstatsoft.org/v17/i03/>
