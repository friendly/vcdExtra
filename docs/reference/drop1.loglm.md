# Term-level tests and effect sizes for loglm models

`drop1.loglm()` performs single-term deletion tests for the generating
class of a fitted
[`MASS::loglm()`](https://rdrr.io/pkg/MASS/man/loglm.html) model: for
each term in that class, it drops that term, refits via
[`update()`](https://rdrr.io/r/stats/update.html), and reports the
change in both the LR statistic (G^2) and the Pearson statistic (X^2).

## Usage

``` r
# S3 method for class 'loglm'
drop1(object, scope, test = c("Chisq", "none"), abbrev = FALSE, ...)

LRanova(
  object,
  baseline = NULL,
  test = c("Chisq", "none"),
  abbrev = FALSE,
  ...
)

assoc_strength(
  object,
  scope,
  method = c("Cramer", "Cohen"),
  abbrev = FALSE,
  ...
)
```

## Arguments

- object:

  a fitted [`MASS::loglm`](https://rdrr.io/pkg/MASS/man/loglm.html)
  model

- scope:

  character vector of term labels (colon-separated, as in `"A:B"`) to
  test; must be a subset of the model's generating class. Defaults to
  the full generating class. Used by `drop1.loglm()` and
  `assoc_strength()`.

- test:

  `"Chisq"` (default) to include an LR p-value column, or `"none"` to
  omit it.

- abbrev:

  passed to
  [`get_model()`](https://friendly.github.io/vcdExtra/reference/get_model.md)
  for the `Model:`/`Baseline:` heading line(s); `FALSE` (default) for
  full factor names, or an integer (e.g. `4`) to abbreviate each factor
  name to that many characters – useful when the generating class has
  many terms and the heading line gets long (see the DaytonSurvey
  example below).

- ...:

  currently unused in `drop1.loglm()`; passed on to it from
  `LRanova()`/`assoc_strength()`.

- baseline:

  `NULL` (default) to use the model of mutual independence over the
  variables in `object`'s generating class, or a fitted `loglm` model
  nested within `object` to use as the reference for partial R^2. Used
  by `LRanova()` only.

- method:

  `"Cramer"` (default) for partial Cramer's V, generalized to terms of
  any order by using the *smallest* factor level count among the term's
  variables in place of the usual two-way `min(r,c)`; this reduces
  exactly to the ordinary two-way Cramer's V (as used by
  [`vcd::assocstats()`](https://rdrr.io/pkg/vcd/man/assocstats.html))
  when the term has two factors. Bounded to `[0,1]`. `"Cohen"` for
  Cohen's w = `sqrt(Delta-X^2 / N)`: unbounded, but requires no
  reference to table shape, so it stays well-defined uniformly for terms
  of any order. Used by `assoc_strength()` only.

## Value

An object of class `c("anova", "data.frame")` with columns `Df`,
`LR Chisq`, `Pearson Chisq`, and (if `test = "Chisq"`) `Pr(>Chi)`, one
row per tested term plus a `<none>` reference row. The fitted models
underlying each row – `object` itself as `"<none>"`, plus the refit
dropping each term in `scope` – are attached as `attr(., "models")`, a
`loglmlist` (see
[`loglmlist()`](https://friendly.github.io/vcdExtra/reference/glmlist.md))
with names matching the table's row names. This is free (the models are
already fit to compute the Delta-statistics) and makes the whole
`*.loglmlist` toolchain available without refitting, e.g.
`mosaic(attr(result, "models"), ask = FALSE)` for a grid of the full
model and every drop-one model, or `LRstats(attr(result, "models"))` for
an AIC/BIC comparison.

`LRanova()` adds a `Partial R2` column. The baseline model is attached
separately as `attr(., "baseline")`, since it's a single reference model
rather than one of the `<none>`/drop-one rows in `attr(., "models")`.

`assoc_strength()` adds a `"Cramer's V"` or `"Cohen's w"` column. Note
this is a *partial* association – conditional on the other terms in
`object` – not the marginal association from
[`vcd::assocstats()`](https://rdrr.io/pkg/vcd/man/assocstats.html) on
the term's own two-way margin; the two can differ sharply (see the
UCBAdmissions `Admit:Gender` example below, a classic Simpson's-paradox
case).

## Details

`LRanova()` wraps `drop1.loglm()` and adds a partial R^2 effect-size
column: the fraction of a baseline model's G^2 attributable to each
dropped term.

`assoc_strength()` wraps `drop1.loglm()` and adds a partial Cramer's V
or Cohen's w effect-size column, converting each term's partial Pearson
X^2 into a bounded or semi-bounded measure of association strength.

All three functions test terms in the model's *generating class* – the
set of highest-order terms in a hierarchical loglinear model, from which
every lower-order relative (main effects, lower interactions) is implied
and so doesn't need to be listed or tested separately.

For example, with four factors `A`, `B`, `C`, `D` and the model
`~ (A + B + C + D)^2` (all terms up to two-way), the fitted model
contains ten terms in all – four main effects and six two-way
interactions – but its generating class is just the six two-way terms:
`A:B`, `A:C`, `A:D`, `B:C`, `B:D`, `C:D` (bracket notation
`[A,B] [A,C] [A,D] [B,C] [B,D] [C,D]`).

None of the four main effects is maximal on its own – `A`, say, is
already implied by `A:B` (or any of `A:C`, `A:D`) – so they're excluded
from the generating class even though they're very much still part of
the fitted model. This is exactly what
[`MASS::loglm()`](https://rdrr.io/pkg/MASS/man/loglm.html) stores in
`object$margin`, and exactly the set of terms `scope` defaults to and
`drop1.loglm()` tests: dropping `A:B` removes only that interaction,
leaving `A` and `B`'s main effects in place (still implied by
`A:C`/`A:D` and `B:C`/`B:D` respectively), so the reduced model stays
hierarchical.

## See also

[`mosaic.loglmlist()`](https://friendly.github.io/vcdExtra/reference/mosaic.glmlist.md),
[`LRstats()`](https://friendly.github.io/vcdExtra/reference/LRstats.md),
[`get_models()`](https://friendly.github.io/vcdExtra/reference/get_model.md)

## Author

Michael Friendly

## Examples

``` r
library(MASS)
ucb <- loglm(~ (Admit + Gender + Dept)^2, data = UCBAdmissions)

drop1.loglm(ucb)
#> Single term deletions
#> Model: [Admit,Gender] [Admit,Dept] [Gender,Dept]
#>              Df LR Chisq Pearson Chisq Pr(>Chi)    
#> <none>                                             
#> Admit:Gender  1     1.53          1.11   0.2159    
#> Admit:Dept    5   763.40        696.47   <2e-16 ***
#> Gender:Dept   5  1128.70        996.88   <2e-16 ***
#> ---
#> Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
drop1.loglm(ucb, scope = "Admit:Gender")   # test only a subset
#> Single term deletions
#> Model: [Admit,Gender] [Admit,Dept] [Gender,Dept]
#>              Df LR Chisq Pearson Chisq Pr(>Chi)
#> <none>                                         
#> Admit:Gender  1   1.5312        1.1147   0.2159
drop1.loglm(ucb, abbrev = 4)               # abbreviate factor names
#> Single term deletions
#> Model: [Admt,Gndr] [Admt,Dept] [Gndr,Dept]
#>              Df LR Chisq Pearson Chisq Pr(>Chi)    
#> <none>                                             
#> Admit:Gender  1     1.53          1.11   0.2159    
#> Admit:Dept    5   763.40        696.47   <2e-16 ***
#> Gender:Dept   5  1128.70        996.88   <2e-16 ***
#> ---
#> Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
names(attr(drop1.loglm(ucb), "models"))    # the fitted models, for free
#> [1] "<none>"       "Admit:Gender" "Admit:Dept"   "Gender:Dept" 

# data.frame + `Freq ~ ...` form works too, not just array/table data
data(DaytonSurvey, package = "vcdExtra")
DS <- loglm(Freq ~ (cigarette + alcohol + marijuana + sex + race)^2,
            data = DaytonSurvey)
drop1.loglm(DS)
#> Single term deletions
#> Model: [cigarette,alcohol] [cigarette,marijuana] [cigarette,sex] [cigarette,race] [alcohol,marijuana] [alcohol,sex] [alcohol,race] [marijuana,sex] [marijuana,race] [sex,race]
#>                     Df LR Chisq Pearson Chisq  Pr(>Chi)    
#> <none>                                                     
#> cigarette:alcohol    1   185.86        171.92 < 2.2e-16 ***
#> cigarette:marijuana  1   498.13        455.59 < 2.2e-16 ***
#> cigarette:sex        1     0.98          0.49  0.322980    
#> cigarette:race       1     0.44          1.45  0.505617    
#> alcohol:marijuana    1    91.62         89.44 < 2.2e-16 ***
#> alcohol:sex          1     3.38          4.47  0.066128 .  
#> alcohol:race         1     4.98         11.65  0.025634 *  
#> marijuana:sex        1     9.82          9.29  0.001726 ** 
#> marijuana:race       1     3.59          4.16  0.058178 .  
#> sex:race             1     0.84          1.30  0.359720    
#> ---
#> Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

LRanova(ucb)  # default baseline: mutual independence over Admit, Gender, Dept
#> Single term deletions, with partial R^2
#> Model:    [Admit,Gender] [Admit,Dept] [Gender,Dept]
#> Baseline: [Admit] [Gender] [Dept]  (G^2 = 2097.671, df = 16)
#>              Df LR Chisq Pearson Chisq Pr(>Chi) Partial R2
#> <none>                                                    
#> Admit:Gender  1     1.53          1.11  0.21593    0.00073
#> Admit:Dept    5   763.40        696.47  0.00000    0.36393
#> Gender:Dept   5  1128.70        996.88  0.00000    0.53807

# supply a specific (nested) baseline other than the default -- here, one
# that already includes Admit:Gender, so partial R^2 is now relative to a
# smaller remaining G^2 than the mutual-independence baseline above
partial_baseline <- loglm(~ Admit + Gender + Dept + Admit:Gender, data = UCBAdmissions)
LRanova(ucb, baseline = partial_baseline)
#> Single term deletions, with partial R^2
#> Model:    [Admit,Gender] [Admit,Dept] [Gender,Dept]
#> Baseline: [Dept] [Admit,Gender]  (G^2 = 2004.222, df = 15)
#>              Df LR Chisq Pearson Chisq Pr(>Chi) Partial R2
#> <none>                                                    
#> Admit:Gender  1     1.53          1.11  0.21593    0.00076
#> Admit:Dept    5   763.40        696.47  0.00000    0.38090
#> Gender:Dept   5  1128.70        996.88  0.00000    0.56316

assoc_strength(ucb)                     # Cramer's V (default)
#> Single term deletions, with Cramer's V
#> Model: [Admit,Gender] [Admit,Dept] [Gender,Dept]
#>              Df LR Chisq Pearson Chisq Pr(>Chi) Cramer's V
#> <none>                                                    
#> Admit:Gender  1     1.53          1.11  0.21593    0.01569
#> Admit:Dept    5   763.40        696.47  0.00000    0.39228
#> Gender:Dept   5  1128.70        996.88  0.00000    0.46932
assoc_strength(ucb, method = "Cohen")   # Cohen's w
#> Single term deletions, with Cohen's w
#> Model: [Admit,Gender] [Admit,Dept] [Gender,Dept]
#>              Df LR Chisq Pearson Chisq Pr(>Chi) Cohen's w
#> <none>                                                   
#> Admit:Gender  1     1.53          1.11  0.21593   0.01569
#> Admit:Dept    5   763.40        696.47  0.00000   0.39228
#> Gender:Dept   5  1128.70        996.88  0.00000   0.46932

# partial (conditional on Dept) vs. marginal association -- the small
# partial Admit:Gender V here versus the much larger marginal V below is
# the classic Simpson's-paradox story for this dataset
vcd::assocstats(margin.table(UCBAdmissions, c(1, 2)))
#>                     X^2 df P(> X^2)
#> Likelihood Ratio 93.449  1        0
#> Pearson          92.205  1        0
#> 
#> Phi-Coefficient   : 0.143 
#> Contingency Coeff.: 0.141 
#> Cramer's V        : 0.143 

# Cramer's V and Cohen's w coincide when every factor in a term is binary,
# and diverge once a term involves a factor with more levels
hec <- loglm(~ (Hair + Eye + Sex)^2, data = HairEyeColor)
assoc_strength(hec)  # Hair, Eye each have 4 levels
#> Single term deletions, with Cramer's V
#> Model: [Hair,Eye] [Hair,Sex] [Eye,Sex]
#>          Df LR Chisq Pearson Chisq Pr(>Chi) Cramer's V
#> <none>                                                
#> Hair:Eye  9  149.917       141.076 0.000000    0.28184
#> Hair:Sex  3   11.566        11.173 0.009028    0.13738
#> Eye:Sex   3    5.002         4.902 0.171617    0.09100
```
