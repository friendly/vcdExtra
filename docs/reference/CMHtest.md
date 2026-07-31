# Generalized Cochran-Mantel-Haenszel Tests

Provides generalized Cochran-Mantel-Haenszel tests of association of two
possibly ordered factors, optionally stratified other factor(s). With
strata, `CMHtest` calculates these tests for each level of the
stratifying variables and also provides overall tests controlling for
the strata. See **Details** for descriptions of what these entail and
enhancements to this function.

## Usage

``` r
CMHtest(x, ...)

# S3 method for class 'formula'
CMHtest(formula, data = NULL, subset = NULL, na.action = NULL, ...)

# Default S3 method
CMHtest(
  x,
  strata = NULL,
  rscores = 1:R,
  cscores = 1:C,
  types = c("cor", "rmeans", "cmeans", "general"),
  overall = FALSE,
  details = overall,
  ...
)

# S3 method for class 'CMHtest'
print(
  x,
  digits = max(getOption("digits") - 2, 3),
  layout = c("table", "2x2"),
  stars = FALSE,
  scale = FALSE,
  ...
)
```

## Arguments

- x:

  A 2+ way contingency table in array form, or a class `"table"` object
  with optional category labels specified in the dimnames(x) attribute.

- ...:

  Other arguments passed to default method.

- formula:

  a formula specifying the variables used to create a contingency table
  from `data`. This should be a one-sided formula when `data` is in
  array form, and a two-sided formula with a response `Freq` if `data`
  is a data frame with a cell frequency variable. For convenience,
  conditioning formulas can be specified indicating strata.

- data:

  either a data frame, or an object of class `"table"` or `"ftable"`.

- subset:

  an optional vector specifying a subset of observations to be used.

- na.action:

  a function which indicates what should happen when the data contain
  `NA`s. Ignored if `data` is a contingency table.

- strata:

  For a 3- or higher-way table, the names or numbers of the factors to
  be treated as strata. By default, the first 2 factors are treated as
  the main table variables, and all others considered stratifying
  factors.

- rscores:

  Row scores. Either a set of numbers (typically integers, `1:R`) or the
  string `"midrank"` for standardized midrank scores, or `NULL` to
  exclude tests that depend on row scores.

- cscores:

  Column scores. Same as for row scores.

- types:

  Types of CMH tests to compute: Any one or more of
  `c("cor", "cmeans", "rmeans", "general")`, or `"ALL"` for all of
  these.

- overall:

  logical. Whether to calculate overall tests, controlling for the
  stratifying factors.

- details:

  logical. Whether to include computational details in the result

- digits:

  Digits to print.

- layout:

  For `print.CMHtest()`, one of `"table"` (default) for the traditional
  flat 4-row printout, or `"2x2"` to reorganize the four CMH statistics
  into a 2x2 table crossing how the row and column variables are each
  treated (general/nominal vs. ordered/scored). `layout = "2x2"`
  requires all four of `cor`, `rmeans`, `cmeans`, `general` to be
  present in `x`; if `types` or `rscores`/`cscores = NULL` excluded any
  of them, this falls back to `layout = "table"` with a warning. The
  corner cell of the 2x2 display (`general - rmeans - cmeans + cor`, a
  "diff of diffs") is algebraically consistent but its distribution as
  chi-square(df) is **experimental/unverified** – these are four
  different quadratic-form statistics, not a nested sequence of LR
  tests, so the naive inclusion-exclusion combination is not guaranteed
  to be non-negative in general (see `dev/CMH-2x2.md` for a worked
  counter-example and a loglinear
  [`LRstats()`](https://friendly.github.io/vcdExtra/reference/LRstats.md)-based
  alternative).

- stars:

  For `print.CMHtest()` with `layout = "2x2"`, logical: annotate each
  cell with significance stars (`'***'`/`'**'`/`'*'` for p \<
  .001/.01/.05) based on its own (Chisq, Df). Default `FALSE`, matching
  `layout = "table"`'s plain display.

- scale:

  For `print.CMHtest()` with `layout = "2x2"`, logical: show `Chisq/Df`
  instead of `Chisq (Df)` in each cell – normalizes for the very
  different degrees of freedom across cells. Default `FALSE`.

## Value

An object of class `"CMHtest"` , a list with the following 4 components:

- table:

  A matrix containing the test statistics, with columns `Chisq`, `Df`
  and `Prob`

- names:

  The names of the table row and column variables

- rscore:

  Row scores

- cscore:

  Column scores

If `details==TRUE`, additional components are included.

If there are strata, the result is a list of `"CMHtest"` objects. If
`overall=TRUE` another component, labeled `ALL` is appended to the list.

## Details

For ordinal factors, more powerful tests than the test for general
association (independence) are obtained by assigning scores to the row
and column categories.

The standard \\\chi^2\\ tests for association in a two-way table treat
both table factors as nominal (unordered) categories. When one or both
factors of a two-way table are quantitative or ordinal, more powerful
tests of association may be obtained by taking ordinality into account
using row and or column scores to test for linear trends or differences
in row or column means.

The CMH analysis for a two-way table produces **generalized**
Cochran-Mantel-Haenszel statistics (Landis etal., 1978):

- The CMH **correlation** statistic (`"cor"`), treating both factors as
  ordered. For a given statum, with equally spaced row and column
  scores, this CMH statistic reduces to \\(n-1) r^2\\, where \\r\\ is
  the Pearson correlation between X and Y. With `"midrank"` scores, this
  CMH statistic is analogous to \\(n-1) r_S^2\\, using the Spearman rank
  correlation.

- The **ANOVA** (row mean scores and column mean scores) statistics,
  treat the columns and rows respectively as ordinal, and are sensitive
  to mean shifts over columns or rows. These are transforms of the \\F\\
  statistics from one-way ANOVAs with equally spaced scores and to
  Kruskal-Wallis tests with `"midrank"` scores.

- The CMH **general** association statistic treat both factors as
  unordered, and give a test closely related to the Pearson \\\chi^2\\
  test. When there is more than one stratum, the overall general CMH
  statistic gives a stratum-adjusted Pearson \\\chi^2\\, equivalent to
  what is calculated by
  [`mantelhaen.test`](https://rdrr.io/r/stats/mantelhaen.test.html).

### Strata

For a 3+ way table, one table of CMH tests is produced for each
combination of the factors identified as `strata`. If `overall=TRUE`, an
additional table is calculated for the same two primary variables,
controlling for (pooling over) the `strata` variables.

These overall tests implicitly assume no interactions between the
primary variables and the strata and they will have low power in the
presence of interactions.

Note that strata combinations with insufficient data (less than 2
observations) are automatically omitted from the analysis.

## References

Stokes, M. E. & Davis, C. S. & Koch, G., (2000). *Categorical Data
Analysis using the SAS System*, 2nd Ed., Cary, NC: SAS Institute, pp
74-75, 92-101, 124-129. Details of the computation are given at:
<http://support.sas.com/documentation/cdl/en/statug/63033/HTML/default/viewer.htm#statug_freq_a0000000648.htm>

Cochran, W. G. (1954), Some Methods for Strengthening the Common
\\\chi^2\\ Tests, *Biometrics*, 10, 417-451.

Landis, R. J., Heyman, E. R., and Koch, G. G. (1978). Average Partial
Association in Three-way Contingency Tables: A Review and Discussion of
Alternative Tests, *International Statistical Review*, **46**, 237-254.

Mantel, N. (1963), Chi-square Tests with One Degree of Freedom:
Extensions of the Mantel-Haenszel Procedure," *Journal of the American
Statistical Association*, 58, 690-700.

## See also

[`cmh_test`](https://rdrr.io/pkg/coin/man/ContingencyTests.html)
provides the CMH test of general association;
[`lbl_test`](https://rdrr.io/pkg/coin/man/ContingencyTests.html)
provides the CMH correlation test of linear by linear association.

[`mantelhaen.test`](https://rdrr.io/r/stats/mantelhaen.test.html)
provides the overall general Cochran-Mantel-Haenszel chi-squared test of
the null that two nominal variables are conditionally independent in
each stratum, assuming that there is no three-way interaction

Other association tests:
[`GKgamma()`](https://friendly.github.io/vcdExtra/reference/GKgamma.md),
[`HLtest()`](https://friendly.github.io/vcdExtra/reference/HLtest.md),
[`breslow_day_test()`](https://friendly.github.io/vcdExtra/reference/breslow_day_test.md),
[`woolf_test()`](https://friendly.github.io/vcdExtra/reference/woolf_test.md),
[`zero.test()`](https://friendly.github.io/vcdExtra/reference/zero.test.md)

## Author

Michael Friendly

## Examples

``` r

data(JobSat, package="vcdExtra")
CMHtest(JobSat)
#> Cochran-Mantel-Haenszel Statistics for income by satisfaction 
#> 
#>                  AltHypothesis  Chisq Df     Prob
#> cor        Nonzero correlation 2.9830  1 0.084144
#> rmeans  Row mean scores differ 4.4774  3 0.214318
#> cmeans  Col mean scores differ 3.1036  3 0.375931
#> general    General association 5.9034  9 0.749549
#> 
CMHtest(JobSat, rscores="midrank", cscores="midrank")
#> Cochran-Mantel-Haenszel Statistics for income by satisfaction 
#> 
#>                  AltHypothesis  Chisq Df     Prob
#> cor        Nonzero correlation 2.9726  1 0.084685
#> rmeans  Row mean scores differ 4.1200  3 0.248799
#> cmeans  Col mean scores differ 3.3108  3 0.346144
#> general    General association 5.9034  9 0.749549
#> 

# formula interface
CMHtest(~ ., data=JobSat)
#> Cochran-Mantel-Haenszel Statistics for income by satisfaction 
#> 
#>                  AltHypothesis  Chisq Df     Prob
#> cor        Nonzero correlation 2.9830  1 0.084144
#> rmeans  Row mean scores differ 4.4774  3 0.214318
#> cmeans  Col mean scores differ 3.1036  3 0.375931
#> general    General association 5.9034  9 0.749549
#> 

# A 3-way table (both factors ordinal)
data(MSPatients, package="vcd")
CMHtest(MSPatients)
#> $`Patients:Winnipeg`
#> Cochran-Mantel-Haenszel Statistics for New Orleans Neurologist by Winnipeg Neurologist 
#>  in stratum Patients:Winnipeg 
#> 
#>                  AltHypothesis  Chisq Df       Prob
#> cor        Nonzero correlation 51.424  1 7.4426e-13
#> rmeans  Row mean scores differ 55.393  3 5.6601e-12
#> cmeans  Col mean scores differ 53.631  3 1.3450e-11
#> general    General association 64.318  9 1.9580e-10
#> 
#> 
#> $`Patients:New Orleans`
#> Cochran-Mantel-Haenszel Statistics for New Orleans Neurologist by Winnipeg Neurologist 
#>  in stratum Patients:New Orleans 
#> 
#>                  AltHypothesis  Chisq Df       Prob
#> cor        Nonzero correlation 28.863  1 7.7667e-08
#> rmeans  Row mean scores differ 30.594  3 1.0347e-06
#> cmeans  Col mean scores differ 29.054  3 2.1818e-06
#> general    General association 43.428  9 1.7990e-06
#> 
#> 


# also calculate overall tests, controlling for Patient
CMHtest(MSPatients, overall = TRUE)
#> $`Patients:Winnipeg`
#> Cochran-Mantel-Haenszel Statistics for New Orleans Neurologist by Winnipeg Neurologist 
#>  in stratum Patients:Winnipeg 
#> 
#>                  AltHypothesis  Chisq Df       Prob
#> cor        Nonzero correlation 51.424  1 7.4426e-13
#> rmeans  Row mean scores differ 55.393  3 5.6601e-12
#> cmeans  Col mean scores differ 53.631  3 1.3450e-11
#> general    General association 64.318  9 1.9580e-10
#> 
#> 
#> $`Patients:New Orleans`
#> Cochran-Mantel-Haenszel Statistics for New Orleans Neurologist by Winnipeg Neurologist 
#>  in stratum Patients:New Orleans 
#> 
#>                  AltHypothesis  Chisq Df       Prob
#> cor        Nonzero correlation 28.863  1 7.7667e-08
#> rmeans  Row mean scores differ 30.594  3 1.0347e-06
#> cmeans  Col mean scores differ 29.054  3 2.1818e-06
#> general    General association 43.428  9 1.7990e-06
#> 
#> 
#> $ALL
#> Cochran-Mantel-Haenszel Statistics for New Orleans Neurologist by Winnipeg Neurologist 
#>  Overall tests, controlling for all strata 
#> 
#>                  AltHypothesis  Chisq Df       Prob
#> cor        Nonzero correlation 80.086  1 3.5848e-19
#> rmeans  Row mean scores differ 83.923  3 4.4189e-18
#> cmeans  Col mean scores differ 81.116  3 1.7685e-17
#> general    General association 101.21  9 8.9683e-18
#> 
#> 
# compare with mantelhaen.test
mantelhaen.test(MSPatients)
#> 
#>  Cochran-Mantel-Haenszel test
#> 
#> data:  MSPatients
#> Cochran-Mantel-Haenszel M^2 = 101.21, df = 9, p-value < 2.2e-16
#> 

# formula interface
CMHtest(~ ., data = MSPatients, overall = TRUE)
#> $`Patients:Winnipeg`
#> Cochran-Mantel-Haenszel Statistics for New Orleans Neurologist by Winnipeg Neurologist 
#>  in stratum Patients:Winnipeg 
#> 
#>                  AltHypothesis  Chisq Df       Prob
#> cor        Nonzero correlation 51.424  1 7.4426e-13
#> rmeans  Row mean scores differ 55.393  3 5.6601e-12
#> cmeans  Col mean scores differ 53.631  3 1.3450e-11
#> general    General association 64.318  9 1.9580e-10
#> 
#> 
#> $`Patients:New Orleans`
#> Cochran-Mantel-Haenszel Statistics for New Orleans Neurologist by Winnipeg Neurologist 
#>  in stratum Patients:New Orleans 
#> 
#>                  AltHypothesis  Chisq Df       Prob
#> cor        Nonzero correlation 28.863  1 7.7667e-08
#> rmeans  Row mean scores differ 30.594  3 1.0347e-06
#> cmeans  Col mean scores differ 29.054  3 2.1818e-06
#> general    General association 43.428  9 1.7990e-06
#> 
#> 
#> $ALL
#> Cochran-Mantel-Haenszel Statistics for New Orleans Neurologist by Winnipeg Neurologist 
#>  Overall tests, controlling for all strata 
#> 
#>                  AltHypothesis  Chisq Df       Prob
#> cor        Nonzero correlation 80.086  1 3.5848e-19
#> rmeans  Row mean scores differ 83.923  3 4.4189e-18
#> cmeans  Col mean scores differ 81.116  3 1.7685e-17
#> general    General association 101.21  9 8.9683e-18
#> 
#> 

# using a frequency data.frame
CMHtest(xtabs(Freq~ses + mental, data = Mental))
#> Cochran-Mantel-Haenszel Statistics for ses by mental 
#> 
#>                  AltHypothesis  Chisq Df       Prob
#> cor        Nonzero correlation 37.156  1 1.0907e-09
#> rmeans  Row mean scores differ 40.297  5 1.3012e-07
#> cmeans  Col mean scores differ 40.666  3 7.6971e-09
#> general    General association 45.958 15 5.4003e-05
#> 
# or, more simply
CMHtest(Freq~ses + mental, data = Mental)
#> Cochran-Mantel-Haenszel Statistics for ses by mental 
#> 
#>                  AltHypothesis  Chisq Df       Prob
#> cor        Nonzero correlation 37.156  1 1.0907e-09
#> rmeans  Row mean scores differ 40.297  5 1.3012e-07
#> cmeans  Col mean scores differ 40.666  3 7.6971e-09
#> general    General association 45.958 15 5.4003e-05
#> 

# conditioning formulae
CMHtest(Freq~right + left | gender, data = VisualAcuity)
#> $`gender:male`
#> Cochran-Mantel-Haenszel Statistics for right by left 
#>  in stratum gender:male 
#> 
#>                  AltHypothesis  Chisq Df Prob
#> cor        Nonzero correlation 1554.6  1    0
#> rmeans  Row mean scores differ 1556.3  3    0
#> cmeans  Col mean scores differ 1556.6  3    0
#> general    General association 3303.3  9    0
#> 
#> 
#> $`gender:female`
#> Cochran-Mantel-Haenszel Statistics for right by left 
#>  in stratum gender:female 
#> 
#>                  AltHypothesis  Chisq Df Prob
#> cor        Nonzero correlation 3691.3  1    0
#> rmeans  Row mean scores differ 3709.4  3    0
#> cmeans  Col mean scores differ 3724.0  3    0
#> general    General association 8095.8  9    0
#> 
#> 

CMHtest(Freq ~ attitude + memory | education + age, data = Punishment)
#> $`education:elementary|age:15-24`
#> Cochran-Mantel-Haenszel Statistics for attitude by memory 
#>  in stratum education:elementary|age:15-24 
#> 
#>                  AltHypothesis  Chisq Df     Prob
#> cor        Nonzero correlation 3.5652  1 0.059002
#> rmeans  Row mean scores differ 3.5652  1 0.059002
#> cmeans  Col mean scores differ 3.5652  1 0.059002
#> general    General association 3.5652  1 0.059002
#> 
#> 
#> $`education:secondary|age:15-24`
#> Cochran-Mantel-Haenszel Statistics for attitude by memory 
#>  in stratum education:secondary|age:15-24 
#> 
#>                  AltHypothesis    Chisq Df   Prob
#> cor        Nonzero correlation 0.077731  1 0.7804
#> rmeans  Row mean scores differ 0.077731  1 0.7804
#> cmeans  Col mean scores differ 0.077731  1 0.7804
#> general    General association 0.077731  1 0.7804
#> 
#> 
#> $`education:high|age:15-24`
#> Cochran-Mantel-Haenszel Statistics for attitude by memory 
#>  in stratum education:high|age:15-24 
#> 
#>                  AltHypothesis    Chisq Df    Prob
#> cor        Nonzero correlation 0.089524  1 0.76478
#> rmeans  Row mean scores differ 0.089524  1 0.76478
#> cmeans  Col mean scores differ 0.089524  1 0.76478
#> general    General association 0.089524  1 0.76478
#> 
#> 
#> $`education:elementary|age:25-39`
#> Cochran-Mantel-Haenszel Statistics for attitude by memory 
#>  in stratum education:elementary|age:25-39 
#> 
#>                  AltHypothesis  Chisq Df     Prob
#> cor        Nonzero correlation 8.5433  1 0.003468
#> rmeans  Row mean scores differ 8.5433  1 0.003468
#> cmeans  Col mean scores differ 8.5433  1 0.003468
#> general    General association 8.5433  1 0.003468
#> 
#> 
#> $`education:secondary|age:25-39`
#> Cochran-Mantel-Haenszel Statistics for attitude by memory 
#>  in stratum education:secondary|age:25-39 
#> 
#>                  AltHypothesis   Chisq Df    Prob
#> cor        Nonzero correlation 0.92897  1 0.33513
#> rmeans  Row mean scores differ 0.92897  1 0.33513
#> cmeans  Col mean scores differ 0.92897  1 0.33513
#> general    General association 0.92897  1 0.33513
#> 
#> 
#> $`education:high|age:25-39`
#> Cochran-Mantel-Haenszel Statistics for attitude by memory 
#>  in stratum education:high|age:25-39 
#> 
#>                  AltHypothesis Chisq Df    Prob
#> cor        Nonzero correlation 0.472  1 0.49207
#> rmeans  Row mean scores differ 0.472  1 0.49207
#> cmeans  Col mean scores differ 0.472  1 0.49207
#> general    General association 0.472  1 0.49207
#> 
#> 
#> $`education:elementary|age:40-`
#> Cochran-Mantel-Haenszel Statistics for attitude by memory 
#>  in stratum education:elementary|age:40- 
#> 
#>                  AltHypothesis  Chisq Df       Prob
#> cor        Nonzero correlation 11.606  1 0.00065737
#> rmeans  Row mean scores differ 11.606  1 0.00065737
#> cmeans  Col mean scores differ 11.606  1 0.00065737
#> general    General association 11.606  1 0.00065737
#> 
#> 
#> $`education:secondary|age:40-`
#> Cochran-Mantel-Haenszel Statistics for attitude by memory 
#>  in stratum education:secondary|age:40- 
#> 
#>                  AltHypothesis  Chisq Df    Prob
#> cor        Nonzero correlation 6.0457  1 0.01394
#> rmeans  Row mean scores differ 6.0457  1 0.01394
#> cmeans  Col mean scores differ 6.0457  1 0.01394
#> general    General association 6.0457  1 0.01394
#> 
#> 
#> $`education:high|age:40-`
#> Cochran-Mantel-Haenszel Statistics for attitude by memory 
#>  in stratum education:high|age:40- 
#> 
#>                  AltHypothesis  Chisq Df     Prob
#> cor        Nonzero correlation 3.0436  1 0.081055
#> rmeans  Row mean scores differ 3.0436  1 0.081055
#> cmeans  Col mean scores differ 3.0436  1 0.081055
#> general    General association 3.0436  1 0.081055
#> 
#> 

# 2x2 layout, reorganizing the four CMH statistics by how the row/column
# variables are treated (general/nominal vs. ordered/scored)
cmh_mental <- CMHtest(Freq ~ ses + mental, data = Mental)
print(cmh_mental, layout = "2x2")
#> Cochran-Mantel-Haenszel Statistics for ses by mental 
#>  Row/column display: general (nominal) vs. ordered (scored)
#>  Cell values: X^2 (df)
#> 
#>                col: general col: ordered diff (gen-ord)
#> row: general    45.958 (15)   40.297 (5)    5.6609 (10)
#> row: ordered     40.666 (3)   37.156 (1)     3.5106 (2)
#> diff (gen-ord)  5.2914 (12)   3.1411 (4)     2.1503 (8)
#> 
print(cmh_mental, layout = "2x2", stars = TRUE)
#> Cochran-Mantel-Haenszel Statistics for ses by mental 
#>  Row/column display: general (nominal) vs. ordered (scored)
#>  Cell values: X^2 (df)
#> 
#>                  col: general  col: ordered diff (gen-ord)
#> row: general   45.958 (15)*** 40.297 (5)***    5.6609 (10)
#> row: ordered    40.666 (3)*** 37.156 (1)***     3.5106 (2)
#> diff (gen-ord)    5.2914 (12)    3.1411 (4)     2.1503 (8)
#> 
#> Signif. codes: '***' p<.001  '**' p<.01  '*' p<.05
#> 
print(cmh_mental, layout = "2x2", scale = TRUE)
#> Cochran-Mantel-Haenszel Statistics for ses by mental 
#>  Row/column display: general (nominal) vs. ordered (scored)
#>  Cell values: X^2/df
#> 
#>                col: general col: ordered diff (gen-ord)
#> row: general         3.0638       8.0593        0.56609
#> row: ordered         13.555       37.156         1.7553
#> diff (gen-ord)      0.44095      0.78527        0.26879
#> 


# Stokes etal, Table 5.1, p 92: two unordered factors
parties <- matrix(
  c(221, 160, 360, 140,
    200, 291, 160, 311,
    208, 106, 316, 97),
  nrow=3, ncol=4,
  byrow=TRUE)
dimnames(parties) <- list(party=c("Dem", "Indep", "Rep"),
             neighborhood=c("Bayside", "Highland", "Longview", "Sheffield"))
CMHtest(parties, rscores=NULL, cscores=NULL)
#> Cochran-Mantel-Haenszel Statistics for party by neighborhood 
#> 
#>               AltHypothesis  Chisq Df       Prob
#> general General association 273.81  6 3.3158e-56
#> 

# compare with Pearson chisquare
chisq.test(parties)
#> 
#>  Pearson's Chi-squared test
#> 
#> data:  parties
#> X-squared = 273.92, df = 6, p-value < 2.2e-16
#> 
```
