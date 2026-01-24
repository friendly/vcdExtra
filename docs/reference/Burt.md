# Burt (1950) Data on Hair, Eyes, Head and Stature

Cyril Burt (1950) gave these data, on a sample of 100 people from
Liverpool, to illustrate the application of a method of factor analysis
(later called multiple correspondence analysis) applied to categorical
data.

## Format

A frequency data frame (representing a 3 x 3 x 2 x 2 frequency table)
with 36 cells on the following 5 variables.

- `Hair`:

  hair color, a factor with levels `Fair` `Red` `Dark`

- `Eyes`:

  eye color, a factor with levels `Light` `Mixed` `Dark`

- `Head`:

  head shape, a factor with levels `Narrow` `Wide`

- `Stature`:

  height, a factor with levels `Tall` `Short`

- `Freq`:

  a numeric vector

## Source

Burt, C. (1950). The factorial analysis of qualitative data, *British
Journal of Statistical Psychology*, **3**(3), 166-185. Table IX.

## Details

He presented these data initially in the form that has come to be called
a "Burt table", giving the univariate and bivariate frequencies for an
n-way frequency table.

Burt says: "In all, 217 individuals were examined, about two-thirds of
them males. But, partly to simplify the calculations and partly because
the later observations were rather more trustworthy, I shall here
restrict my analysis to the data obtained from the last hundred males in
the series."

`Head` and `Stature` reflect a binary coding where people are classified
according to whether they are below or above the average for the
population.

## Examples

``` r
data(Burt)
mosaic(Freq ~ Hair + Eyes + Head + Stature, data=Burt, shade=TRUE)
#> Error in eval(predvars, data, env): object 'Hair' not found

#or
burt.tab <- xtabs(Freq ~ Hair + Eyes + Head + Stature, data=Burt)
#> Error in eval(predvars, data, env): object 'Freq' not found
mosaic(burt.tab, shade=TRUE)
#> Error: object 'burt.tab' not found
```
