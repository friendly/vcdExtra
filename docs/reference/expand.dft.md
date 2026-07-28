# Expand a frequency table to case form

Converts a frequency table, given either as a table object or a data
frame in frequency form to a data frame representing individual
observations in the table.

## Usage

``` r
expand.dft(x, var.names = NULL, freq = "Freq", ...)
```

## Arguments

- x:

  A table object, or a data frame in frequency form containing factors
  and one numeric variable representing the cell frequency for that
  combination of factors.

- var.names:

  A list of variable names for the factors, if you wish to override
  those already in the table

- freq:

  The name of the frequency variable in the table

- ...:

  Other arguments passed down to `type.convert`. In particular, pay
  attention to `na.strings` (default: `na.strings=NA` if there are
  missing cells) and `as.is` (default: `as.is=FALSE`, converting
  character vectors to factors).

## Value

A data frame containing the factors in the table and as many
observations as are represented by the total of the `freq` variable.

## Details

`expand.table` is a synonym for `expand.dft`.

## References

Originally posted on R-Help, Jan 20, 2009,
http://tolstoy.newcastle.edu.au/R/e6/help/09/01/1873.html

Friendly, M. and Meyer, D. (2016). *Discrete Data Analysis with R:
Visualization and Modeling Techniques for Categorical and Count Data*.
Boca Raton, FL: Chapman & Hall/CRC. <http://ddar.datavis.ca>.

## See also

[`type.convert`](https://rdrr.io/r/utils/type.convert.html),
[`expandCategorical`](https://rdrr.io/pkg/gnm/man/expandCategorical.html),
[`as_caseform`](https://friendly.github.io/vcdExtra/reference/as_caseform.md),
[`as_table`](https://friendly.github.io/vcdExtra/reference/as_table.md),
[`as_freqform`](https://friendly.github.io/vcdExtra/reference/as_freqform.md),
[`as_array`](https://friendly.github.io/vcdExtra/reference/as_array.md),
[`as_matrix`](https://friendly.github.io/vcdExtra/reference/as_matrix.md)

## Author

Mark Schwarz

## Examples

``` r

library(vcd)
art <- xtabs(~Treatment + Improved, data = Arthritis)
art
#>          Improved
#> Treatment None Some Marked
#>   Placebo   29    7      7
#>   Treated   13    7     21
artdf <- expand.dft(art)
str(artdf)
#> 'data.frame':    84 obs. of  2 variables:
#>  $ Treatment: chr  "Placebo" "Placebo" "Placebo" "Placebo" ...
#>  $ Improved : chr  "None" "None" "None" "None" ...

# 1D case
(tab <- table(sample(head(letters), 20, replace=TRUE)))
#> 
#> a c d e f 
#> 4 2 2 3 9 
expand.table(tab, var.names="letter")
#>    letter
#> 1       a
#> 2       a
#> 3       a
#> 4       a
#> 5       c
#> 6       c
#> 7       d
#> 8       d
#> 9       e
#> 10      e
#> 11      e
#> 12      f
#> 13      f
#> 14      f
#> 15      f
#> 16      f
#> 17      f
#> 18      f
#> 19      f
#> 20      f

```
