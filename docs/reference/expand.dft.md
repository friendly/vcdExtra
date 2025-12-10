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
[`expandCategorical`](https://rdrr.io/pkg/gnm/man/expandCategorical.html)

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
#> a b c d e f 
#> 7 3 1 1 2 6 
expand.table(tab, var.names="letter")
#>    letter
#> 1       a
#> 2       a
#> 3       a
#> 4       a
#> 5       a
#> 6       a
#> 7       a
#> 8       b
#> 9       b
#> 10      b
#> 11      c
#> 12      d
#> 13      e
#> 14      e
#> 15      f
#> 16      f
#> 17      f
#> 18      f
#> 19      f
#> 20      f

```
