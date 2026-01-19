# Social Mobility data

Data on social mobility, recording the occupational category of fathers
and their sons.

## Format

A 2-dimensional array resulting from cross-tabulating 2 variables for
19912 observations. The variable names and their levels are:

|     |                       |                                                          |
|-----|-----------------------|----------------------------------------------------------|
| No  | Name                  | Levels                                                   |
| 1   | `Son's_Occupation`    | `"UpNonMan", "LoNonMan", "UpManual", "LoManual", "Farm"` |
| 2   | `Father's_Occupation` | `"UpNonMan", "LoNonMan", "UpManual", "LoManual", "Farm"` |

## Source

Falguerolles, A. de and Mathieu, J. R. (1988). *Proceedings of COMPSTAT
88*, Copenhagen, Denmark, Springer-Verlag.

% FeathermanHauser:78

Featherman, D. L. and Hauser, R. M. Occupations and social mobility in
the United States. *Sociological Microjournal*, 12, Fiche 62.
Copenhagen: Sociological Institute.

## See also

[`Glass`](https://friendly.github.io/vcdExtra/reference/Glass.md),
[`Hauser79`](https://friendly.github.io/vcdExtra/reference/Hauser79.md),
[`Yamaguchi87`](https://friendly.github.io/vcdExtra/reference/Yamaguchi87.md)
for other examples of mobility data.

## Examples

``` r
data(Mobility)
Mobility
#>               Father_Occupation
#> Son_Occupation UpNonMan LoNonMan UpManual LoManual Farm
#>       UpNonMan     1414      724      798      756  409
#>       LoNonMan      521      524      648      914  357
#>       UpManual      302      254      856      771  441
#>       LoManual      643      703     1676     3325 1611
#>       Farm           40       48      108      237 1832

# independence model
MASS::loglm(~Father_Occupation + Son_Occupation, data = Mobility)
#> Call:
#> MASS::loglm(formula = ~Father_Occupation + Son_Occupation, data = Mobility)
#> 
#> Statistics:
#>                       X^2 df P(> X^2)
#> Likelihood Ratio 6170.130 16        0
#> Pearson          7166.771 16        0

vcd::mosaic(Mobility, shade=TRUE, legend = FALSE)



```
