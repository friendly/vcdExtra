# Collapse Levels of a Table

Collapse (or re-label) variables in a a contingency table, array or
`ftable` object by re-assigning levels of the table variables.

## Usage

``` r
collapse.table(table, ...)
```

## Arguments

- table:

  A [`table`](https://rdrr.io/r/base/table.html),
  [`array`](https://rdrr.io/r/base/array.html) or
  [`ftable`](https://rdrr.io/r/stats/ftable.html) object

- ...:

  A collection of one or more assignments of factors of the table to a
  list of levels

## Value

A `xtabs` and `table` object, representing the original table with one
or more of its factors collapsed or rearranged into other levels.

## Details

Each of the `...{}` arguments must be of the form `variable = levels`,
where `variable` is the name of one of the table dimensions, and
`levels` is a character or numeric vector of length equal to the
corresponding dimension of the table.

## See also

[`expand.dft`](https://friendly.github.io/vcdExtra/reference/expand.dft.md)
expands a frequency data frame to case form.

[`margin.table`](https://rdrr.io/r/base/marginSums.html) "collapses" a
table in a different way, by summing over table dimensions.

## Author

Michael Friendly

## Examples

``` r
# create some sample data in table form
sex <- c("Male", "Female")
age <- letters[1:6]
education <- c("low", 'med', 'high')
data <- expand.grid(sex=sex, age=age, education=education)
counts <- rpois(36, 100) 
data <- cbind(data, counts)
t1 <- xtabs(counts ~ sex + age + education, data=data)
structable(t1)
#>                  age   a   b   c   d   e   f
#> sex    education                            
#> Male   low           121  93 106  80 114 107
#>        med           100  97  86 104 103  89
#>        high          105 114  96  91 108  98
#> Female low            85 103 110 102  98 100
#>        med            92  96 106 100 112  98
#>        high          117 115  94  93 118 109

##                  age   a   b   c   d   e   f
## sex    education                            
## Male   low           119 101 109  85  99  93
##        med            94  98 103 108  84  84
##        high           81  88  96 110 100  92
## Female low           107 104  95  86 103  96
##        med           104  98  94  95 110 106
##        high           93  85  90 109  99  86


# collapse age to 3 levels
t2 <- collapse.table(t1, age=c("A", "A", "B", "B", "C", "C"))
structable(t2)
#>                  age   A   B   C
#> sex    education                
#> Male   low           214 186 221
#>        med           197 190 192
#>        high          219 187 206
#> Female low           188 212 198
#>        med           188 206 210
#>        high          232 187 227

##                  age   A   B   C
## sex    education                
## Male   low           220 194 192
##        med           192 211 168
##        high          169 206 192
## Female low           211 181 199
##        med           202 189 216
##        high          178 199 185


# collapse age to 3 levels and pool education: "low" and "med" to "low"
t3 <- collapse.table(t1, age=c("A", "A", "B", "B", "C", "C"), 
    education=c("low", "low", "high"))
structable(t3)
#>                  age   A   B   C
#> sex    education                
#> Male   low           411 376 413
#>        high          219 187 206
#> Female low           376 418 408
#>        high          232 187 227

##                  age   A   B   C
## sex    education                
## Male   low           412 405 360
##        high          169 206 192
## Female low           413 370 415
##        high          178 199 185



# change labels for levels of education to 1:3
t4 <- collapse.table(t1,  education=1:3)
structable(t4)
#>                  age   a   b   c   d   e   f
#> sex    education                            
#> Male   1             121  93 106  80 114 107
#>        2             100  97  86 104 103  89
#>        3             105 114  96  91 108  98
#> Female 1              85 103 110 102  98 100
#>        2              92  96 106 100 112  98
#>        3             117 115  94  93 118 109

structable(t4)
#>                  age   a   b   c   d   e   f
#> sex    education                            
#> Male   1             121  93 106  80 114 107
#>        2             100  97  86 104 103  89
#>        3             105 114  96  91 108  98
#> Female 1              85 103 110 102  98 100
#>        2              92  96 106 100 112  98
#>        3             117 115  94  93 118 109
##                  age   a   b   c   d   e   f
## sex    education                            
## Male   1             119 101 109  85  99  93
##        2              94  98 103 108  84  84
##        3              81  88  96 110 100  92
## Female 1             107 104  95  86 103  96
##        2             104  98  94  95 110 106
##        3              93  85  90 109  99  86



```
