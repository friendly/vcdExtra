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
and
[`as_caseform`](https://friendly.github.io/vcdExtra/reference/as_caseform.md):
expands a frequency data frame to case form.

[`margin.table`](https://rdrr.io/r/base/marginSums.html) "collapses" a
table in a different way, by summing over table dimensions.

[`collapse_levels`](https://friendly.github.io/vcdExtra/reference/collapse_levels.md)
collapses in the same manner as `collapse.table` but also works for
frequency and case form data.

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
#> Male   low            82 107  98  77  98  91
#>        med           111  92  95  99 102  95
#>        high          109 110 105 105 107 114
#> Female low           113  96  97 110 103  91
#>        med           116 118 106 113  93 101
#>        high           97  97 101 110  98 108

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
#> Male   low           189 175 189
#>        med           203 194 197
#>        high          219 210 221
#> Female low           209 207 194
#>        med           234 219 194
#>        high          194 211 206

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
#> Male   low           392 369 386
#>        high          219 210 221
#> Female low           443 426 388
#>        high          194 211 206

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
#> Male   1              82 107  98  77  98  91
#>        2             111  92  95  99 102  95
#>        3             109 110 105 105 107 114
#> Female 1             113  96  97 110 103  91
#>        2             116 118 106 113  93 101
#>        3              97  97 101 110  98 108

structable(t4)
#>                  age   a   b   c   d   e   f
#> sex    education                            
#> Male   1              82 107  98  77  98  91
#>        2             111  92  95  99 102  95
#>        3             109 110 105 105 107 114
#> Female 1             113  96  97 110 103  91
#>        2             116 118 106 113  93 101
#>        3              97  97 101 110  98 108
##                  age   a   b   c   d   e   f
## sex    education                            
## Male   1             119 101 109  85  99  93
##        2              94  98 103 108  84  84
##        3              81  88  96 110 100  92
## Female 1             107 104  95  86 103  96
##        2             104  98  94  95 110 106
##        3              93  85  90 109  99  86



```
