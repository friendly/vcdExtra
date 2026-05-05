# Convert any form (case or table form) into frequency form.

A wrapper for
[`as.data.frame`](https://rdrr.io/r/base/as.data.frame.html) that is
able to properly handle arrays. Converts object (`obj`) in case or table
form into frequency form. The column containing the frequencies (`freq`)
must be supplied if `obj` is already in frequency form (and you are
using this function to select dimensions). Returns a tibble if `tidy` is
set to `TRUE`.

## Usage

``` r
as_freqform(obj, freq = NULL, dims = NULL, prop = NULL, tidy = TRUE)
```

## Arguments

- obj:

  Object to be converted to frequency form.

- freq:

  If `obj` is already in frequency form, this is the name of the
  frequency column. If `obj` is in any other form, do not supply an
  argument (see "Details").

- dims:

  A character vector of dimensions. If not specified, all variables
  apart from `freq` will be used as dimensions.

- prop:

  If set to `TRUE`, the resulting "frequency" column will contain
  proportions (that sum to 1). May also be set to a character or numeric
  vector of dimensions to be used as margins from which proportions will
  be computed. The resulting "frequency" column is renamed to "Prop."

- tidy:

  Returns a tibble if set to `TRUE`.

## Value

Object in frequency form.

## Details

Converts `obj` to a table using
[`as_table`](https://friendly.github.io/vcdExtra/reference/as_table.md)
before converting to frequency form

## See also

[`as_table`](https://friendly.github.io/vcdExtra/reference/as_table.md),
[`as_caseform`](https://friendly.github.io/vcdExtra/reference/as_caseform.md),
[`as_array`](https://friendly.github.io/vcdExtra/reference/as_array.md),
[`as_matrix`](https://friendly.github.io/vcdExtra/reference/as_matrix.md)

## Author

Gavin M. Klorfine

## Examples

``` r
library(vcdExtra)

data("HairEyeColor")

freqForm <- as.data.frame(HairEyeColor) # Generate frequency form data
tableForm <- as_table(HairEyeColor) # Generate table form data
arrayDat <- as_array(HairEyeColor) # Generate an array
caseForm <- as_caseform(HairEyeColor) # Generate case form data

# array -> frequency form
as_freqform(arrayDat) |> str()
#> tibble [32 × 4] (S3: tbl_df/tbl/data.frame)
#>  $ Hair: Factor w/ 4 levels "Black","Brown",..: 1 2 3 4 1 2 3 4 1 2 ...
#>  $ Eye : Factor w/ 4 levels "Brown","Blue",..: 1 1 1 1 2 2 2 2 3 3 ...
#>  $ Sex : Factor w/ 2 levels "Male","Female": 1 1 1 1 1 1 1 1 1 1 ...
#>  $ Freq: num [1:32] 32 53 10 3 11 50 10 30 10 25 ...

# table -> frequency form
as_freqform(tableForm) |> str()
#> tibble [32 × 4] (S3: tbl_df/tbl/data.frame)
#>  $ Hair: Factor w/ 4 levels "Black","Brown",..: 1 2 3 4 1 2 3 4 1 2 ...
#>  $ Eye : Factor w/ 4 levels "Brown","Blue",..: 1 1 1 1 2 2 2 2 3 3 ...
#>  $ Sex : Factor w/ 2 levels "Male","Female": 1 1 1 1 1 1 1 1 1 1 ...
#>  $ Freq: num [1:32] 32 53 10 3 11 50 10 30 10 25 ...

# case -> frequency form
as_freqform(caseForm) |> str()
#> tibble [32 × 4] (S3: tbl_df/tbl/data.frame)
#>  $ Hair: Factor w/ 4 levels "Black","Blond",..: 1 2 3 4 1 2 3 4 1 2 ...
#>  $ Eye : Factor w/ 4 levels "Blue","Brown",..: 1 1 1 1 2 2 2 2 3 3 ...
#>  $ Sex : Factor w/ 2 levels "Female","Male": 1 1 1 1 1 1 1 1 1 1 ...
#>  $ Freq: int [1:32] 9 64 34 7 36 4 66 16 2 8 ...

# Selecting dimensions (optional)
as_freqform(freqForm, freq = "Freq", dims = c("Hair", "Eye")) |> str()
#> tibble [16 × 3] (S3: tbl_df/tbl/data.frame)
#>  $ Hair: Factor w/ 4 levels "Black","Brown",..: 1 2 3 4 1 2 3 4 1 2 ...
#>  $ Eye : Factor w/ 4 levels "Brown","Blue",..: 1 1 1 1 2 2 2 2 3 3 ...
#>  $ Freq: num [1:16] 68 119 26 7 20 84 17 94 15 54 ...

as_freqform(tableForm, dims = c("Hair", "Eye")) |> str()
#> tibble [16 × 3] (S3: tbl_df/tbl/data.frame)
#>  $ Hair: Factor w/ 4 levels "Black","Brown",..: 1 2 3 4 1 2 3 4 1 2 ...
#>  $ Eye : Factor w/ 4 levels "Brown","Blue",..: 1 1 1 1 2 2 2 2 3 3 ...
#>  $ Freq: num [1:16] 68 119 26 7 20 84 17 94 15 54 ...

#-----For proportions-----#

as_freqform(tableForm, prop = TRUE) |> head() # print only Sex == Male rows
#> # A tibble: 6 × 4
#>   Hair  Eye   Sex      Prop
#>   <fct> <fct> <fct>   <dbl>
#> 1 Black Brown Male  0.0541 
#> 2 Brown Brown Male  0.0895 
#> 3 Red   Brown Male  0.0169 
#> 4 Blond Brown Male  0.00507
#> 5 Black Blue  Male  0.0186 
#> 6 Brown Blue  Male  0.0845 

# Marginalize proportions along "Sex" (i.e., male proportions sum to 1, female proportions sum to 1)
as_freqform(tableForm, prop = "Sex") |> head()
#> # A tibble: 6 × 4
#>   Hair  Eye   Sex     Prop
#>   <fct> <fct> <fct>  <dbl>
#> 1 Black Brown Male  0.115 
#> 2 Brown Brown Male  0.190 
#> 3 Red   Brown Male  0.0358
#> 4 Blond Brown Male  0.0108
#> 5 Black Blue  Male  0.0394
#> 6 Brown Blue  Male  0.179 

as_freqform(tableForm, prop = 3) |> head() # Same as above
#> # A tibble: 6 × 4
#>   Hair  Eye   Sex     Prop
#>   <fct> <fct> <fct>  <dbl>
#> 1 Black Brown Male  0.115 
#> 2 Brown Brown Male  0.190 
#> 3 Red   Brown Male  0.0358
#> 4 Blond Brown Male  0.0108
#> 5 Black Blue  Male  0.0394
#> 6 Brown Blue  Male  0.179 

# Marginalize proportions along multiple variables
as_freqform(tableForm, prop = c("Hair", "Sex")) |> head()
#> # A tibble: 6 × 4
#>   Hair  Eye   Sex     Prop
#>   <fct> <fct> <fct>  <dbl>
#> 1 Black Brown Male  0.571 
#> 2 Brown Brown Male  0.371 
#> 3 Red   Brown Male  0.294 
#> 4 Blond Brown Male  0.0652
#> 5 Black Blue  Male  0.196 
#> 6 Brown Blue  Male  0.350 

as_freqform(tableForm, prop = c(1, 3)) |> head() # Same as above
#> # A tibble: 6 × 4
#>   Hair  Eye   Sex     Prop
#>   <fct> <fct> <fct>  <dbl>
#> 1 Black Brown Male  0.571 
#> 2 Brown Brown Male  0.371 
#> 3 Red   Brown Male  0.294 
#> 4 Blond Brown Male  0.0652
#> 5 Black Blue  Male  0.196 
#> 6 Brown Blue  Male  0.350 

# Using dims and prop arguments in tandem
as_freqform(tableForm, dims = c("Hair", "Eye"), prop = TRUE)
#> # A tibble: 16 × 3
#>    Hair  Eye      Prop
#>    <fct> <fct>   <dbl>
#>  1 Black Brown 0.115  
#>  2 Brown Brown 0.201  
#>  3 Red   Brown 0.0439 
#>  4 Blond Brown 0.0118 
#>  5 Black Blue  0.0338 
#>  6 Brown Blue  0.142  
#>  7 Red   Blue  0.0287 
#>  8 Blond Blue  0.159  
#>  9 Black Hazel 0.0253 
#> 10 Brown Hazel 0.0912 
#> 11 Red   Hazel 0.0236 
#> 12 Blond Hazel 0.0169 
#> 13 Black Green 0.00845
#> 14 Brown Green 0.0490 
#> 15 Red   Green 0.0236 
#> 16 Blond Green 0.0270 
```
