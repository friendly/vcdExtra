# Convert any form (case or table form) into frequency form.

A wrapper for
[`as.data.frame()`](https://rdrr.io/r/base/as.data.frame.html) that is
able to properly handle arrays. Converts object (`obj`) in case or table
form into frequency form. The column containing the frequencies (`freq`)
must be supplied if `obj` is already in frequency form (and you are
using this function to select dimensions). Returns a tibble if `tidy` is
set to `TRUE`.

## Usage

``` r
as_freqform(obj, freq = NULL, dims = NULL, tidy = TRUE)
```

## Arguments

- obj:

  object to be converted to frequency form

- freq:

  If `obj` is already in frequency form, this is the name of the
  frequency column. If `obj` is in any other form, do not supply an
  argument (see "Details")

- dims:

  A character vector of dimensions. If not specified, all variables
  apart from `freq` will be used as dimensions

- tidy:

  returns a tibble if set to TRUE

## Value

object in frequency form.

## Details

Converts `obj` to a table using
[`as_table()`](https://friendly.github.io/vcdExtra/reference/as_table.md)
before converting to frequency form

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

```
