# Convert frequency, case, or table form data into an array

Converts object (`obj`) in frequency, case or table form into an array.
The column containing the frequencies (`freq`) must be supplied if `obj`
is in frequency form.

## Usage

``` r
as_array(obj, freq = NULL, dims = NULL)
```

## Arguments

- obj:

  object to be converted to an array

- freq:

  If `obj` is in frequency form, this is the name of the frequency
  column. Leave as `NULL` if `obj` is in any other form.

- dims:

  A character vector of dimensions. If not specified, all variables
  apart from `freq` will be used as dimensions

## Value

object in array form

## Details

Unclasses the
[`as_table()`](https://friendly.github.io/vcdExtra/reference/as_table.md)
function to return an object in array form.

## Author

Gavin M. Klorfine

## Examples

``` r
library(vcdExtra)

data("HairEyeColor")

freqForm <- as.data.frame(HairEyeColor) # Generate frequency form data
tidy_freqForm <- dplyr::as_tibble(HairEyeColor) # Generate tidy frequency form data
caseForm <- expand.dft(freqForm) # Generate case form data

# Frequency form -> array form
as_array(freqForm, freq = "Freq") |> str()
#>  num [1:4, 1:4, 1:2] 32 53 10 3 11 50 10 30 10 25 ...
#>  - attr(*, "dimnames")=List of 3
#>   ..$ Hair: chr [1:4] "Black" "Brown" "Red" "Blond"
#>   ..$ Eye : chr [1:4] "Brown" "Blue" "Hazel" "Green"
#>   ..$ Sex : chr [1:2] "Male" "Female"
#>  - attr(*, "call")= language xtabs(formula = reformulate(cols, response = freq), data = obj)

# Warned if forgot to specify freq
as_array(freqForm) |> str()
#> Warning: Ensure a value for 'freq' was supplied if your data was in frequency form.
#>  int [1:4, 1:4, 1:2, 1:22] 0 0 0 0 0 0 0 0 0 0 ...
#>  - attr(*, "dimnames")=List of 4
#>   ..$ Hair: chr [1:4] "Black" "Brown" "Red" "Blond"
#>   ..$ Eye : chr [1:4] "Brown" "Blue" "Hazel" "Green"
#>   ..$ Sex : chr [1:2] "Male" "Female"
#>   ..$ Freq: chr [1:22] "2" "3" "4" "5" ...
#>  - attr(*, "call")= language xtabs(formula = reformulate(cols), data = obj)

# Case form -> array form
as_array(caseForm) |> str()
#>  int [1:4, 1:4, 1:2] 9 64 34 7 36 4 66 16 2 8 ...
#>  - attr(*, "dimnames")=List of 3
#>   ..$ Hair: chr [1:4] "Black" "Blond" "Brown" "Red"
#>   ..$ Eye : chr [1:4] "Blue" "Brown" "Green" "Hazel"
#>   ..$ Sex : chr [1:2] "Female" "Male"
#>  - attr(*, "call")= language xtabs(formula = reformulate(cols), data = obj)

# Frequency (tibble) form -> array form
as_array(tidy_freqForm, freq = "n") |> str()
#>  num [1:4, 1:4, 1:2] 9 64 34 7 36 4 66 16 2 8 ...
#>  - attr(*, "dimnames")=List of 3
#>   ..$ Hair: chr [1:4] "Black" "Blond" "Brown" "Red"
#>   ..$ Eye : chr [1:4] "Blue" "Brown" "Green" "Hazel"
#>   ..$ Sex : chr [1:2] "Female" "Male"
#>  - attr(*, "call")= language xtabs(formula = reformulate(cols, response = freq), data = obj)

# For specific dimensions
as_array(tidy_freqForm, freq = "n", dims = c("Hair", "Eye")) |> str()
#>  num [1:4, 1:4] 20 94 84 17 68 7 119 26 5 16 ...
#>  - attr(*, "dimnames")=List of 2
#>   ..$ Hair: chr [1:4] "Black" "Blond" "Brown" "Red"
#>   ..$ Eye : chr [1:4] "Blue" "Brown" "Green" "Hazel"
#>  - attr(*, "call")= language xtabs(formula = reformulate(cols, response = freq), data = obj)

```
