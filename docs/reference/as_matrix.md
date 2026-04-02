# Convert frequency, case, or table form data into a matrix.

Converts object (`obj`) in frequency, case or table form into a matrix
of specified dimensions (`dims`). The column containing the frequencies
(`freq`) must be supplied if `obj` is in frequency form.

## Usage

``` r
as_matrix(obj, freq = NULL, dims = NULL, prop = NULL)
```

## Arguments

- obj:

  Object to be converted into a matrix.

- freq:

  If `obj` is in frequency form, this is the name of the frequency
  column. Leave as `NULL` if `obj` is in any other form.

- dims:

  A character vector of dimensions. If not specified, all variables
  apart from `freq` will be used as dimensions.

- prop:

  If set to `TRUE`, returns a matrix of proportions (that sum to 1). May
  also be set to a character or numeric vector of dimensions to be used
  as margins from which proportions will be computed.

## Value

Object in matrix form.

## Details

First converts `obj` into an array using
[`as_array`](https://friendly.github.io/vcdExtra/reference/as_array.md).
Then a check is made to ensure the user inputted a 2D `obj`. If `obj` is
not 2D, an error is returned. If `obj` is 2D,
[`as.matrix`](https://rdrr.io/r/base/matrix.html) is applied.

## See also

[`as_array`](https://friendly.github.io/vcdExtra/reference/as_array.md),
[`as_table`](https://friendly.github.io/vcdExtra/reference/as_table.md),
[`as_freqform`](https://friendly.github.io/vcdExtra/reference/as_freqform.md),
[`as_caseform`](https://friendly.github.io/vcdExtra/reference/as_caseform.md)

## Author

Gavin M. Klorfine

## Examples

``` r
library(vcdExtra)

data("HairEyeColor")

freqForm <- as.data.frame(HairEyeColor) # Generate frequency form data
tidy_freqForm <- dplyr::as_tibble(HairEyeColor) # Generate tidy frequency form data
caseForm <- expand.dft(freqForm) # Generate case form data
arrayDat <- as_array(HairEyeColor) # Generate an array

# Table form -> matrix
as_matrix(HairEyeColor, dims = c("Hair", "Sex")) |> str()
#>  num [1:4, 1:2] 56 143 34 46 52 143 37 81
#>  - attr(*, "dimnames")=List of 2
#>   ..$ Hair: chr [1:4] "Black" "Brown" "Red" "Blond"
#>   ..$ Sex : chr [1:2] "Male" "Female"

# Frequency form -> matrix
as_matrix(freqForm, freq = "Freq", dims = c("Hair", "Sex")) |> str()
#>  num [1:4, 1:2] 56 143 34 46 52 143 37 81
#>  - attr(*, "dimnames")=List of 2
#>   ..$ Hair: chr [1:4] "Black" "Brown" "Red" "Blond"
#>   ..$ Sex : chr [1:2] "Male" "Female"
#>  - attr(*, "call")= language xtabs(formula = reformulate(cols, response = freq), data = obj)

# Case form -> matrix form
as_matrix(caseForm, dims = c("Hair", "Sex")) |> str()
#>  int [1:4, 1:2] 52 81 143 37 56 46 143 34
#>  - attr(*, "dimnames")=List of 2
#>   ..$ Hair: chr [1:4] "Black" "Blond" "Brown" "Red"
#>   ..$ Sex : chr [1:2] "Female" "Male"
#>  - attr(*, "call")= language xtabs(formula = reformulate(cols), data = obj)

# Frequency (tibble) form -> matrix form
as_matrix(tidy_freqForm, freq = "n", dims = c("Hair", "Sex")) |> str()
#>  num [1:4, 1:2] 52 81 143 37 56 46 143 34
#>  - attr(*, "dimnames")=List of 2
#>   ..$ Hair: chr [1:4] "Black" "Blond" "Brown" "Red"
#>   ..$ Sex : chr [1:2] "Female" "Male"
#>  - attr(*, "call")= language xtabs(formula = reformulate(cols, response = freq), data = obj)

#-----For proportions-----#

# Proportions relative to grand total
as_matrix(HairEyeColor, dims = c("Hair", "Sex"), prop = TRUE)
#>        Sex
#> Hair          Male     Female
#>   Black 0.09459459 0.08783784
#>   Brown 0.24155405 0.24155405
#>   Red   0.05743243 0.06250000
#>   Blond 0.07770270 0.13682432

# Marginalize proportions along "Sex" (i.e., male proportions sum to 1, 
# female proportions sum to 1)
as_matrix(HairEyeColor, dims = c("Hair", "Sex"), prop = "Sex")
#>        Sex
#> Hair         Male    Female
#>   Black 0.2007168 0.1661342
#>   Brown 0.5125448 0.4568690
#>   Red   0.1218638 0.1182109
#>   Blond 0.1648746 0.2587859

as_matrix(HairEyeColor, dims = c("Hair", "Sex"), prop = 2) # Same as above
#>        Sex
#> Hair         Male    Female
#>   Black 0.2007168 0.1661342
#>   Brown 0.5125448 0.4568690
#>   Red   0.1218638 0.1182109
#>   Blond 0.1648746 0.2587859

```
