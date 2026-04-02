# Convert frequency, case, or table form data into an array

Converts object (`obj`) in frequency, case or table form into an array.
The column containing the frequencies (`freq`) must be supplied if `obj`
is in frequency form.

## Usage

``` r
as_array(obj, freq = NULL, dims = NULL, prop = NULL)
```

## Arguments

- obj:

  Object to be converted to an array.

- freq:

  If `obj` is in frequency form, this is the name of the frequency
  column. Leave as `NULL` if `obj` is in any other form.

- dims:

  A character vector of dimensions. If not specified, all variables
  apart from `freq` will be used as dimensions.

- prop:

  If set to `TRUE`, returns an array of proportions (that sum to 1). May
  also be set to a character or numeric vector of dimensions to be used
  as margins from which proportions will be computed.

## Value

object in array form

## Details

Unclasses the
[`as_table`](https://friendly.github.io/vcdExtra/reference/as_table.md)
function to return an object in array form.

## See also

[`as_table`](https://friendly.github.io/vcdExtra/reference/as_table.md),
[`as_freqform`](https://friendly.github.io/vcdExtra/reference/as_freqform.md),
[`as_caseform`](https://friendly.github.io/vcdExtra/reference/as_caseform.md),
[`as_matrix`](https://friendly.github.io/vcdExtra/reference/as_matrix.md)

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

#-----For proportions-----#

as_array(freqForm, freq = "Freq", prop = TRUE) |> # proportions relative to grand total
  head(c(4,4,1)) 
#> , , Sex = Male
#> 
#>        Eye
#> Hair          Brown       Blue       Hazel       Green
#>   Black 0.054054054 0.01858108 0.016891892 0.005067568
#>   Brown 0.089527027 0.08445946 0.042229730 0.025337838
#>   Red   0.016891892 0.01689189 0.011824324 0.011824324
#>   Blond 0.005067568 0.05067568 0.008445946 0.013513514
#> 

# Marginalize proportions along "Sex" (i.e., male proportions sum to 1, female proportions sum to 1)
as_array(freqForm, freq = "Freq", prop = "Sex") |> head(c(4,4,1))
#> , , Sex = Male
#> 
#>        Eye
#> Hair         Brown       Blue      Hazel      Green
#>   Black 0.11469534 0.03942652 0.03584229 0.01075269
#>   Brown 0.18996416 0.17921147 0.08960573 0.05376344
#>   Red   0.03584229 0.03584229 0.02508961 0.02508961
#>   Blond 0.01075269 0.10752688 0.01792115 0.02867384
#> 

as_array(freqForm, freq = "Freq", prop = 3) |> head(c(4,4,1)) # Same as above
#> , , Sex = Male
#> 
#>        Eye
#> Hair         Brown       Blue      Hazel      Green
#>   Black 0.11469534 0.03942652 0.03584229 0.01075269
#>   Brown 0.18996416 0.17921147 0.08960573 0.05376344
#>   Red   0.03584229 0.03584229 0.02508961 0.02508961
#>   Blond 0.01075269 0.10752688 0.01792115 0.02867384
#> 

# Marginalize proportions along multiple variables
as_array(freqForm, freq = "Freq", prop = c("Hair", "Sex")) |> head(c(4,4,1))
#> , , Sex = Male
#> 
#>        Eye
#> Hair         Brown      Blue     Hazel      Green
#>   Black 0.57142857 0.1964286 0.1785714 0.05357143
#>   Brown 0.37062937 0.3496503 0.1748252 0.10489510
#>   Red   0.29411765 0.2941176 0.2058824 0.20588235
#>   Blond 0.06521739 0.6521739 0.1086957 0.17391304
#> 

as_array(freqForm, freq = "Freq", prop = c(1, 3)) |> head(c(4,4,1)) # Same as above
#> , , Sex = Male
#> 
#>        Eye
#> Hair         Brown      Blue     Hazel      Green
#>   Black 0.57142857 0.1964286 0.1785714 0.05357143
#>   Brown 0.37062937 0.3496503 0.1748252 0.10489510
#>   Red   0.29411765 0.2941176 0.2058824 0.20588235
#>   Blond 0.06521739 0.6521739 0.1086957 0.17391304
#> 

# Using dims and prop arguments in tandem
as_array(freqForm, freq = "Freq", dims = c("Hair", "Eye"), prop = TRUE)
#>        Eye
#> Hair         Brown       Blue      Hazel       Green
#>   Black 0.11486486 0.03378378 0.02533784 0.008445946
#>   Brown 0.20101351 0.14189189 0.09121622 0.048986486
#>   Red   0.04391892 0.02871622 0.02364865 0.023648649
#>   Blond 0.01182432 0.15878378 0.01689189 0.027027027
#> attr(,"call")
#> xtabs(formula = reformulate(cols, response = freq), data = obj)

```
