# Convert frequency or case form data into table form

Converts object (`obj`) in frequency or case form into table form. The
column containing the frequencies (`freq`) must be supplied if `obj` is
in frequency form. Optionally returns a table of proportions with
(optionally) specified margins.

## Usage

``` r
as_table(obj, freq = NULL, dims = NULL, prop = NULL)
```

## Arguments

- obj:

  Object to be converted to table form.

- freq:

  If `obj` is in frequency form, this is the name of the frequency
  column. Leave as `NULL` if `obj` is in any other form.

- dims:

  A character vector of dimensions. If not specified, all variables
  apart from `freq` will be used as dimensions.

- prop:

  If set to `TRUE`, returns a table of proportions (that sum to 1). May
  also be set to a character or numeric vector of dimensions to be used
  as margins from which proportions will be computed.

## Value

Object in table form.

## Details

If `obj` was in table form to begin with, it is returned to the user
as-is unless dimensions were specified (in which case it returns a table
with entries summed over excluded dimensions). When `prop` is set to
`TRUE`, the returned table will have proportions that sum to one,
whereas if a character or numerical vector of table dimensions is
supplied to `prop`, proportions will be marginalized across the
specified dimensions.

## See also

[`as_freqform`](https://friendly.github.io/vcdExtra/reference/as_freqform.md),
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
tidy_freqForm <- dplyr::as_tibble(HairEyeColor) # Generate tidy frequency form data
caseForm <- expand.dft(freqForm) # Generate case form data

# Frequency form -> table form
as_table(freqForm, freq = "Freq") |> str()
#>  'xtabs' num [1:4, 1:4, 1:2] 32 53 10 3 11 50 10 30 10 25 ...
#>  - attr(*, "dimnames")=List of 3
#>   ..$ Hair: chr [1:4] "Black" "Brown" "Red" "Blond"
#>   ..$ Eye : chr [1:4] "Brown" "Blue" "Hazel" "Green"
#>   ..$ Sex : chr [1:2] "Male" "Female"
#>  - attr(*, "call")= language xtabs(formula = reformulate(cols, response = freq), data = obj)

# Warned if forgot to specify freq
as_table(freqForm) |> str()
#> Warning: Ensure a value for 'freq' was supplied if your data was in frequency form.
#>  'xtabs' int [1:4, 1:4, 1:2, 1:22] 0 0 0 0 0 0 0 0 0 0 ...
#>  - attr(*, "dimnames")=List of 4
#>   ..$ Hair: chr [1:4] "Black" "Brown" "Red" "Blond"
#>   ..$ Eye : chr [1:4] "Brown" "Blue" "Hazel" "Green"
#>   ..$ Sex : chr [1:2] "Male" "Female"
#>   ..$ Freq: chr [1:22] "2" "3" "4" "5" ...
#>  - attr(*, "call")= language xtabs(formula = reformulate(cols), data = obj)

# Frequency form (tibble) -> table form
as_table(tidy_freqForm, freq = "n") |> str()
#>  'xtabs' num [1:4, 1:4, 1:2] 9 64 34 7 36 4 66 16 2 8 ...
#>  - attr(*, "dimnames")=List of 3
#>   ..$ Hair: chr [1:4] "Black" "Blond" "Brown" "Red"
#>   ..$ Eye : chr [1:4] "Blue" "Brown" "Green" "Hazel"
#>   ..$ Sex : chr [1:2] "Female" "Male"
#>  - attr(*, "call")= language xtabs(formula = reformulate(cols, response = freq), data = obj)

# Case form -> table form
as_table(caseForm) |> str()
#>  'xtabs' int [1:4, 1:4, 1:2] 9 64 34 7 36 4 66 16 2 8 ...
#>  - attr(*, "dimnames")=List of 3
#>   ..$ Hair: chr [1:4] "Black" "Blond" "Brown" "Red"
#>   ..$ Eye : chr [1:4] "Blue" "Brown" "Green" "Hazel"
#>   ..$ Sex : chr [1:2] "Female" "Male"
#>  - attr(*, "call")= language xtabs(formula = reformulate(cols), data = obj)

# For specific dimensions
as_table(tidy_freqForm, freq = "n", dims = c("Hair", "Eye")) |> str()
#>  'xtabs' num [1:4, 1:4] 20 94 84 17 68 7 119 26 5 16 ...
#>  - attr(*, "dimnames")=List of 2
#>   ..$ Hair: chr [1:4] "Black" "Blond" "Brown" "Red"
#>   ..$ Eye : chr [1:4] "Blue" "Brown" "Green" "Hazel"
#>  - attr(*, "call")= language xtabs(formula = reformulate(cols, response = freq), data = obj)

#-----For proportions-----#

as_table(freqForm, freq = "Freq", prop = TRUE) |> head(c(4,4,1)) # print only Sex == Male rows
#> , , Sex = Male
#> 
#>        Eye
#> Hair          Brown        Blue       Hazel       Green
#>   Black 0.054054054 0.018581081 0.016891892 0.005067568
#>   Brown 0.089527027 0.084459459 0.042229730 0.025337838
#>   Red   0.016891892 0.016891892 0.011824324 0.011824324
#>   Blond 0.005067568 0.050675676 0.008445946 0.013513514
#> 

# Marginalize proportions along "Sex" (i.e., male proportions sum to 1, female proportions sum to 1)
as_table(freqForm, freq = "Freq", prop = "Sex") |> head(c(4,4,1))
#> , , Sex = Male
#> 
#>        Eye
#> Hair         Brown       Blue      Hazel      Green
#>   Black 0.11469534 0.03942652 0.03584229 0.01075269
#>   Brown 0.18996416 0.17921147 0.08960573 0.05376344
#>   Red   0.03584229 0.03584229 0.02508961 0.02508961
#>   Blond 0.01075269 0.10752688 0.01792115 0.02867384
#> 

as_table(freqForm, freq = "Freq", prop = 3) |> head(c(4,4,1)) # Same as above
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
as_table(freqForm, freq = "Freq", prop = c("Hair", "Sex")) |> head(c(4,4,1))
#> , , Sex = Male
#> 
#>        Eye
#> Hair         Brown       Blue      Hazel      Green
#>   Black 0.57142857 0.19642857 0.17857143 0.05357143
#>   Brown 0.37062937 0.34965035 0.17482517 0.10489510
#>   Red   0.29411765 0.29411765 0.20588235 0.20588235
#>   Blond 0.06521739 0.65217391 0.10869565 0.17391304
#> 

as_table(freqForm, freq = "Freq", prop = c(1, 3)) |> head(c(4,4,1)) # Same as above
#> , , Sex = Male
#> 
#>        Eye
#> Hair         Brown       Blue      Hazel      Green
#>   Black 0.57142857 0.19642857 0.17857143 0.05357143
#>   Brown 0.37062937 0.34965035 0.17482517 0.10489510
#>   Red   0.29411765 0.29411765 0.20588235 0.20588235
#>   Blond 0.06521739 0.65217391 0.10869565 0.17391304
#> 

# Using dims and prop arguments in tandem
as_table(freqForm, freq = "Freq", dims = c("Hair", "Eye"), prop = TRUE)
#>        Eye
#> Hair          Brown        Blue       Hazel       Green
#>   Black 0.114864865 0.033783784 0.025337838 0.008445946
#>   Brown 0.201013514 0.141891892 0.091216216 0.048986486
#>   Red   0.043918919 0.028716216 0.023648649 0.023648649
#>   Blond 0.011824324 0.158783784 0.016891892 0.027027027

```
