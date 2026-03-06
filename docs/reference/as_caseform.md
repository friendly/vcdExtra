# Convert frequency or table form into case form.

Converts object (`obj`) in frequency or table form into case form. The
column containing the frequencies (`freq`) must be supplied if `obj` is
in frequency form. Returns a tibble if `tidy` is set to `TRUE`.

## Usage

``` r
as_caseform(obj, freq = "Freq", dims = NULL, tidy = TRUE)
```

## Arguments

- obj:

  object to be converted to case form

- freq:

  If `obj` is in frequency form, this is the name of the frequency
  column. If `obj` is in any other form, do not supply an argument (see
  "Details")

- dims:

  A character vector of dimensions. If not specified, all variables
  apart from `freq` will be used as dimensions

- tidy:

  returns a tibble if set to TRUE

## Value

object in case form.

## Details

A wrapper for
[`expand.dft()`](https://friendly.github.io/vcdExtra/reference/expand.dft.md)
that is able to handle arrays.

If a frequency column is not supplied, this function defaults to "Freq"
just like
[`expand.dft()`](https://friendly.github.io/vcdExtra/reference/expand.dft.md).
Converts `obj` to a table using
[`as_table()`](https://friendly.github.io/vcdExtra/reference/as_table.md)
before converting to case form.

## Author

Gavin M. Klorfine

## Examples

``` r
library(vcdExtra)

data("HairEyeColor")

freqForm <- as.data.frame(HairEyeColor) # Generate frequency form data
tidy_freqForm <- dplyr::as_tibble(HairEyeColor) # Generate tidy frequency form data
tableForm <- as_table(HairEyeColor) # Generate table form data
arrayDat <- as_array(HairEyeColor) # Generate an array

# Frequency form -> case form
as_caseform(freqForm) |> str()
#> tibble [592 × 3] (S3: tbl_df/tbl/data.frame)
#>  $ Hair: chr [1:592] "Black" "Black" "Black" "Black" ...
#>  $ Eye : chr [1:592] "Brown" "Brown" "Brown" "Brown" ...
#>  $ Sex : chr [1:592] "Male" "Male" "Male" "Male" ...

# Frequency form (tibble) -> case form
as_caseform(tidy_freqForm, freq = "n") |> str()
#> tibble [592 × 3] (S3: tbl_df/tbl/data.frame)
#>  $ Hair: chr [1:592] "Black" "Black" "Black" "Black" ...
#>  $ Eye : chr [1:592] "Blue" "Blue" "Blue" "Blue" ...
#>  $ Sex : chr [1:592] "Female" "Female" "Female" "Female" ...

# Array -> case form
as_caseform(arrayDat) |> str()
#> tibble [592 × 3] (S3: tbl_df/tbl/data.frame)
#>  $ Hair: chr [1:592] "Black" "Black" "Black" "Black" ...
#>  $ Eye : chr [1:592] "Brown" "Brown" "Brown" "Brown" ...
#>  $ Sex : chr [1:592] "Male" "Male" "Male" "Male" ...

# Optionally specify dims
as_caseform(tableForm, dims = c("Hair", "Eye")) |> str()
#> tibble [592 × 2] (S3: tbl_df/tbl/data.frame)
#>  $ Hair: chr [1:592] "Black" "Black" "Black" "Black" ...
#>  $ Eye : chr [1:592] "Brown" "Brown" "Brown" "Brown" ...

```
