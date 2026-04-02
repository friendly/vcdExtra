# Collapse the levels of a dataset

Collapses the levels of a dataset (of any form) into those specified.
May also be used to re-name levels. Ensure argument `freq` is supplied
should your data be in frequency form (and the frequency column differs
in name from default, "Freq").

## Usage

``` r
collapse_levels(x, freq = "Freq", ...)
```

## Arguments

- x:

  The dataset to be collapsed.

- freq:

  Supply only if your data is in frequency form AND your frequency
  column differs in name from the default ("Freq").

- ...:

  A collection of one or more assignments of dataset variables to a list
  of levels in the format
  `new_level = c("old_level_1", "old_level_2", ..., "old_level_n")`.

## Value

The collapsed dataset in its original form (i.e., the initial form of
`x`).

## Details

First converts the object `x` into a frequency form data frame. Then,
[`fct_collapse`](https://forcats.tidyverse.org/reference/fct_collapse.html)
is used to collapse variable levels. Next, duplicate rows (an artefact
of collapsing) are aggregated via
[`summarise`](https://dplyr.tidyverse.org/reference/summarise.html).
Last, the frequency form data frame is converted back into the initial
form of object `x`.

The exceptions to this are objects in case form, which are passed
directly to
[`fct_collapse`](https://forcats.tidyverse.org/reference/fct_collapse.html)
(and duplicate rows are not aggregated).

## See also

[`fct_collapse`](https://forcats.tidyverse.org/reference/fct_collapse.html),
[`collapse.table`](https://friendly.github.io/vcdExtra/reference/collapse.table.md)

Tidy conversion functions: `link{as_table}`, `link{as_freqform}`,
`link{as_caseform}`, `link{as_matrix}`, `link{as_array}`,

## Author

Gavin M. Klorfine

## Examples

``` r
data("HairEyeColor") # Table form data
str(HairEyeColor)
#>  'table' num [1:4, 1:4, 1:2] 32 53 10 3 11 50 10 30 10 25 ...
#>  - attr(*, "dimnames")=List of 3
#>   ..$ Hair: chr [1:4] "Black" "Brown" "Red" "Blond"
#>   ..$ Eye : chr [1:4] "Brown" "Blue" "Hazel" "Green"
#>   ..$ Sex : chr [1:2] "Male" "Female"

collapse_levels(
  HairEyeColor,                 # Dataset
  Hair = list(                  # List of arguments for first variable
    Dark = c("Black", "Brown"), # Collapse "Black" and "Brown" -> "Dark"
    Light = c("Blond", "Red")   # Collapse "Blond" and "Red" -> "Light"
  ),
  Eye = list(                   # List of arguments for second variable
    Common = c("Brown"),        # Collapse (rename) "Brown" -> "Common"
    Uncommon = c("Blue", "Green", "Hazel")
  )
) |> str()
#>  'xtabs' num [1:2, 1:2, 1:2] 85 13 114 67 102 20 93 98
#>  - attr(*, "dimnames")=List of 3
#>   ..$ Hair: chr [1:2] "Dark" "Light"
#>   ..$ Eye : chr [1:2] "Common" "Uncommon"
#>   ..$ Sex : chr [1:2] "Male" "Female"
#>  - attr(*, "call")= language xtabs(formula = reformulate(cols, response = freq), data = obj)

# To illustrate `freq` argument usage, convert Hoyt dataset to frequency form 
# (ff) and then rename frequency column to "n"

data("Hoyt", package = "vcdExtra")
ff_Hoyt <- as_freqform(Hoyt)
names(ff_Hoyt)[length(ff_Hoyt)] <- "n"
str(ff_Hoyt)
#> tibble [168 × 5] (S3: tbl_df/tbl/data.frame)
#>  $ Status    : Factor w/ 4 levels "College","School",..: 1 2 3 4 1 2 3 4 1 2 ...
#>  $ Rank      : Factor w/ 3 levels "Low","Middle",..: 1 1 1 1 2 2 2 2 3 3 ...
#>  $ Occupation: Factor w/ 7 levels "1","2","3","4",..: 1 1 1 1 1 1 1 1 1 1 ...
#>  $ Sex       : Factor w/ 2 levels "Male","Female": 1 1 1 1 1 1 1 1 1 1 ...
#>  $ n         : num [1:168] 87 3 17 105 216 4 14 118 256 2 ...

collapse_levels(
  ff_Hoyt,
  
  # Ensure to supply if data is in frequency form and frequency column name
  # differs from "Freq"
  freq = "n",
  
  Occupation = list(
    High = c(1, 2),
    Middle = 3,
    Low = 4,
    VeryLow = c(5, 6, 7)
  )
) |> str()
#> tibble [96 × 5] (S3: tbl_df/tbl/data.frame)
#>  $ Status    : Factor w/ 4 levels "College","School",..: 1 2 3 4 1 2 3 4 1 2 ...
#>  $ Rank      : Factor w/ 3 levels "Low","Middle",..: 1 1 1 1 2 2 2 2 3 3 ...
#>  $ Occupation: Factor w/ 4 levels "High","Middle",..: 1 1 1 1 1 1 1 1 1 1 ...
#>  $ Sex       : Factor w/ 2 levels "Male","Female": 1 1 1 1 1 1 1 1 1 1 ...
#>  $ Freq      : num [1:96] 159 9 35 314 375 18 42 345 432 10 ...

```
