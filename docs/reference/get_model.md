# Extract Model Formula from a loglm or glm Object

`get_model()` extracts the model formula or bracket notation from a
single `loglm` or `glm` object. This is useful for labeling models in
summaries and plots. See
[`get_models`](https://friendly.github.io/vcdExtra/reference/glmlist.md)
for the corresponding function for `loglmlist` and `glmlist` objects.

## Usage

``` r
get_model(x, type = c("brackets", "formula"), abbrev = FALSE, ...)
```

## Arguments

- x:

  A `loglm` or `glm` object

- type:

  Type of output: `"brackets"` for loglinear bracket notation (e.g.,
  `"[AB] [C]"`), or `"formula"` for R formula notation. For `glm`
  objects, only `"formula"` is meaningful.

- abbrev:

  Logical or integer. If `TRUE` or a positive integer, abbreviate factor
  names to that many characters (default 1 when `TRUE`). Only applies to
  bracket notation.

- ...:

  Additional arguments passed to `loglin2string` such as `sep` and
  `collapse`.

## Value

A character string with the model formula or bracket notation.

## Details

For `loglm` objects created by
[`seq_loglm`](https://friendly.github.io/vcdExtra/reference/seq_loglm.md),
the bracket notation is stored in the `model.string` component. For
other `loglm` objects, it is constructed from the `margin` component
using
[`loglin2string`](https://friendly.github.io/vcdExtra/reference/loglin-utilities.md).

For `glm` objects, the formula is extracted using
[`formula()`](https://rdrr.io/r/stats/formula.html).

## See also

[`get_models`](https://friendly.github.io/vcdExtra/reference/glmlist.md)
for `glmlist` and `loglmlist` objects,
[`loglin2string`](https://friendly.github.io/vcdExtra/reference/loglin-utilities.md),
[`seq_mosaic`](https://friendly.github.io/vcdExtra/reference/seq_mosaic.md)

Other loglinear models:
[`glmlist()`](https://friendly.github.io/vcdExtra/reference/glmlist.md),
[`joint()`](https://friendly.github.io/vcdExtra/reference/loglin-utilities.md),
[`seq_loglm()`](https://friendly.github.io/vcdExtra/reference/seq_loglm.md)

## Examples

``` r
data(Titanic)
tit.joint <- seq_loglm(Titanic, type = "joint")
get_model(tit.joint[[2]])
#> [1] "(Class) (Sex)"
get_model(tit.joint[[4]])
#> [1] "[Class,Sex,Age] [Survived]"
get_model(tit.joint[[4]], abbrev = TRUE)
#> [1] "[C,S,A] [S]"
get_model(tit.joint[[4]], type = "formula")
#> [1] "~Class:Sex:Age + Survived"
```
