# Extract Model Formulas from Model Objects or Model Lists

`get_model()` extracts the model formula or bracket notation from a
single `loglm` or `glm` object. `get_models()` does the same for each
model in a `loglmlist` or `glmlist` object. These are useful for
labeling models in summaries and plots.

## Usage

``` r
get_model(x, type = c("brackets", "formula"), abbrev = FALSE, ...)

get_models(x, type = c("brackets", "formula"), abbrev = FALSE, ...)
```

## Arguments

- x:

  For `get_model()`: a `loglm` or `glm` object. For `get_models()`: a
  `glmlist` or `loglmlist` object.

- type:

  Type of output: `"brackets"` for loglinear bracket notation (e.g.,
  `"[AB] [C]"`), or `"formula"` for R formula notation. For `glm` and
  `glmlist` objects, only `"formula"` is meaningful.

- abbrev:

  Logical or integer. If `TRUE` or a positive integer, abbreviate factor
  names to that many characters (default 1 when `TRUE`). Only applies to
  bracket notation.

- ...:

  Additional arguments passed to `loglin2string` such as `sep` and
  `collapse`.

## Value

For `get_model()`: a character string with the model formula or bracket
notation. For `get_models()`: a named character vector with the model
formulas or bracket notations.

## Details

For `loglm` objects created by
[`seq_loglm`](https://friendly.github.io/vcdExtra/reference/seq_loglm.md),
the bracket notation is stored in the `model.string` component. For
other `loglm` objects, it is constructed from the `margin` component
using
[`loglin2string`](https://friendly.github.io/vcdExtra/reference/loglin-utilities.md).

For `glm` objects, the formula is extracted using
[`formula()`](https://rdrr.io/r/stats/formula.html).

**Model notation**

For `loglmlist` objects created by
[`seq_loglm`](https://friendly.github.io/vcdExtra/reference/seq_loglm.md),
the bracket notation distinguishes between models fit to marginal
sub-tables and models fit to the full table. Parentheses are used for
marginal sub-tables, e.g., `"(Class) (Sex)"`, while square brackets are
used for the full table, e.g., `"[Class,Sex,Age] [Survived]"`.

The `abbrev` argument can be used to abbreviate factor names for more
compact display, e.g., `"[C,S,A] [S]"`.

## See also

[`glmlist`](https://friendly.github.io/vcdExtra/reference/glmlist.md),
[`loglmlist`](https://friendly.github.io/vcdExtra/reference/glmlist.md),
[`loglin2string`](https://friendly.github.io/vcdExtra/reference/loglin-utilities.md),
[`LRstats`](https://friendly.github.io/vcdExtra/reference/LRstats.md),
[`seq_mosaic`](https://friendly.github.io/vcdExtra/reference/seq_mosaic.md)

Other glmlist functions:
[`Kway()`](https://friendly.github.io/vcdExtra/reference/Kway.md),
[`LRstats()`](https://friendly.github.io/vcdExtra/reference/LRstats.md),
[`glmlist()`](https://friendly.github.io/vcdExtra/reference/glmlist.md),
[`mosaic.glmlist()`](https://friendly.github.io/vcdExtra/reference/mosaic.glmlist.md)

Other loglinear models:
[`glmlist()`](https://friendly.github.io/vcdExtra/reference/glmlist.md),
[`joint()`](https://friendly.github.io/vcdExtra/reference/loglin-utilities.md),
[`seq_loglm()`](https://friendly.github.io/vcdExtra/reference/seq_loglm.md)

## Examples

``` r
data(Titanic)
tit.joint <- seq_loglm(Titanic, type = "joint")

# Single model
get_model(tit.joint[[2]])
#> [1] "(Class) (Sex)"
get_model(tit.joint[[4]])
#> [1] "[Class,Sex,Age] [Survived]"
get_model(tit.joint[[4]], abbrev = TRUE)
#> [1] "[C,S,A] [S]"
get_model(tit.joint[[4]], type = "formula")
#> [1] "~Class:Sex:Age + Survived"

# Model list
get_models(tit.joint)
#>                      joint.1                      joint.2 
#>                    "= Class"              "(Class) (Sex)" 
#>                      joint.3                      joint.4 
#>          "(Class,Sex) (Age)" "[Class,Sex,Age] [Survived]" 
get_models(tit.joint, type = "formula")
#>                     joint.1                     joint.2 
#>                    "~Class"              "~Class + Sex" 
#>                     joint.3                     joint.4 
#>          "~Class:Sex + Age" "~Class:Sex:Age + Survived" 

# With abbreviated factor names
get_models(tit.joint, abbrev = TRUE)
#>       joint.1       joint.2       joint.3       joint.4 
#>     "= Class"     "(C) (S)"   "(C,S) (A)" "[C,S,A] [S]" 
get_models(tit.joint, abbrev = 2)
#>           joint.1           joint.2           joint.3           joint.4 
#>         "= Class"       "(Cl) (Sx)"    "(Cl,Sx) (Ag)" "[Cl,Sx,Ag] [Sr]" 
```
