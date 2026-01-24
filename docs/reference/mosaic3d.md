# 3D Mosaic Plots

Produces a 3D mosaic plot for a contingency table (or a
`link[MASS]{loglm}` model) using the
[`rgl-package`](https://dmurdoch.github.io/rgl/dev/reference/rgl-package.html).

## Usage

``` r
mosaic3d(x, ...)

# S3 method for class 'loglm'
mosaic3d(
  x,
  type = c("observed", "expected"),
  residuals_type = c("pearson", "deviance"),
  ...
)

# Default S3 method
mosaic3d(
  x,
  expected = NULL,
  residuals = NULL,
  type = c("observed", "expected"),
  residuals_type = NULL,
  shape = rgl::cube3d(alpha = alpha),
  alpha = 0.5,
  spacing = 0.1,
  split_dir = 1:3,
  shading = shading_basic,
  interpolate = c(2, 4),
  zero_size = 0.05,
  label_edge,
  labeling_args = list(),
  newpage = TRUE,
  box = FALSE,
  ...
)
```

## Arguments

- x:

  A `link[MASS]{loglm}` model object. Alternatively, a multidimensional
  `array` or `table`
  or[`structable`](https://rdrr.io/pkg/vcd/man/structable.html) of
  frequencies in a contingency table. In the present implementation, the
  dimensions are taken in sequential order. Use `link[base]{aperm}` or
  [`structable`](https://rdrr.io/pkg/vcd/man/structable.html) to change
  this.

- ...:

  Other arguments passed down to `mosaic.default` or 3D functions.

- type:

  a character string indicating whether the `"observed"` or the
  `"expected"` frequencies in the table should be visualized by the
  volume of the 3D tiles.

- residuals_type:

  a character string indicating the type of residuals to be computed
  when none are supplied. If residuals is `NULL`, `residuals_type` must
  be one of `"pearson"` (default; giving components of Pearson's
  chi-squared), `"deviance"` (giving components of the likelihood ratio
  chi-squared), or `"FT"` for the Freeman-Tukey residuals. The value of
  this argument can be abbreviated.

- expected:

  optionally, for contingency tables, an array of expected frequencies
  of the same dimension as `x`, or alternatively the corresponding
  loglinear model specification as used by`link[stats]{loglin}` or
  `link[MASS]{loglm}` (see
  [`structable`](https://rdrr.io/pkg/vcd/man/structable.html) for
  details).

- residuals:

  optionally, an array of residuals of the same dimension as `x` (see
  details).

- shape:

  The initial 3D shape on which the mosaic is based. Typically this is a
  call to an rgl function, and must produce a `shape3d` object. The
  default is a "unit cube" on (-1, +1), with transparency specified by
  `alpha`.

- alpha:

  Specifies the transparency of the 3D tiles used to compose the 3D
  mosaic.

- spacing:

  A number or vector giving the total amount of space used to separate
  the 3D tiles along each of the dimensions of the table. The values
  specified are re-cycled to the number of table dimensions.

- split_dir:

  A numeric vector composed of the integers `1:3` or a character vector
  composed of `c("x", "y", "z")`, where `split_dir[i]` specifies the
  axis along which the tiles should be split for dimension `i` of the
  table. The values specified are re-cycled to the number of table
  dimensions.

- shading:

  A function, taking an array or vector of residuals for the given
  model, returning a vector of colors. At present, only the default
  `shading=shading_basic` is provided. This is roughly equivalent to the
  use of the `shade` argument in
  [`mosaicplot`](https://rdrr.io/r/graphics/mosaicplot.html) or to the
  use of `gp=shading_Friendly` in
  [`mosaic`](https://rdrr.io/pkg/vcd/man/mosaic.html).

- interpolate:

  a vector of interpolation values for the `shading` function.

- zero_size:

  The radius of a small sphere used to mark zero cells in the display.

- label_edge:

  A character vector composed of `c("-", "+")` indicating whether the
  labels for a given table dimension are to be written at the minima
  (`"-"`) or maxima (`"+"`) of the *other* dimensions in the plot. The
  default is `rep( c('-', '+'), each=3, length=ndim)`, meaning that the
  first three table variables are labeled at the minima, and successive
  ones at the maxima.

- labeling_args:

  This argument is intended to be used to specify details of the
  rendering of labels for the table dimensions, but at present has no
  effect.

- newpage:

  logical indicating whether a new page should be created for the plot
  or not.

- box:

  logical indicating whether a bounding box should be drawn around the
  plot.

## Value

Invisibly, the list of `shape3d` objects used to draw the 3D mosaic,
with names corresponding to the concatenation of the level labels,
separated by ":".

## Details

Generalizing the 2D mosaic plot, this begins with a given 3D shape (a
unit cube), and successively sub-divides it along the X, Y, Z dimensions
according to the table margins, generating a nested set of 3D tiles. The
volume of the resulting tiles is therefore proportional to the frequency
represented in the table cells. Residuals from a given loglinear model
are then used to color or shade each of the tiles.

This is a developing implementation. The arguments and details are
subject to change.

Friendly (1995), Friendly (2000, Sect. 4.5) and Theus and Lauer (1999)
have all used the idea of 3D mosaic displays to explain various aspects
of loglinear models (the iterative proportional fitting algorithm, the
structure of various models for 3-way and n-way tables, etc.), but no
implementation of 3D mosaics was previously available.

For the default method, residuals, used to color and shade the 3D tiles,
can be passed explicitly, or, more typically, are computed as needed
from observed and expected frequencies. In this case, the expected
frequencies are optionally computed for a specified loglinear model
given by the `expected` argument. For the loglm method, residuals and
observed frequencies are calculated from the model object.

## References

Friendly, M. (1995). Conceptual and Visual Models for Categorical Data,
*The American Statistician*, **49**, 153-160.

Friendly, M. *Visualizing Categorical Data*, Cary NC: SAS Institute,
2000. Web materials: <http://www.datavis.ca/books/vcd/>.

Theus, M. & Lauer, S. R. W. (1999) Visualizing Loglinear Models.
*Journal of Computational and Graphical Statistics*, **8**, 396-412.

## See also

[`strucplot`](https://rdrr.io/pkg/vcd/man/strucplot.html),
[`mosaic`](https://rdrr.io/pkg/vcd/man/mosaic.html),
[`mosaicplot`](https://rdrr.io/r/graphics/mosaicplot.html) for aspects
of mosaic plots.

[`loglin`](https://rdrr.io/r/stats/loglin.html),
[`loglm`](https://rdrr.io/pkg/MASS/man/loglm.html) for details on
fitting loglinear models

[`play3d`](https://dmurdoch.github.io/rgl/dev/reference/play3d.html),
[`movie3d`](https://dmurdoch.github.io/rgl/dev/reference/play3d.html)
for what you can do with a 3D mosaic plot

Other mosaic plots:
[`mosaic.glm()`](https://friendly.github.io/vcdExtra/reference/mosaic.glm.md),
[`mosaic.glmlist()`](https://friendly.github.io/vcdExtra/reference/mosaic.glmlist.md)

## Author

Michael Friendly, with the help of Duncan Murdoch and Achim Zeileis

## Examples

``` r
# 2 x 2 x 2
if (!rgl.useNULL() && interactive()){
mosaic3d(Bartlett, box=TRUE)
# compare with expected frequencies under model of mutual independence
mosaic3d(Bartlett, type="expected", box=TRUE)

# 2 x 2 x 3
mosaic3d(Heart, box=TRUE)
}
#> Error in rgl.useNULL(): could not find function "rgl.useNULL"

# Make a dynamic display
if (FALSE) { # \dontrun{
  mosaic3d(Heart, box=TRUE, alpha = 0.3, interpolate = c(1, 2, 4))
  play3d(spin3d(axis = c(0, 1, 0), rpm = 10), duration = 5)
} # }

if (FALSE) { # \dontrun{
# 2 x 2 x 2 x 3
# illustrates a 4D table
mosaic3d(Detergent)

# compare 2D and 3D mosaics
#demo("mosaic-hec")
} # }

```
