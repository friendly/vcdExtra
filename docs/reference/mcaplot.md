# Simple and enhanced plot of MCA solutions

This function is intended as an alternative to
[`plot.mjca`](https://rdrr.io/pkg/ca/man/plot.mjca.html) for plotting
multiple correspondence analysis solutions. It provides more flexibility
for labeling factor levels and connecting them with lines. It does not
support some features of `plot.mjca` (centroids, supplementary points,
arrows, etc.)

## Usage

``` r
mcaplot(
  obj,
  map = "symmetric",
  dim = 1:2,
  col = c("blue", "red", "brown", "black", "green3", "purple"),
  pch = 15:20,
  cex = 1.2,
  pos = 3,
  lines = TRUE,
  lwd = 2,
  legend = FALSE,
  legend.pos = "topright",
  xlab = "_auto_",
  ylab = "_auto_",
  rev.axes = c(FALSE, FALSE),
  ...
)
```

## Arguments

- obj:

  An `"mjca"` object

- map:

  Character string specifying the map type, i.e., the scaling applied to
  coordinates for different types of MCA representations. Allowed
  options include: `"symmetric"` (default), `"rowprincipal"`,
  `"colprincipal"`, `"symbiplot"`, `"rowgab"`, `"colgab"`, `"rowgreen"`,
  `"colgreen"`. See [`mjca`](https://rdrr.io/pkg/ca/man/mjca.html) for
  details.

- dim:

  Dimensions to plot, an integer vector of length 2

- col:

  Vector of colors, one for each factor in the MCA

- pch:

  Vector of point symbols for the category levels, one for each factor

- cex:

  Character size for points and level labels

- pos:

  Position of level labels relative to the category points; either a
  single number or a vector of length equal to the number of category
  points.

- lines:

  A logical or an integer vector indicating which factors are to be
  joined with lines using
  [`multilines`](https://rdrr.io/pkg/ca/man/multilines.html)

- lwd:

  Line width(s) for the lines

- legend:

  Logical; draw a legend for the factor names?

- legend.pos:

  Position of the legend in the plot, as in
  [`legend`](https://rdrr.io/r/graphics/legend.html)

- xlab, ylab:

  Labels for horizontal and vertical axes. The default, `"_auto_"` means
  that the function auto-generates a label of the form
  `"Dimension X (xx.x \%)"`

- rev.axes:

  A logical vector of length 2, where TRUE reverses the direction of the
  corresponding axis

- ...:

  Arguments passed down to `plot`

## Value

Returns the coordinates of the category points invisibly

## See also

[`mjca`](https://rdrr.io/pkg/ca/man/mjca.html),
[`plot.mjca`](https://rdrr.io/pkg/ca/man/plot.mjca.html)
[`cacoord`](https://rdrr.io/pkg/ca/man/cacoord.html) returns CA and MCA
coordinates, [`multilines`](https://rdrr.io/pkg/ca/man/multilines.html)
draw multiple lines according to a factor,

## Author

Michael Friendly

## Examples

``` r
require(ca)
data(Titanic)
titanic.mca <- mjca(Titanic)
#> Warning: 'as.is' should be specified by the caller; using TRUE
#> Warning: 'as.is' should be specified by the caller; using TRUE
#> Warning: 'as.is' should be specified by the caller; using TRUE
#> Warning: 'as.is' should be specified by the caller; using TRUE
mcaplot(titanic.mca, legend=TRUE, legend.pos="topleft")


data(HairEyeColor)
haireye.mca <- mjca(HairEyeColor)
#> Warning: 'as.is' should be specified by the caller; using TRUE
#> Warning: 'as.is' should be specified by the caller; using TRUE
#> Warning: 'as.is' should be specified by the caller; using TRUE
mcaplot(haireye.mca, legend=TRUE, cex.lab=1.3)



```
