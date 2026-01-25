# Subdivide a 3D Object

Subdivides a `shape3d` object or a list of `shape3d` objects into
objects of the same shape along a given dimension according to the
proportions or frequencies specified in vector(s).

## Usage

``` r
split3d(obj, ...)

# S3 method for class 'shape3d'
split3d(obj, p, dim, space = 0.1, ...)

# S3 method for class 'list'
split3d(obj, p, dim, space = 0.1, ...)

range3d(obj)

center3d(obj)
```

## Arguments

- obj:

  A `shape3d` object, or a list composed of them

- ...:

  Other arguments for split3d methods

- p:

  For a single `shade3d` object, a vector of proportions (or a vector of
  non-negative numbers which will be normed to proportions) indicating
  the number of subdivisions and their scaling along dimension `dim`.
  For a list of `shade3d` objects, a matrix whose columns indicate the
  subdivisions of each object.

- dim:

  The dimension along which the object is to be subdivided. Either an
  integer: 1, 2, or 3, or a character: "x", "y", or "z".

- space:

  The total space used to separate the copies of the object along
  dimension `dim`. The unit inter-object space is therefore
  `space/(length(p)-1)`.

## Value

`split3d` returns a list of `shape3d` objects.

`range3d` returns a 2 x 3 matrix, whose first row contains the minima on
dimensions x, y, z, and whose second row contains the maxima.

`center3d` returns a numeric vector containing the means of the minima
and maxima on dimensions x, y, z.

## Details

`split3d` is the basic workhorse used in
[`mosaic3d`](https://friendly.github.io/vcdExtra/reference/mosaic3d.md),
but may be useful in other contexts.

`range3d` and `center3d` are utility functions, also useful in other
contexts.

The resulting list of `shape3d` objects is actually composed of *copies*
of the input object(s), scaled according to the proportions in `p` and
then translated to make their range along the splitting dimension equal
to that of the input object(s).

## See also

[`mosaic3d`](https://friendly.github.io/vcdExtra/reference/mosaic3d.md)

[`shapelist3d`](https://dmurdoch.github.io/rgl/dev/reference/shapelist3d.html)
for the plotting of lists of `shape3d` objects.

## Author

Duncan Murdoch, with refinements by Michael Friendly

## Examples

``` r
if (!rgl::rgl.useNULL() && interactive()){
  open3d()
  cube <- cube3d(alpha=0.4)
  sl1 <- split3d(cube, c(.2, .3, .5), 1)
  col <- c("#FF000080", "#E5E5E580", "#0000FF80")
  shapelist3d(sl1, col=col)

  open3d()
  p <- matrix(c(.6, .4, .5, .5, .2, .8), nrow=2)
  sl2 <- split3d(sl1, p, 2)
  shapelist3d(sl2, col=col)
  }
```
