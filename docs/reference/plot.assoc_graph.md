# Plot an Association Graph

Plot method for
[`assoc_graph`](https://friendly.github.io/vcdExtra/reference/assoc_graph.md)
objects, displaying the association structure of a loglinear model as a
network diagram.

## Usage

``` r
# S3 method for class 'assoc_graph'
plot(
  x,
  layout = NULL,
  groups = NULL,
  colors = c("lightblue", "lightyellow", "lightgreen", "lightsalmon", "plum"),
  vertex.size = 30,
  vertex.label.cex = 1.2,
  edge.width = 2,
  edge.label = NULL,
  ...
)
```

## Arguments

- x:

  An `assoc_graph` object, as returned by
  [`assoc_graph`](https://friendly.github.io/vcdExtra/reference/assoc_graph.md).

- layout:

  Layout function or coordinate matrix for node positions. Defaults to
  [`layout_in_circle`](https://r.igraph.org/reference/layout_in_circle.html)
  for up to 6 nodes,
  [`layout_with_fr`](https://r.igraph.org/reference/layout_with_fr.html)
  otherwise.

- groups:

  Optional named list assigning variables to groups for coloring, e.g.,
  `list(response = "Survived", predictors = c("Class", "Sex", "Age"))`.

- colors:

  Character vector of colors for groups. Recycled as needed.

- vertex.size:

  Vertex size (default 30).

- vertex.label.cex:

  Label size for vertex names (default 1.2).

- edge.width:

  Edge width (default 2). If edge weights are present, widths are scaled
  from the weights automatically.

- edge.label:

  Optional edge labels. If `TRUE` and edge weights are present, the
  weight values are used as labels.

- ...:

  Additional arguments passed to
  [`plot.igraph`](https://r.igraph.org/reference/plot.igraph.html), such
  as `main` for a title.

## Value

The `assoc_graph` object `x`, returned invisibly.

## See also

[`assoc_graph`](https://friendly.github.io/vcdExtra/reference/assoc_graph.md),
[`plot.igraph`](https://r.igraph.org/reference/plot.igraph.html)

Other loglinear models:
[`assoc_graph()`](https://friendly.github.io/vcdExtra/reference/assoc_graph.md),
[`get_model()`](https://friendly.github.io/vcdExtra/reference/get_model.md),
[`glmlist()`](https://friendly.github.io/vcdExtra/reference/glmlist.md),
[`joint()`](https://friendly.github.io/vcdExtra/reference/loglin-utilities.md),
[`seq_loglm()`](https://friendly.github.io/vcdExtra/reference/seq_loglm.md)

## Examples

``` r
# Basic structural plot
g <- conditional(3, factors = c("A", "B", "C")) |> assoc_graph()
plot(g, main = "Conditional independence: [AC] [BC]")


# With grouped node colors
g <- saturated(4, factors = c("A", "B", "C", "D")) |> assoc_graph()
plot(g, groups = list(c("A", "B"), c("C", "D")),
     main = "Saturated model")

```
