# Association graphs for loglinear models


Association graphs for loglinear models represent variables as nodes and their partial associations
between pairs of variables as edges.
If two variables are not connected by an edge, they are conditionally independent given the other variables in the model.
How can we use this in practice, to understand a model, or how well it fits a given dataset?

## Ideas:

* use this as a visual representation of a loglinear model in a network diagram of nodes and edges representing associations
allowed (fitted) in the model. Allow nodes (variables) to be in different groups using different node shape / color, ...
reflecting different roles in models.

This representation only allows to represent pairwise associations in a model, but this is similar to what is done in
multiple correspondence analysis.

* use this to represent the actual associations fitted in a given loglinear model -- use edge width to represent the
strength of the pairwise associations. This could be based on the decrease in G^2 when a term is dropped or
a measure of pairwise association, e.g., Cramer's V. In this, it would be good to print the value of the
association value on the edge.



Read the lecture slides, C:/Dropbox/Documents/psy6136/lectures/04-Loglin.pdf, slides 41-47

There is also a large CRAN Task View on Graphical Models: https://cran.r-project.org/web/views/GraphicalModels.html

## What and How to Do

I want to develop this idea into R functions for representing loglinear models as graph structures.
There is a collection of old packages: gRbase, gRim, ... that implement these ideas.
See: https://people.math.aau.dk/~sorenh/software/gR/
However, they rely on old graphics packages (`graph`, `Rgraphiviz`), no longer available or very hard to
work with.

It's not clear whether these should be in a new package, or be part of {vcdExtra}, but let's create a plan for this,
perhaps starting quite simply, but with a view to how this could be further developed.

### Package / dependency choices

* **igraph**: Use for graph construction and layout. Well-established, lightweight, already widely used.
  The margin lists from `joint()`, `conditional()`, etc. and the `$margin` component of `loglm` objects
  can be converted to igraph objects with a simple `combn()`-based helper. This avoids depending on gRbase.

* **igraph** also has good `plot()` support for basic structural graphs (node colors, shapes, edge width/labels).
  This may be sufficient for both levels of visualization described below, at least initially.

* **qgraph**: A strong candidate for rendering *weighted* association networks, with built-in support for
  edge width proportional to weight, edge labels, positive/negative coloring, and spring layouts.
  However, it adds a dependency. Could be added to `Suggests:` for an enhanced plotting method.

* **DiagrammeR**: More verbose and general-purpose (flowcharts, process diagrams). Not well suited here.

**Decision**: Start with `igraph` only (add to `Imports:`). Consider `qgraph` later for a richer weighted-graph display.


---

## Implementation plan

### Phase 1: Graph construction -- `assoc_graph()`

A function to convert a loglinear model specification into an igraph undirected graph.

**File**: `R/assoc_graph.R`

#### Input sources

The function should accept several forms of input:

1. A **margin list** -- e.g., `list(c("A","B"), c("B","C"))`, as produced by `joint()`, `conditional()`, etc.
   and stored in `loglm` objects as `$margin`.
2. A **fitted `loglm` object** -- extract `$margin` from it.
3. A **fitted `glm` object** (poisson family) -- extract pairwise terms from the model formula.
4. A **model formula** -- e.g., `~ A*B + B*C`, parse the terms to find the generating class.

A simple S3 generic with methods might be cleanest:

```r
assoc_graph(x, ...)
assoc_graph.list(x, ...)       # margin list
assoc_graph.loglm(x, ...)     # loglm object
assoc_graph.glm(x, ...)       # glm object
```

#### Core logic

Each margin term (a character vector of variable names) is a *clique*: expand it into all
pairwise edges via `combn(term, 2)`. Union of all edges forms the graph. Single-variable
terms (from mutual independence) yield isolated nodes.

```r
# Core helper:
.margins_to_graph <- function(margins) {
  # all variable names (including isolated ones)
  all_vars <- unique(unlist(margins))

  # pairwise edges from each clique
  edge_list <- do.call(rbind, lapply(margins, function(m) {
    if (length(m) >= 2) t(combn(m, 2)) else NULL
  }))

  if (is.null(edge_list) || nrow(edge_list) == 0) {
    # No edges: mutual independence
    g <- igraph::make_empty_graph(n = 0, directed = FALSE)
    g <- igraph::add_vertices(g, length(all_vars), name = all_vars)
  } else {
    edge_list <- unique(edge_list)
    g <- igraph::graph_from_edgelist(edge_list, directed = FALSE)
    # Add any isolated nodes not covered by edges
    missing <- setdiff(all_vars, igraph::V(g)$name)
    if (length(missing) > 0) g <- igraph::add_vertices(g, length(missing), name = missing)
  }
  g
}
```

#### Return value

An igraph object with:
- vertex names = variable names from the table
- Optional vertex attributes: `group` (for coloring response vs. explanatory variables)
- Optional edge attributes: `weight` and `label` (for Phase 2)

#### Print method

Consider also a better print method for `igraph` objects. The default one is very compact, not not very readable.
But there is also a result="matrix" argument.

```
> library(gRbase)
> uG1 <- ug(~ a:b:c + c:d)
> uG1
IGRAPH 3d85909 UN-- 4 4 -- 
+ attr: name (v/c)
+ edges from 3d85909 (vertex names):
[1] a--b a--c b--c c--d

> ug(~a:b:c + c:d, result="matrix") 
  a b c d
a 0 1 1 0
b 1 0 1 0
c 1 1 0 1
d 0 0 1 0
```


### Phase 2: Edge weights -- association strength

Add an optional argument to compute pairwise association measures and store them as edge weights.

#### Measures (options for a `measure` argument):

* **`"chisq"`**: Partial chi-squared -- the decrease in G^2 when the corresponding two-way term is
  dropped from the model. Requires a fitted model object and the data table.

* **`"cramer"`**: Cramer's V for each pair, computed from the marginal 2-way table
  (not conditional on other variables, but simple and interpretable).

* **`"none"`** (default): Structural graph only, no weights.

For `"chisq"`, the approach:
- For each edge (pair of variables) in the model, fit the model without that term
  and compute the change in G^2. This is analogous to Type III tests.
- `drop1()` or manual refitting via `update()`.

Store results as `E(g)$weight` and `E(g)$label` (formatted value).


### Phase 3: Plotting -- `plot.assoc_graph()`

A plot method (or just pass igraph object to `plot()` / `qgraph()`). Key visual features:

#### Structural graph (no weights):
- Nodes: circles with variable names, colored by `group` if specified
- Edges: solid lines connecting associated pairs
- Missing edges: visually represent conditional independence
- Layout: `igraph::layout_in_circle()` for small graphs, `layout_with_fr()` for larger ones

#### Weighted graph:
- Edge width proportional to association strength
- Edge labels showing the numeric value
- Optionally, edge color intensity proportional to strength

#### Node grouping:
Allow a `groups` argument (named list or factor) to color nodes by role, e.g.,
response vs. explanatory, or substantive groupings. This maps to vertex `color` attributes.

#### Using igraph directly:

```r
plot.assoc_graph <- function(x, layout = NULL,
                             edge.width = NULL,
                             edge.label = NULL,
                             groups = NULL, ...) {
  # Set defaults
  if (is.null(layout)) {
    layout <- if (igraph::vcount(x) <= 6) igraph::layout_in_circle
              else igraph::layout_with_fr
  }
  # Map groups to colors
  if (!is.null(groups)) {
    igraph::V(x)$color <- groups_to_colors(x, groups)
  }
  # Use edge weights for width if available and not overridden
  if (is.null(edge.width) && !is.null(igraph::E(x)$weight)) {
    edge.width <- scale_weights(igraph::E(x)$weight)
  }
  igraph::plot.igraph(x, layout = layout,
                      edge.width = edge.width,
                      edge.label = edge.label, ...)
}
```

#### Optional qgraph rendering:

If qgraph is available, offer an alternative via `assoc_graph_qgraph()` or a `renderer = "qgraph"`
argument. This would convert the igraph adjacency matrix (with weights) to a qgraph call,
getting automatic edge width scaling, positive/negative coloring, and spring layout.


### Phase 4: Comparing models visually

Given a `loglmlist` (e.g., from `seq_loglm()`), show a panel of association graphs to visualize
how the model structure changes across a sequence. This connects to the existing `seq_mosaic()` idea.

```r
# Sketch:
assoc_graphs <- function(x, ...) {
  # x: loglmlist
  op <- par(mfrow = layout_for_n(length(x)))
  for (i in seq_along(x)) {
    g <- assoc_graph(x[[i]])
    plot(g, main = get_model(x[[i]]), ...)
  }
  par(op)
}
```


---

## Dependencies

* `igraph`: Add to `Imports:`. Core graph construction and basic plotting.
* `qgraph`: Optionally add to `Suggests:`. Enhanced weighted graph rendering.


## Files

* `R/assoc_graph.R` -- `assoc_graph()` generic + methods, `.margins_to_graph()` helper, `plot.assoc_graph()`
* `man/assoc_graph.Rd` -- documentation (via roxygen)
* `dev/assoc-graph-examples.R` -- development test script


## Examples to develop with

```r
library(vcdExtra)
data(Titanic)

# 1. Structural graphs from margin lists (3-way: A, B, C)
mutual(3, factors = c("A", "B", "C"))    |> assoc_graph() |> plot()  # no edges
joint(3, factors = c("A", "B", "C"))     |> assoc_graph() |> plot()  # A--B edge only
conditional(3, factors = c("A","B","C")) |> assoc_graph() |> plot()  # A--C, B--C
saturated(3, factors = c("A", "B", "C")) |> assoc_graph() |> plot()  # complete graph

# 2. From a fitted loglm model
mod <- MASS::loglm(~ (Admit + Gender) * Dept, data = UCBAdmissions)
assoc_graph(mod) |> plot()   # A--D, G--D edges (slide 47 example)

# 3. Sequential models (Titanic)
tit.joint <- seq_loglm(Titanic, type = "joint")
assoc_graphs(tit.joint)  # panel of 4 graphs showing growing structure

# 4. Weighted graph
g <- assoc_graph(mod, measure = "chisq")
plot(g)  # edge widths reflect partial chi-squared contributions
```
