# Association Graph Rendering: Phase 3 Notes

## Current state

`plot.assoc_graph()` in `R/plot.assoc_graph.R` uses `igraph::plot.igraph()` for all rendering.
It handles:

- Layout selection (circle for <= 6 nodes, Fruchterman-Reingold otherwise)
- Node coloring via `groups` argument
- Edge width scaling from weights (range [1, 6])
- Edge labels from weights when `edge.label = TRUE`

## Problems with igraph rendering

### 1. Edge labels sit directly on the line

igraph places edge labels at the midpoint of the edge, drawn directly on top of it.
There is **no `edge.label.dist`** parameter (that only exists for vertex labels).
The only built-in controls are:

| Parameter            | What it does                                      |
|----------------------|---------------------------------------------------|
| `edge.label`         | Label text (or `NA` to suppress)                  |
| `edge.label.cex`     | Font size multiplier                              |
| `edge.label.color`   | Text color                                        |
| `edge.label.family`  | Font family (`"sans"`, `"serif"`, `"mono"`)       |
| `edge.label.font`    | Style: 1=plain, 2=bold, 3=italic                  |
| `edge.label.x`       | Explicit x-coordinates (overrides auto-placement) |
| `edge.label.y`       | Explicit y-coordinates (overrides auto-placement) |

None of these offset the label away from the edge line.

#### Workarounds within igraph

1. **`edge.curved = 0.15`**: A slight curve separates the label from the straight-line
   path between nodes, making it more readable. Easy to add as a default.

2. **Manual offset via `edge.label.x` / `edge.label.y`**: Compute the midpoint of each
   edge, then shift perpendicular to the edge direction. This requires extracting layout
   coordinates and doing geometry -- doable but fragile across layouts.

3. **Post-hoc rendering**: Plot without edge labels, then call `graphics::text()` with
   computed offset positions. Same geometry problem as (2).

4. **Smaller label font**: `edge.label.cex = 0.7` or `0.8` reduces the overlap area.
   Helps but doesn't solve the core problem.

### 2. Layout issues with weighted graphs

- `layout_in_circle()` ignores weights entirely -- edges cross and overlap.
- `layout_nicely()` does **not** forward weights to the underlying algorithm by default;
  it issues a warning and ignores them.
- `layout_with_fr()` (Fruchterman-Reingold) does use weights: larger weight = shorter edge
  (stronger attraction). This is the right behavior for association strength, but
  the resulting layout can look cramped for dense weighted graphs.
- `layout_with_kk()` (Kamada-Kawai) treats weights as distances (larger = farther apart),
  which is the **opposite** of what we want for association strength.

**Recommendation for igraph**: For weighted graphs, `layout_with_fr()` is the best built-in
option. Could also try `layout_with_graphopt()` which has tunable `spring.length` and
`spring.constant` parameters.


## Quick improvements to `plot.assoc_graph()` (igraph-only)

These changes could be made to the current function with minimal effort:

```r
# In the igraph::plot.igraph() call, add:
edge.label.cex = 0.8,          # slightly smaller labels
edge.label.color = "black",    # ensure readable color
edge.curved = 0.15,            # slight curve to separate label from line
```

Could also add `edge.label.cex` and `edge.curved` as function parameters with these
defaults, so users can override them.

For layout, detect when weights are present and prefer `layout_with_fr()` over
`layout_in_circle()` even for small graphs, since the force-directed layout
positions strongly-associated nodes closer together.


## qgraph as an alternative renderer

The `qgraph` package is purpose-built for rendering weighted network graphs and handles
many of the pain points with igraph's plotting.

### Key advantages over igraph for this use case

| Feature                  | igraph                             | qgraph                                     |
|--------------------------|------------------------------------|---------------------------------------------|
| Edge label background    | Not supported                      | `edge.label.bg = TRUE` (white box behind label) |
| Edge label positioning   | Only manual x/y overrides          | `edge.label.position` (0-1 along edge)      |
| Weight-to-width scaling  | Manual (we compute it ourselves)   | Automatic with `cut`, `maximum`, `minimum`   |
| Positive/negative color  | Manual                             | Automatic (green/blue vs red by default)     |
| Group coloring + legend  | Manual (we compute it ourselves)   | Built-in `groups` + `palette` with auto legend |
| Publication defaults     | Requires extensive tuning          | Good out of the box for weighted graphs      |

### Input format

qgraph does **not** accept igraph objects directly. The bridge is straightforward:

```r
adj <- igraph::as_adjacency_matrix(g, attr = "weight", sparse = FALSE)
qgraph::qgraph(adj, labels = igraph::V(g)$name, ...)
```

For unweighted graphs, use `attr = NULL` to get a 0/1 adjacency matrix.

### How it would work

A sketch for a qgraph rendering path:

```r
# Extract adjacency matrix
if (!is.null(igraph::E(x)$weight)) {
  adj <- igraph::as_adjacency_matrix(x, attr = "weight", sparse = FALSE)
} else {
  adj <- igraph::as_adjacency_matrix(x, sparse = FALSE)
}

qgraph::qgraph(adj,
  labels = igraph::V(x)$name,
  edge.labels = TRUE,
  edge.label.bg = TRUE,
  edge.label.cex = 0.8,
  groups = groups,             # same list format we already use
  palette = "colorblind",
  layout = "spring",           # or "circle", "groups"
  title = main
)
```

### Layout options in qgraph

| Layout value  | Description                                                |
|---------------|------------------------------------------------------------|
| `"circle"`    | Single circle (default for weighted without groups)        |
| `"groups"`    | Separate circle per group (default for weighted + groups)  |
| `"spring"`    | Fruchterman-Reingold force-directed                        |
| A matrix      | Custom n-by-2 coordinate matrix                           |
| igraph fn     | Any igraph layout function (e.g., `igraph::layout_with_kk`) |

### Groups and coloring

qgraph accepts groups in the same list format we already use:

```r
groups = list(c("cigarette", "marijuana"),
              "alcohol",
              c("sex", "race"))
```

It automatically assigns colors per group and draws a legend. Named list elements
become legend labels. The `palette` parameter controls the color scheme:
`"colorblind"`, `"pastel"`, `"rainbow"`, `"ggplot2"`, `"gray"`.


## Design options

### Option A: Improve igraph defaults only

- Add `edge.curved`, `edge.label.cex`, `edge.label.color` defaults to `plot.assoc_graph()`
- Document the layout considerations for weighted graphs
- Lowest effort; addresses the worst of the readability issues
- No new dependency

### Option B: Add qgraph as optional renderer

- Add `qgraph` to `Suggests:` in DESCRIPTION
- Add a `renderer = c("igraph", "qgraph")` parameter to `plot.assoc_graph()`
- When `renderer = "qgraph"`, convert the igraph object to an adjacency matrix and
  call `qgraph::qgraph()` with sensible defaults
- Check availability at runtime: `if (!requireNamespace("qgraph", quietly = TRUE)) stop(...)`
- Best of both worlds: igraph for structure, qgraph for presentation

### Option C: Separate `qgraph_assoc_graph()` function

- Instead of overloading `plot.assoc_graph()`, create a dedicated function
  `qgraph_assoc_graph(x, groups, ...)` that takes an `assoc_graph` object
  and renders via qgraph
- Cleaner separation of concerns; avoids a `renderer` switch in the plot method
- User calls `plot(g)` for quick igraph view, `qgraph_assoc_graph(g)` for publication quality


## Recommendation

**Option B** is probably the best balance. It keeps the familiar `plot()` interface,
lets users get improved rendering with a single argument change, and falls back to
igraph when qgraph is not installed. The implementation is small:

1. Add `qgraph` to `Suggests:` in DESCRIPTION
2. Add `renderer` parameter to `plot.assoc_graph()`
3. Write a small internal `.plot_qgraph()` helper for the conversion + call
4. Document both renderers with examples
