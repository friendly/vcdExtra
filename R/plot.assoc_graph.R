#' Plot an Association Graph
#'
#' Plot method for \code{\link{assoc_graph}} objects, displaying the association
#' structure of a loglinear model as a network diagram.
#'
#' @param x An \code{assoc_graph} object, as returned by \code{\link{assoc_graph}}.
#' @param layout Layout function or coordinate matrix for node positions.
#'   Defaults to \code{\link[igraph]{layout_in_circle}} for up to 6 nodes,
#'   \code{\link[igraph]{layout_with_fr}} otherwise.
#' @param groups Optional named list assigning variables to groups for coloring,
#'   e.g., \code{list(response = "Survived", predictors = c("Class", "Sex", "Age"))}.
#' @param colors Character vector of colors for groups. Recycled as needed.
#' @param vertex.size Vertex size (default 30).
#' @param vertex.label.cex Label size for vertex names (default 1.2).
#' @param edge.width Edge width (default 2). If edge weights are present, widths are
#'   scaled from the weights automatically.
#' @param edge.label Optional edge labels. If \code{TRUE} and edge weights are present,
#'   the weight values are used as labels.
#' @param \dots Additional arguments passed to \code{\link[igraph]{plot.igraph}},
#'   such as \code{main} for a title.
#'
#' @return The \code{assoc_graph} object \code{x}, returned invisibly.
#'
#' @seealso \code{\link{assoc_graph}}, \code{\link[igraph]{plot.igraph}}
#'
#' @family loglinear models
#' @export
#' @examples
#' # Basic structural plot
#' g <- conditional(3, factors = c("A", "B", "C")) |> assoc_graph()
#' plot(g, main = "Conditional independence: [AC] [BC]")
#'
#' # With grouped node colors
#' g <- saturated(4, factors = c("A", "B", "C", "D")) |> assoc_graph()
#' plot(g, groups = list(c("A", "B"), c("C", "D")),
#'      main = "Saturated model")
#'
plot.assoc_graph <- function(x, layout = NULL,
                             groups = NULL,
                             colors = c("lightblue", "lightyellow", "lightgreen",
                                        "lightsalmon", "plum"),
                             vertex.size = 30,
                             vertex.label.cex = 1.2,
                             edge.width = 2,
                             edge.label = NULL,
                             ...) {

  if (is.null(layout)) {
    layout <- if (igraph::vcount(x) <= 6) igraph::layout_in_circle(x)
              else igraph::layout_with_fr(x)
  }

  # Node colors from groups
  vcol <- rep(colors[1], igraph::vcount(x))
  if (!is.null(groups)) {
    vnames <- igraph::V(x)$name
    if (is.list(groups)) {
      # named list: list(response = "Survived", explanatory = c("Class", "Sex"))
      for (i in seq_along(groups)) {
        idx <- match(groups[[i]], vnames)
        idx <- idx[!is.na(idx)]
        vcol[idx] <- colors[((i - 1) %% length(colors)) + 1]
      }
    }
  }

  # Edge weights -> width scaling
  ew <- edge.width
  if (!is.null(igraph::E(x)$weight)) {
    w <- igraph::E(x)$weight
    # Scale to range [1, 6]
    if (max(w) > min(w)) {
      ew <- 1 + 5 * (w - min(w)) / (max(w) - min(w))
    } else {
      ew <- rep(3, length(w))
    }
  }

  # Edge labels from weights
  el <- edge.label
  if (isTRUE(el) && !is.null(igraph::E(x)$weight)) {
    el <- round(igraph::E(x)$weight, 2)
  }

  igraph::plot.igraph(x,
                      layout = layout,
                      vertex.size = vertex.size,
                      vertex.color = vcol,
                      vertex.label = igraph::V(x)$name,
                      vertex.label.cex = vertex.label.cex,
                      vertex.frame.color = "gray40",
                      edge.width = ew,
                      edge.label = el,
                      edge.color = "gray30",
                      ...)

  invisible(x)
}
