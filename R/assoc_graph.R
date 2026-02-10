#' Association Graph for a Loglinear Model
#'
#' Construct an undirected graph representing the associations in a loglinear model.
#' Nodes represent variables and edges represent pairwise associations fitted in the model.
#' If two variables are not connected by an edge, they are conditionally independent
#' given the other variables.
#'
#' @param x An object specifying the model. Can be:
#'   \itemize{
#'     \item A \code{list} of character vectors (a margin/generating class list, as produced by
#'           \code{\link{joint}}, \code{\link{conditional}}, etc.)
#'     \item A fitted \code{\link[MASS]{loglm}} object
#'     \item A fitted \code{\link[stats]{glm}} object (poisson family loglinear model)
#'   }
#' @param result Type of result to return: \code{"igraph"} (default) returns an
#'   \code{\link[igraph:igraph-package]{igraph}} object; \code{"matrix"} returns the
#'   adjacency matrix; \code{"edge_list"} returns a two-column character matrix of edges.
#' @param measure Type of association measure for edge weights (only for \code{glm} method):
#'   \code{"none"} (default) produces an unweighted graph;
#'   \code{"chisq"} computes partial chi-squared statistics (deviance change when each
#'   edge is removed from the model);
#'   \code{"cramer"} computes Cramer's V from the marginal two-way table for each edge.
#' @param \dots Additional arguments (currently unused).
#'
#' @return Depending on \code{result}:
#'   \itemize{
#'     \item \code{"igraph"}: An \code{igraph} undirected graph object of class
#'           \code{c("assoc_graph", "igraph")}, with vertex names corresponding to
#'           the variable names. When \code{measure != "none"}, edge weights are
#'           stored as \code{E(g)$weight} and the measure name as \code{g$measure}.
#'     \item \code{"matrix"}: A symmetric adjacency matrix with variable names as
#'           row and column names. Contains 0/1 when unweighted, or association
#'           strength values when \code{measure} is specified.
#'     \item \code{"edge_list"}: When unweighted, a two-column character matrix (from, to).
#'           When \code{measure} is specified, a data frame with columns from, to, and weight.
#'   }
#'
#' @details
#' Each high-order term (margin) in a hierarchical loglinear model defines a clique
#' in the association graph. For example, the term \code{c("A", "B", "C")} generates
#' edges A--B, A--C, and B--C. Single-variable terms (as in mutual independence)
#' yield isolated nodes with no edges.
#'
#' For \code{loglm} objects, the margins are extracted from the \code{$margin} component.
#' For \code{glm} objects, the interaction terms are extracted from the model formula.
#'
#' @references
#' Khamis, H. J. (2011). \emph{The Association Graph and the Multigraph for Loglinear Models}.
#' SAGE Publications. \doi{10.4135/9781452226521}
#'
#' Darroch, J. N., Lauritzen, S. L., & Speed, T. P. (1980). Markov Fields and Log-Linear
#' Interaction Models for Contingency Tables. \emph{The Annals of Statistics}, 8(3), 522--539.
#' \doi{10.1214/aos/1176345006}
#'
#' Whittaker, J. (1990). \emph{Graphical Models in Applied Multivariate Statistics}.
#' John Wiley & Sons, Chichester.
#'
#' @seealso \code{\link{joint}}, \code{\link{conditional}}, \code{\link{mutual}},
#'   \code{\link{saturated}}, \code{\link{loglin2string}}, \code{\link{seq_loglm}},
#'   \code{\link{plot.assoc_graph}}
#'
#' @family loglinear models
#' @export
#' @examples
#' # Structural graphs from margin lists (3-way: A, B, C)
#' mutual(3, factors = c("A", "B", "C"))      |> assoc_graph()
#' joint(3, factors = c("A", "B", "C"))       |> assoc_graph()
#' conditional(3, factors = c("A", "B", "C")) |> assoc_graph()
#' saturated(3, factors = c("A", "B", "C"))   |> assoc_graph()
#'
#' # Adjacency matrix form
#' conditional(3, factors = c("A", "B", "C")) |> assoc_graph(result = "matrix")
#'
#' # From a fitted loglm model (Berkeley admissions)
#' \dontrun{
#' mod <- MASS::loglm(~ (Admit + Gender) * Dept, data = UCBAdmissions)
#' assoc_graph(mod)
#' plot(assoc_graph(mod), main = "Berkeley: [AD] [GD]")
#' }
#'
#' # From glm models (Dayton Survey: cigarette, alcohol, marijuana, sex, race)
#' data(DaytonSurvey)
#'
#' # Mutual independence + sex*race: one edge only
#' mod.SR <- glm(Freq ~ . + sex*race, data = DaytonSurvey, family = poisson)
#' assoc_graph(mod.SR)
#' plot(assoc_graph(mod.SR), main = "Mutual indep. + [SR]")
#'
#' # [AM][AC][MC][AR][AS][RS]: {race, Sender} indep {marijuana, ciS} | alcohol
#' mod.cond <- glm(Freq ~ (cigarette + alcohol + marijuana)^2 +
#'                         (alcohol + sex + race)^2,
#'                 data = DaytonSurvey, family = poisson)
#' assoc_graph(mod.cond)
#' plot(assoc_graph(mod.cond),
#'      groups = list(c("cigarette", "alcohol", "marijuana"),
#'                    c("sex", "race")),
#'      main = "{R,S} indep {M,C} | A")
#'
#' # Weighted graph: partial chi-squared
#' g <- assoc_graph(mod.cond, measure = "chisq")
#' g
#' plot(g, edge.label = TRUE,
#'      groups = list(c("cigarette", "alcohol", "marijuana"),
#'                    c("sex", "race")),
#'      main = "Partial chi-squared weights")
#'
#' # Cramer's V (marginal)
#' g2 <- assoc_graph(mod.cond, measure = "cramer")
#' g2
#' plot(g2, edge.label = TRUE, main = "Cramer's V weights")
#'
assoc_graph <- function(x, ...) {
  UseMethod("assoc_graph")
}

#' @rdname assoc_graph
#' @export
assoc_graph.list <- function(x, result = c("igraph", "matrix", "edge_list"), ...) {
  result <- match.arg(result)
  .margins_to_assoc_graph(x, result = result)
}

#' @rdname assoc_graph
#' @export
assoc_graph.loglm <- function(x, result = c("igraph", "matrix", "edge_list"), ...) {
  result <- match.arg(result)
  if (is.null(x$margin)) {
    stop("Cannot extract margins from this loglm object")
  }
  .margins_to_assoc_graph(x$margin, result = result)
}

#' @rdname assoc_graph
#' @importFrom vcd assocstats
#' @export
assoc_graph.glm <- function(x, result = c("igraph", "matrix", "edge_list"),
                            measure = c("none", "chisq", "cramer"), ...) {
  result <- match.arg(result)
  measure <- match.arg(measure)
  margins <- .glm_to_margins(x)

  if (measure == "none") {
    return(.margins_to_assoc_graph(margins, result = result))
  }

  # Compute weights — need the igraph object regardless of result type

  g <- .margins_to_assoc_graph(margins, result = "igraph")

  weights <- switch(measure,
    chisq  = .compute_chisq_weights(g, x),
    cramer = .compute_cramer_weights(g, x)
  )

  g$measure <- measure

  if (igraph::ecount(g) > 0) {
    igraph::E(g)$weight <- weights
  }

  if (result == "igraph") {
    return(g)
  }

  if (result == "matrix") {
    all_vars <- igraph::V(g)$name
    nv <- length(all_vars)
    adj <- matrix(0, nv, nv, dimnames = list(all_vars, all_vars))
    if (igraph::ecount(g) > 0) {
      el <- igraph::as_edgelist(g)
      for (i in seq_len(nrow(el))) {
        adj[el[i, 1], el[i, 2]] <- weights[i]
        adj[el[i, 2], el[i, 1]] <- weights[i]
      }
    }
    return(adj)
  }

  if (result == "edge_list") {
    if (igraph::ecount(g) == 0) {
      return(matrix(character(0), ncol = 2, dimnames = list(NULL, c("from", "to"))))
    }
    el <- igraph::as_edgelist(g)
    out <- data.frame(from = el[, 1], to = el[, 2], weight = weights,
                      stringsAsFactors = FALSE)
    return(out)
  }
}


# --- Core helper: margin list -> assoc_graph ---

.margins_to_assoc_graph <- function(margins, result = "igraph") {

  # all variable names (including isolated ones from single-variable terms)
  all_vars <- unique(unlist(margins))

  # pairwise edges from each clique
  edge_list <- do.call(rbind, lapply(margins, function(m) {
    if (length(m) >= 2) t(utils::combn(m, 2)) else NULL
  }))

  if (!is.null(edge_list) && nrow(edge_list) > 0) {
    # deduplicate edges (sort each pair so A-B and B-A are treated the same)
    edge_list <- unique(edge_list)
  }

  if (result == "edge_list") {
    if (is.null(edge_list) || nrow(edge_list) == 0) {
      return(matrix(character(0), ncol = 2, dimnames = list(NULL, c("from", "to"))))
    }
    colnames(edge_list) <- c("from", "to")
    return(edge_list)
  }

  if (result == "matrix") {
    nv <- length(all_vars)
    adj <- matrix(0L, nv, nv, dimnames = list(all_vars, all_vars))
    if (!is.null(edge_list) && nrow(edge_list) > 0) {
      for (i in seq_len(nrow(edge_list))) {
        adj[edge_list[i, 1], edge_list[i, 2]] <- 1L
        adj[edge_list[i, 2], edge_list[i, 1]] <- 1L
      }
    }
    return(adj)
  }

  # result == "igraph"
  if (is.null(edge_list) || nrow(edge_list) == 0) {
    g <- igraph::make_empty_graph(n = 0, directed = FALSE)
    g <- igraph::add_vertices(g, length(all_vars), name = all_vars)
  } else {
    g <- igraph::graph_from_edgelist(edge_list, directed = FALSE)
    # add any isolated nodes not covered by edges
    missing <- setdiff(all_vars, igraph::V(g)$name)
    if (length(missing) > 0) {
      g <- igraph::add_vertices(g, length(missing), name = missing)
    }
  }

  class(g) <- c("assoc_graph", class(g))
  g
}


# --- Helper: margins -> glm formula with hierarchy (using * notation) ---

.margins_to_glm_formula <- function(margins, response) {
  rhs_terms <- vapply(margins, function(m) {
    if (length(m) == 1) m else paste(m, collapse = " * ")
  }, character(1))
  stats::as.formula(paste(response, "~", paste(rhs_terms, collapse = " + ")))
}


# --- Helper: partial chi-squared weights (deviance change per edge) ---

.compute_chisq_weights <- function(g, model) {
  el <- igraph::as_edgelist(g)
  if (nrow(el) == 0) return(numeric(0))

  dev_full <- stats::deviance(model)
  resp <- as.character(stats::formula(model)[[2]])

  weights <- numeric(nrow(el))
  for (i in seq_len(nrow(el))) {
    # Remove this edge
    eid <- igraph::get.edge.ids(g, c(el[i, 1], el[i, 2]))
    g_reduced <- igraph::delete_edges(g, eid)

    # Convert reduced graph -> margins -> glm formula
    margins_reduced <- .graph_to_margins(g_reduced)
    new_formula <- .margins_to_glm_formula(margins_reduced, resp)

    # Refit and compute deviance change
    mod_reduced <- stats::update(model, formula. = new_formula)
    weights[i] <- stats::deviance(mod_reduced) - dev_full
  }
  weights
}


# --- Helper: Cramer's V weights (marginal 2-way tables) ---

.compute_cramer_weights <- function(g, model) {
  el <- igraph::as_edgelist(g)
  if (nrow(el) == 0) return(numeric(0))

  mf <- stats::model.frame(model)
  freq <- stats::model.response(mf)

  weights <- numeric(nrow(el))
  for (i in seq_len(nrow(el))) {
    tab <- stats::xtabs(freq ~ mf[[el[i, 1]]] + mf[[el[i, 2]]])
    weights[i] <- vcd::assocstats(tab)$cramer
  }
  weights
}


# --- Helper: extract generating class (margins) from a glm formula ---

.glm_to_margins <- function(object) {
  tt <- stats::terms(object)
  factors <- attr(tt, "factors")
  order <- attr(tt, "order")

  if (is.null(factors)) {
    stop("Cannot extract model terms from this glm object")
  }

  # Get variable names involved in each term
  # Only keep the highest-order terms (generating class for hierarchical model)
  var_names <- rownames(factors)
  term_names <- colnames(factors)

  # Build list of variable sets for each term
  term_vars <- lapply(seq_along(term_names), function(j) {
    var_names[factors[, j] > 0]
  })

  # Filter to the generating class: remove terms that are subsets of other terms
  is_maximal <- vapply(seq_along(term_vars), function(i) {
    ti <- term_vars[[i]]
    !any(vapply(seq_along(term_vars), function(j) {
      if (i == j) return(FALSE)
      all(ti %in% term_vars[[j]]) && length(term_vars[[j]]) > length(ti)
    }, logical(1)))
  }, logical(1))

  margins <- term_vars[is_maximal]
  names(margins) <- paste0("term", seq_along(margins))
  margins
}


# --- Print method ---

#' @rdname assoc_graph
#' @export
print.assoc_graph <- function(x, ...) {
  nv <- igraph::vcount(x)
  ne <- igraph::ecount(x)
  vnames <- igraph::V(x)$name

  cat("Association graph: ", nv, " variables, ", ne, " edges\n", sep = "")
  cat("Variables:", paste(vnames, collapse = ", "), "\n")

  if (ne > 0) {
    el <- igraph::as_edgelist(x)
    w <- igraph::E(x)$weight
    if (!is.null(w)) {
      edge_strings <- paste0(el[, 1], " -- ", el[, 2], " (", round(w, 2), ")")
    } else {
      edge_strings <- paste0(el[, 1], " -- ", el[, 2])
    }
    cat("Edges:", paste(edge_strings, collapse = ", "), "\n")
  } else {
    cat("Edges: (none -- mutual independence)\n")
  }

  # Show measure if present
  if (!is.null(x$measure) && x$measure != "none") {
    cat("Measure:", x$measure, "\n")
  }

  # Show bracket notation
  margins <- .graph_to_margins(x)
  cat("Model:", loglin2string(margins), "\n")

  invisible(x)
}


# --- Helper: recover generating class from the graph (maximal cliques) ---

.graph_to_margins <- function(g) {
  if (igraph::ecount(g) == 0) {
    # Mutual independence: each variable is its own term
    margins <- as.list(igraph::V(g)$name)
  } else {
    # Maximal cliques give the generating class
    cliques <- igraph::max_cliques(g)
    margins <- lapply(cliques, function(cl) igraph::V(g)$name[cl])
  }
  names(margins) <- paste0("term", seq_along(margins))
  margins
}
