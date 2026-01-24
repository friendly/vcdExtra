#' Labeling Function for Mosaic Displays Using Points
#'
#' This labeling function for use with \code{\link[vcd]{strucplot}} displays,
#' such as \code{\link[vcd]{mosaic}}, draws
#' random points within each cell of a mosaic plot, where the number of points
#' represents observed or expected frequencies. This creates a "dot-density"
#' visualization that provides a direct visual representation of cell frequencies.
#'
#' @param labels Logical vector or scalar indicating whether labels should be
#'        drawn for the table dimensions via \code{\link[vcd]{labeling_border}}.
#'        Defaults to \code{TRUE}.
#' @param varnames Logical vector or scalar indicating whether variable names
#'        should be drawn. Defaults to \code{labels}.
#' @param value_type Character string specifying whether to display
#'        \code{"observed"} or \code{"expected"} frequencies as points.
#' @param scale Numeric scaling factor. The number of points drawn equals
#'        \code{round(frequency / scale)}. Use larger values for tables with
#'        large counts.
#' @param pch Point character (plotting symbol). Default is 19 (filled circle).
#' @param size Point size as a \code{\link[grid]{unit}} object.
#'        Default is \code{unit(0.5, "char")}.
#' @param gp_points A \code{\link[grid]{gpar}} object controlling point appearance
#'        (color, alpha, etc.), for example: \code{gp_points = gpar(col = "red")}.
#' @param margin Margin inside cells as a \code{\link[grid]{unit}} object.
#'        Points are drawn within this inset area. Default is \code{unit(0.05, "npc")}.
#' @param seed Optional integer seed for reproducible point placement across several similar plots.
#'        If \code{NULL} (default), no seed is set.
#' @param jitter Numeric jitter amount (0-1) for point placement.
#'        Default is 1 (full random). Values < 1 create more regular patterns.
#' @param clip Logical indicating whether to clip points at cell boundaries.
#'        Default is \code{FALSE}.
#' @param ... Additional arguments passed to \code{\link[vcd]{labeling_border}}
#'        for axis labels.
#'
#' @details
#' This function follows the "grapcon_generator" pattern used by vcd labeling
#' functions. It returns a function that can be passed to the \code{labeling}
#' argument of \code{\link[vcd]{strucplot}}, \code{\link[vcd]{mosaic}}, or
#' related functions as the argument \code{labeling = labeling_poionts(...)}.
#'
#' The visualization is inspired by the conceptual model described in
#' Friendly (1995), where cell frequencies are represented as physical
#' counts of objects. Under independence, point density would be uniform
#' across cells (adjusted for marginals), so departures from independence
#' become visible as density variations.
#'
#' This approach is related to sieve diagrams (\code{\link[vcd]{sieve}}),
#' which also use density to represent association.
#'
#' @return A function of class \code{"grapcon_generator"} suitable for use
#'   as the \code{labeling} argument in \code{\link[vcd]{strucplot}} and
#'   related functions like \code{\link[vcd]{mosaict}}.
#'
#' @references
#' Friendly, M. (1995). Conceptual and Visual Models for Categorical Data.
#' \emph{The American Statistician}, \strong{49}, 153-160.
#' \doi{10.1080/00031305.1995.10476131}
#'
#' Friendly, M. (1997). Conceptual Models for Visualizing Contingency Table Data.
#' In M. Greenacre & J. Blasius (Eds.), \emph{Visualization of Categorical Data} (pp. 17–35).
#' Academic Press. \url{https://www.datavis.ca/papers/koln/kolnpapr.pdf}
#'
#' Meyer, D., Zeileis, A., and Hornik, K. (2006).
#' The Strucplot Framework: Visualizing Multi-way Contingency Tables with vcd.
#' \emph{Journal of Statistical Software}, \strong{17}(3), 1-48.
#' \doi{10.18637/jss.v017.i03}
#'
#' @seealso
#' \code{\link[vcd]{labeling_cells}}, \code{\link[vcd]{labeling_border}},
#' \code{\link[vcd]{mosaic}}, \code{\link[vcd]{sieve}}
#'
#' @examples
#' library(vcd)
#'
#' # 2-way table of Hair and Eye color
#' HairEye <- margin.table(HairEyeColor, 2:1)
#'
#' # Basic usage - observed frequencies as points
#' mosaic(HairEye,
#'        labeling = labeling_points(scale = 1))
#'
#' # Show expected frequencies instead of observed
#' mosaic(HairEye,
#'        labeling = labeling_points(
#'                      value_type = "expected",
#'                      scale = 2,
#'                      seed = 42)
#'       )
#'
#' # Combine with residual shading
#' mosaic(HairEye,
#'        shade = TRUE, legend = FALSE,
#'        labeling = labeling_points(scale = 2, seed = 42))
#'
#' # Make tiles show expected frequencies, show points for observed frequencies
#' # Reproduces: Fig 6 in Friendly (1995)
#' mosaic(HairEye,
#'        type = "expected",
#'        shade = TRUE, legend = FALSE,
#'        labeling = labeling_points(scale = 2, seed = 42))
#'
#' @importFrom grid seekViewport pushViewport popViewport upViewport viewport
#' @importFrom grid grid.points unit gpar
#' @importFrom stats runif
#' @importFrom vcd labeling_border
#' @export
labeling_points <- function(
    labels = TRUE,
    varnames = labels,
    value_type = c("observed", "expected"),
    scale = 1,
    pch = 19,
    size = unit(0.5, "char"),
    gp_points = gpar(col = "black", alpha = 0.7),
    margin = unit(0.05, "npc"),
    seed = NULL,
    jitter = 1,
    clip = FALSE,
    ...
) {
  value_type <- match.arg(value_type)

  # Ensure margin is a unit

if (!inherits(margin, "unit")) {
    margin <- unit(margin, "npc")
  }

  # Ensure size is a unit
  if (!inherits(size, "unit")) {
    size <- unit(size, "char")
  }

  # Return the labeling function
  function(d, split_vertical, condvars, prefix = "") {

    # Draw border labels first (variable names and factor levels)
    labeling_border(labels = labels, varnames = varnames, ...)(
      d, split_vertical, condvars, prefix
    )

    # Get dimnames
    if (is.table(d) || inherits(d, "structable")) {
      d <- dimnames(d)
    }
    dn <- names(d)
    ld <- length(d)

    # Get the frequency values from the parent frame
    # (strucplot stores observed in 'x' and expected in 'expected')
    lookup <- if (value_type == "observed") "x" else "expected"
    if (!exists(lookup, envir = parent.frame())) {
      stop(paste("Could not find", sQuote(value_type), "values in calling environment."))
    }
    values <- get(lookup, envir = parent.frame())

    # Set seed for reproducibility if provided
    if (!is.null(seed)) set.seed(seed)

    # Recursive function to traverse all cells
    # (adapted from labeling_cells in vcd)
    draw_points <- function(vind = 1, labs = c()) {
      n <- d[[vind]]

      for (labind in seq_along(n)) {
        lab <- c(labs, n[labind])
        names(lab) <- names(d)[1:vind]

        # Build viewport name for this cell
        mlab <- paste(prefix, "cell:",
                      paste(dn[1:vind], lab, sep = "=", collapse = ","),
                      sep = "")

        if (vind < ld) {
          # Not at leaf level yet - recurse deeper
          draw_points(vind + 1, lab)
        } else {
          # At leaf level - draw points in this cell
          seekViewport(mlab)

          # Push a viewport with margins and optional clipping
          pushViewport(viewport(
            width = unit(1, "npc") - 2 * margin,
            height = unit(1, "npc") - 2 * margin,
            clip = if (clip) "on" else "off"
          ))

          # Get the count for this cell
          count <- do.call("[", c(list(values), as.list(lab)))

          # Calculate number of points
          n_points <- round(count / scale)

          if (n_points > 0) {
            # Generate random positions
            if (jitter >= 1) {
              # Full random placement
              x_pos <- runif(n_points)
              y_pos <- runif(n_points)
            } else {
              # Jittered grid for more even distribution
              n_side <- ceiling(sqrt(n_points))
              grid_x <- rep(seq(0.5/n_side, 1 - 0.5/n_side, length.out = n_side), n_side)
              grid_y <- rep(seq(0.5/n_side, 1 - 0.5/n_side, length.out = n_side), each = n_side)
              # Take only n_points from the grid
              idx <- seq_len(min(n_points, length(grid_x)))
              x_pos <- grid_x[idx] + runif(length(idx), -jitter/(2*n_side), jitter/(2*n_side))
              y_pos <- grid_y[idx] + runif(length(idx), -jitter/(2*n_side), jitter/(2*n_side))
              # Clamp to [0, 1]
              x_pos <- pmax(0, pmin(1, x_pos))
              y_pos <- pmax(0, pmin(1, y_pos))
            }

            # Draw the points
            grid.points(
              x = x_pos,
              y = y_pos,
              pch = pch,
              size = size,
              gp = gp_points,
              default.units = "npc"
            )
          }

          popViewport()
        }
      }
    }

    # Start the recursive traversal
    draw_points()

    # Return to base viewport
    seekViewport(paste(prefix, "base", sep = ""))
    upViewport(1)
  }
}

class(labeling_points) <- "grapcon_generator"
