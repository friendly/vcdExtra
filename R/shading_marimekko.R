#' Categorical Shading for Mosaic Displays (Marimekko Style)
#'
#' A non-residual-based shading function for mosaic plots that colors tiles
#' by category levels of a selected dimension, similar to the style used
#' in \pkg{ggmosaic} and marimekko charts. This is useful for showing
#' the composition of sub-categories within each split of a mosaic display.
#'
#' Unlike residual-based shadings (e.g., \code{\link[vcd]{shading_hcl}}),
#' this function returns a \code{\link[grid]{gpar}} object directly,
#' not a \code{grapcon_generator} function. As a result, it must be passed
#' to the \code{gp} argument of \code{\link[vcd]{mosaic}}, and the
#' built-in legend must be suppressed with \code{legend = FALSE}.
#' Use \code{\link{legend_marimekko}} to draw a legend separately.
#'
#' This is an enhanced replacement for \code{\link[vcd]{shading_Marimekko}}
#' (uppercase M) in \pkg{vcd}, which is limited to coloring by dimension 2
#' only, has no border or legend support, and uses
#' \code{\link[colorspace]{rainbow_hcl}} which can produce hard-to-distinguish
#' adjacent colors.
#'
#' @param x A \code{table}, \code{array}, or \code{structable} object.
#' @param dim Integer specifying which dimension of \code{x} provides the
#'        coloring categories. If \code{NULL} (default), uses dimension 2
#'        for tables with 2+ dimensions, or dimension 1 for 1-way tables.
#'        Overridden by \code{byrow = TRUE} (which sets \code{dim = 1}).
#' @param fill A character vector of colors, a function that takes an integer
#'        and returns colors, or \code{NULL} (default) to use
#'        \code{\link[colorspace]{qualitative_hcl}} with the \code{"Dark 3"} palette.
#' @param col Border color for tiles. Default is \code{"black"}.
#'        Use \code{NA} or \code{"transparent"} for no borders.
#' @param lty Line type for tile borders. Default is \code{"solid"}.
#' @param byrow Logical. If \code{TRUE} and \code{dim} is not specified,
#'        color by rows (dimension 1) instead of columns (dimension 2).
#'        Default is \code{FALSE}.
#'
#' @return A \code{\link[grid]{gpar}} object with \code{fill}, \code{col},
#'   and \code{lty} components. The fill values are an array matching the
#'   dimensions of \code{x}, with colors assigned by the levels of the
#'   selected dimension. A \code{"legend"} attribute is attached containing
#'   the fill colors, labels, and variable name for use by
#'   \code{\link{legend_marimekko}}.
#'
#' @references
#' Friendly, M. (1994). Mosaic Displays for Multi-Way Contingency Tables.
#' \emph{Journal of the American Statistical Association}, \strong{89}, 190-200.
#' \doi{10.2307/2291215}
#'
#' Friendly, M. (1995). Conceptual and Visual Models for Categorical Data.
#' \emph{The American Statistician}, \strong{49}, 153-160.
#' \doi{10.1080/00031305.1995.10476131}
#'
#' @seealso \code{\link{legend_marimekko}} for drawing a separate legend;
#'   \code{\link[vcd]{shading_Marimekko}} for the simpler version in \pkg{vcd};
#'   \code{\link[vcd]{shading_hcl}} for residual-based shading;
#'   \code{\link{labeling_points}} for combining dot-density overlays with
#'   marimekko shading
#'
#' @examples
#' library(vcd)
#'
#' # --- 2-way table: Hair x Eye ---
#' HairEye <- margin.table(HairEyeColor, 1:2)
#'
#' # Color tiles by Eye color (dim 2, the default)
#' gp <- shading_marimekko(HairEye)
#' mosaic(HairEye, gp = gp, legend = FALSE)
#' legend_marimekko(gp)
#'
#' # Color tiles by Hair color (dim 1)
#' gp1 <- shading_marimekko(HairEye, dim = 1)
#' mosaic(HairEye, gp = gp1, legend = FALSE)
#' legend_marimekko(gp1)
#'
#' \donttest{
#' # --- 3-way table: Hair x Eye x Sex ---
#' gp3 <- shading_marimekko(HairEyeColor, dim = 2)
#' mosaic(HairEyeColor, gp = gp3, legend = FALSE)
#' legend_marimekko(gp3)
#'
#' # --- Custom palette ---
#' gp_custom <- shading_marimekko(HairEye, fill = c("pink", "lightblue", "lightgreen", "wheat"))
#' mosaic(HairEye, gp = gp_custom, legend = FALSE)
#'
#' # --- Combine with labeling_points ---
#' gp <- shading_marimekko(HairEye)
#' mosaic(HairEye, gp = gp, legend = FALSE,
#'        labeling = labeling_points(scale = 2, seed = 42))
#' legend_marimekko(gp)
#' }
#'
#' @importFrom colorspace qualitative_hcl
#' @importFrom grid gpar
#' @export
shading_marimekko <- function(x, dim = NULL, fill = NULL,
                               col = "black", lty = "solid",
                               byrow = FALSE) {
  if (!is.array(x) && !is.table(x))
    stop("'x' must be a table or array")

  d <- dim(x)
  ndim <- length(d)

  # Determine which dimension provides the coloring categories

  if (is.null(dim)) {
    dim <- if (byrow || ndim == 1L) 1L else 2L
  }
  stopifnot(dim >= 1L, dim <= ndim)

  ncats <- d[dim]

  # Generate fill colors
  if (is.null(fill)) fill <- colorspace::qualitative_hcl(ncats, palette = "Dark 3")
  if (is.function(fill)) fill <- fill(ncats)
  fill <- rep_len(fill, ncats)

  # Build fill array: each cell gets the color of its level along `dim`
  fill_array <- array("", dim = d)
  idx <- slice.index(fill_array, dim)
  fill_array[] <- fill[idx]

  # Construct gpar
  result <- gpar(fill = fill_array, col = col, lty = lty)

  # Attach legend info as attribute
  dn <- dimnames(x)
  attr(result, "legend") <- list(
    fill    = fill,
    labels  = if (!is.null(dn)) dn[[dim]] else as.character(seq_len(ncats)),
    varname = if (!is.null(names(dn))) names(dn)[dim] else paste0("Dim", dim)
  )

  result
}


#' Draw Legend for Marimekko Shading
#'
#' Draws a legend for a mosaic plot colored with \code{\link{shading_marimekko}}.
#' This is needed because \code{shading_marimekko} returns a \code{\link[grid]{gpar}}
#' object (not a \code{grapcon_generator}), so the built-in \code{strucplot}
#' legend mechanism does not apply.
#'
#' @param gp A \code{\link[grid]{gpar}} object returned by
#'        \code{\link{shading_marimekko}}, which carries a \code{"legend"}
#'        attribute with the fill colors, labels, and variable name.
#' @param x Position of the legend, passed to \code{\link[vcd]{grid_legend}}.
#'        Default is \code{"bottomright"}.
#' @param ... Additional arguments passed to \code{\link[vcd]{grid_legend}},
#'        such as \code{frame}, \code{title}, or \code{gp}.
#'
#' @return Invisibly returns \code{NULL}. Called for its side effect of
#'   drawing a legend on the current grid graphics device.
#'
#' @seealso \code{\link{shading_marimekko}}, \code{\link[vcd]{grid_legend}}
#'
#' @examples
#' library(vcd)
#'
#' HairEye <- margin.table(HairEyeColor, 1:2)
#' gp <- shading_marimekko(HairEye)
#' mosaic(HairEye, gp = gp, legend = FALSE)
#' legend_marimekko(gp)
#'
#' @importFrom vcd grid_legend
#' @export
legend_marimekko <- function(gp, x = "bottomright", ...) {
  info <- attr(gp, "legend")
  if (is.null(info)) return(invisible(NULL))
  grid_legend(x = x, labels = info$labels,
              pch = 22, col = info$fill,
              title = info$varname, frame = TRUE, ...)
  invisible(NULL)
}
