#' Diagonal Panel Function for Pairs Plots of Contingency Tables
#'
#' An enhanced replacement for \code{\link[vcd]{pairs_diagonal_mosaic}} from the
#' \pkg{vcd} package. This version fixes two bugs in the original (the
#' \code{labeling} and \code{alternate_labels} arguments were hardcoded and
#' ignored) and changes the default labeling to 
#' \code{\link[vcd]{labeling_border}}.
#'
#' A companion \code{pairs.table} method is also provided that uses this
#' improved function as the default diagonal panel.
#'
#' @param split_vertical Logical; passed to \code{\link[vcd]{mosaic}}.
#'   Default is \code{TRUE}.
#' @param margins A \code{\link[grid]{unit}} object giving the margins around
#'   the mosaic. Default is \code{unit(0, "lines")}.
#' @param offset_labels Numeric; offset for the cell labels. Default is
#'   \code{-0.4}.
#' @param offset_varnames Numeric; offset for the variable name labels.
#'   Default is \code{0}.
#' @param gp A \code{\link[grid]{gpar}} object for the mosaic tiles, or
#'   \code{NULL} (default) to use \code{fill}.
#' @param fill Either a color name or a function mapping an integer to a
#'   vector of colors (e.g. a shading function). Default is \code{"grey"}.
#' @param labeling A labeling function such as
#'   \code{\link[vcd]{labeling_border}} or \code{\link[vcd]{labeling_values}}.
#'   Default is \code{\link[vcd]{labeling_border}} (no cell counts shown).
#' @param alternate_labels Logical; whether to alternate label positions on
#'   the axes. Default is \code{TRUE}.
#' @param ... Additional arguments passed to \code{\link[vcd]{mosaic}}.
#'
#' @details
#' The function follows the \code{"grapcon_generator"} pattern used throughout
#' \pkg{vcd}: calling \code{pairs_diagonal_mosaic(...)} returns a
#' \emph{function} suitable for passing as the \code{diag_panel} argument of
#' \code{\link[vcd]{pairs.table}}.
#'
#' The original \code{vcd::pairs_diagonal_mosaic} has two bugs: (1) the
#' \code{labeling} argument is accepted but then hardcoded to
#' \code{labeling_values} inside the returned function, and (2)
#' \code{alternate_labels} is similarly hardcoded to \code{TRUE}.  This version
#' fixes both, making those arguments work as documented.
#' 
#' The default labeling scheme was changed from \code{\link[vcd]{labeling_values}} to 
#' \code{\link[vcd]{labeling_border}}, so as to disable cell counts by default. To
#' enable cell counts, use \code{\link[vcd]{labeling_values}}.
#'
#' @return A function of class \code{"grapcon_generator"} suitable for use as
#'   the \code{diag_panel} argument in \code{\link[vcd]{pairs.table}}.
#'
#' @seealso
#' \code{\link[vcd]{pairs.table}}, \code{\link[vcd]{pairs_diagonal_mosaic}},
#' \code{\link[vcd]{labeling_border}}, \code{\link[vcd]{labeling_values}}
#'
#' @examples
#' data(Hoyt, package = "vcdExtra")
#' pred_tab <- margin.table(Hoyt, c("Rank", "Occupation", "Sex"))
#'
#' # Default: no cell counts in diagonal panels
#' pairs(pred_tab)
#'
#' # Show cell counts in diagonal panels
#' pairs(pred_tab, diag_panel_args = list(labeling = labeling_values))
#'
#' # Use residual shading for off-diagonal panels
#' pairs(pred_tab, gp = shading_Friendly)
#'
#' @importFrom vcd labeling_border labeling_values mosaic
#' @importFrom grid gpar unit
#' @export
pairs_diagonal_mosaic <- function(split_vertical = TRUE,
                                  margins = unit(0, "lines"),
                                  offset_labels = -0.4,
                                  offset_varnames = 0,
                                  gp = NULL,
                                  fill = "grey",
                                  labeling = labeling_border,
                                  alternate_labels = TRUE,
                                  ...) {

  function(x, i) {
    if (is.function(fill))
      fill <- rev(fill(dim(x)[i]))
    if (is.null(gp))
      gp <- gpar(fill = fill)
    mosaic(margin.table(x, i),
           newpage = FALSE,
           split_vertical = split_vertical,
           margins = margins,
           offset_labels = offset_labels,
           offset_varnames = offset_varnames,
           prefix = "diag",
           gp = gp,
           labeling = labeling,
           labeling_args = list(alternate_labels = alternate_labels),
           ...)
  }
}

class(pairs_diagonal_mosaic) <- "grapcon_generator"


#' @rdname pairs_diagonal_mosaic
#'
#' @param x A contingency table (object of class \code{"table"}).
#' @param diag_panel A \code{"grapcon_generator"} function or a panel function
#'   for the diagonal cells. Defaults to the vcdExtra version of
#'   \code{\link{pairs_diagonal_mosaic}}.
#' @param ... Additional arguments passed to the \pkg{vcd} \code{pairs.table} method.
#'
#' @details
#' \code{pairs.table} is an S3 method for \code{\link[graphics]{pairs}} that
#' overrides the version in \pkg{vcd} solely to change the default
#' \code{diag_panel} to the improved \code{pairs_diagonal_mosaic} defined in
#' this package. All actual rendering is delegated to the \pkg{vcd} implementation,
#' retrieved via \code{utils::getFromNamespace()} to avoid a \code{:::} check NOTE.
#'
#' @export
pairs.table <- function(x, diag_panel = pairs_diagonal_mosaic, ...) {
  vcd_pairs_table <- utils::getFromNamespace("pairs.table", "vcd")
  vcd_pairs_table(x, diag_panel = diag_panel, ...)
}
