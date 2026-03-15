#' Include an HTML-renderable object in any knitr output format
#'
#' A pipe-friendly helper in Rmarkdown, Quarto, and other documents for objects that render natively in HTML output
#' (RStudio Viewer, HTML documents) but need a convenient image fallback for PDF, Word,
#' and other non-HTML formats. It was designed to solve a problem in rendering the result of `color_table()`,
#' and then generalized to provide for other similar cases.
#'
#' Supported classes (handled internally) solve this problem by saving the image to a file and then inserting it in the document
#' using `knitr::include_graphics()`:
#'   - `"gt_tbl"`     (gt package)       -- saved via \code{\link[gt]{gtsave}}
#'   - `"htmlwidget"` (htmlwidgets family: plotly, DT, leaflet, dygraphs, ...)
#'                                     -- saved via
#'                                        \code{\link[htmlwidgets]{saveWidget}}
#'                                        + \code{\link[webshot2]{webshot}}
#'
#' Packages that handle cross-format output natively (\pkg{flextable}, \pkg{kableExtra},
#' \pkg{huxtable}) do NOT need this helper.
#'
#' For any other class, \code{x} is returned as-is in all output formats and
#' knitr's normal printing applies.  This means the function is safe to use in
#' a pipe on any object: it only intervenes when it knows how to help!
#'
#' @param x       Any R object. Specialised handling for \code{gt_tbl} and
#'                \code{htmlwidget}; all other classes are passed through
#'                unchanged.
#' @param file    Base filename (no extension) for temporary files written when
#'                output is not HTML.  Defaults to a session-unique temp file in
#'                the current directory so multiple calls don't collide.
#' @param width   Viewport / screenshot width in pixels (non-HTML only).
#' @param height  Viewport / screenshot height in pixels (non-HTML only).
#' @param ...     Additional arguments forwarded to \code{\link[gt]{gtsave}} or
#'                \code{\link[webshot2]{webshot}}.
#'
#' @return For \code{gt_tbl} / \code{htmlwidget} in non-HTML output: the result
#'         of \code{\link[knitr]{include_graphics}}. Otherwise: \code{x}
#'         unchanged.
#'
#' @examples
#' \dontrun{
#' data(HairEyeColor)
#' HEC <- margin.table(HairEyeColor, 1:2)
#'
#' ## gt_tbl from color_table()
#' color_table(HEC, title = "Hair x Eye Color") |> knit_include()
#'
#' ## DT htmlwidget
#' DT::datatable(mtcars) |> knit_include(width = 900, height = 500)
#'
#' ## plotly htmlwidget
#' plotly::plot_ly(mtcars, x = ~wt, y = ~mpg) |> knit_include()
#'
#' ## Any other object passes through unchanged
#' lm(mpg ~ wt, data = mtcars) |> knit_include()
#' }
#'
#' @importFrom knitr is_html_output include_graphics
#' @importFrom webshot2 webshot
#' @importFrom htmlwidgets saveWidget
#' @export
knit_include <- function(x,
                         file   = tempfile("ki_", tmpdir = "."),
                         width  = 700,
                         height = 400,
                         ...) {

  if (knitr::is_html_output()) {
    return(x)
  }

  if (inherits(x, "gt_tbl")) {
    img <- paste0(file, ".png")
    gt::gtsave(x, filename = img, vwidth = width, vheight = height, ...)
    return(knitr::include_graphics(img))
  }

  if (inherits(x, "htmlwidget")) {
    if (!requireNamespace("htmlwidgets", quietly = TRUE))
      stop("Package 'htmlwidgets' is required for htmlwidget objects.")
    if (!requireNamespace("webshot2", quietly = TRUE))
      stop("Package 'webshot2' is required for htmlwidget objects in non-HTML output.")

    html <- paste0(file, ".html")
    img  <- paste0(file, ".png")
    htmlwidgets::saveWidget(x, html, selfcontained = TRUE)
    webshot2::webshot(html, img, vwidth = width, vheight = height, ...)
    return(knitr::include_graphics(img))
  }

  # Unknown class: return x and let knitr's normal printing handle it,
  # just as it would outside a pipe.
  x
}
