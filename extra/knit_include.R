#' Include an HTML-renderable object in any knitr output format
#'
#' A pipe-friendly helper for objects that render natively in HTML output
#' (RStudio Viewer, HTML documents) but need an image fallback for PDF, Word,
#' and other non-HTML formats.
#'
#' Supported classes:
#'   - `gt_tbl`     (gt package)       -- saved via gt::gtsave()
#'   - `htmlwidget` (htmlwidgets family: plotly, DT, leaflet, dygraphs, ...)
#'                                     -- saved via htmlwidgets::saveWidget()
#'                                        + webshot2::webshot()
#'
#' Packages that handle cross-format output natively (flextable, kableExtra,
#' huxtable) do NOT need this helper.
#'
#' For any other class, `x` is returned as-is in all output formats and knitr's
#' normal printing applies.  This means the function is safe to use in a pipe
#' on any object: it only intervenes when it knows how to help.
#'
#' @param x       Any R object. Specialised handling for `gt_tbl` and
#'                `htmlwidget`; all other classes are passed through unchanged.
#' @param file    Base filename (no extension) for temporary files written when
#'                output is not HTML.  Defaults to a session-unique temp file in
#'                the current directory so multiple calls don't collide.
#' @param width   Viewport / screenshot width in pixels (non-HTML only).
#' @param height  Viewport / screenshot height in pixels (non-HTML only).
#' @param ...     Additional arguments forwarded to gt::gtsave() or
#'                webshot2::webshot().
#'
#' @return For `gt_tbl` / `htmlwidget` in non-HTML output: the result of
#'         knitr::include_graphics(). Otherwise: `x` unchanged.
#'
#' @examples
#' \dontrun{
#' ## gt table
#' color_table(HEC) |> knit_include()
#'
#' ## DT widget
#' DT::datatable(mtcars) |> knit_include(width = 900, height = 500)
#'
#' ## plotly figure
#' plotly::plot_ly(mtcars, x = ~wt, y = ~mpg) |> knit_include()
#' }

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
