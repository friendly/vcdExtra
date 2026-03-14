#' Render a gt table object in any knitr output format
#'
#' In HTML output the gt object is returned as-is (gt's knit_print handles it).
#' In PDF, Word, or other non-HTML output the table is saved as a PNG via
#' gt::gtsave() and included with knitr::include_graphics().
#'
#' Designed to be used at the end of a pipe:
#'
#'   color_table(HEC, title = "Hair x Eye") |> include_gt()
#'   gt::gt(my_df) |> gt::fmt_number() |> include_gt(file = "my_table")
#'
#' @param gt_obj  A gt table object.
#' @param file    Base filename (no extension) for the saved PNG.
#'                Only used in non-HTML output. Defaults to a temp file.
#' @param width   Viewport width in pixels passed to gt::gtsave().
#' @param height  Viewport height in pixels passed to gt::gtsave().
#' @param ...     Additional arguments passed to gt::gtsave().
#' @return In HTML output, the gt object (rendered by knit_print).
#'         In other formats, the result of knitr::include_graphics().

include_gt <- function(gt_obj,
                       file   = tempfile("gt_", tmpdir = "."),
                       width  = 600,
                       height = 400,
                       ...) {
  if (knitr::is_html_output()) {
    gt_obj
  } else {
    img <- paste0(file, ".png")
    gt::gtsave(gt_obj, filename = img, vwidth = width, vheight = height, ...)
    knitr::include_graphics(img)
  }
}

# --- Quick tests (run interactively or via rmarkdown::render) ----------------

library(vcdExtra)

data(HairEyeColor)
HEC <- margin.table(HairEyeColor, 1:2)

# Basic pipe usage
color_table(HEC, title = "Hair \u00d7 Eye Color") |>
  include_gt()

# Controlling image size for non-HTML output
color_table(HEC, shade = "freq", title = "Frequency shading") |>
  include_gt(file = "hec_freq", width = 520, height = 300)

# Works on any gt object, not just color_table()
as.data.frame(HEC) |>
  gt::gt() |>
  gt::tab_header(title = "Hair \u00d7 Eye (plain gt)") |>
  include_gt(file = "hec_plain", width = 400, height = 300)

# 3-way table
color_table(HairEyeColor,
            formula = Eye ~ Hair + Sex,
            legend  = TRUE,
            title   = "3-way: Eye ~ Hair + Sex") |>
  include_gt(file = "hec_3way", width = 620, height = 340)
