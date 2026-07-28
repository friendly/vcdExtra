#' ---
#' title: "color_table() Demo"
#' author: ""
#' date: "`r Sys.Date()`"
#' output:
#'   html_document:
#'     toc: true
#'     toc_float: true
#'     self_contained: true
#' ---

#+ setup, include=FALSE
library(vcdExtra)

#' ## Overview
#'
#' `color_table()` produces a `gt` table object with cell backgrounds shaded by
#' observed frequencies or Pearson residuals from an independence model.
#'
#' For HTML output, `gt` tables render natively — just return the object.
#' Compile this script with:
#'
#' ```r
#' rmarkdown::render("color_table_demo.R")
#' ```

#' ## Basic usage — shade by residuals

#+ hec-setup
data(HairEyeColor)
HEC <- margin.table(HairEyeColor, 1:2)   # collapse over Sex

#+ hec-residuals, message=FALSE
color_table(HEC, title = "Hair × Eye Color (residual shading)")

#' ## Shade by frequency

#+ hec-freq
color_table(HEC, shade = "freq", title = "Hair × Eye Color (frequency shading)")

#' ## 3-way table with formula layout

#+ three-way, message=FALSE
color_table(HairEyeColor,
            formula = Eye ~ Hair + Sex,
            legend  = TRUE,
            title   = "Hair × Eye × Sex (complete independence residuals)")

#' ## Display residual values in cells

#+ resid-values, message=FALSE
color_table(HEC,
            values = "residuals",
            title  = "Hair × Eye — Pearson residuals in cells")

#' ## Multi-way table — PreSex data

#+ presex, message=FALSE
data(PreSex, package = "vcd")
color_table(PreSex,
            formula = MaritalStatus ~ PremaritalSex + ExtramaritalSex,
            legend  = TRUE,
            title   = "Pre/Extra-marital Sex by Marital Status")

#' ## Helper for non-HTML output
#'
#' If you ever need to knit this script to PDF or Word, the helper below
#' falls back to saving an image and using `knitr::include_graphics()`.

#+ helper
include_color_table <- function(x, ..., file = "color_table_tmp",
                                width = 600, height = 400) {
  gt_obj <- color_table(x, ...)
  if (knitr::is_html_output()) {
    gt_obj
  } else {
    img <- paste0(file, ".png")
    gt::gtsave(gt_obj, filename = img, vwidth = width, vheight = height)
    knitr::include_graphics(img)
  }
}

#+ universal-demo, message=FALSE, out.width="70%"
include_color_table(HEC,
                    title  = "Hair × Eye Color (any output format)",
                    file   = "color_table_universal",
                    width  = 520,
                    height = 300)
