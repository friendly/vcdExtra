# Display Frequency Table with Colored Cell Backgrounds

Creates a formatted table display of frequency data with cell
backgrounds colored according to observed frequencies or their residuals
from a loglinear model.

## Usage

``` r
color_table(
  x,
  formula = NULL,
  shade = c("residuals", "freq", "pearson", "deviance"),
  model = NULL,
  expected = NULL,
  palette = NULL,
  legend = TRUE,
  margins = TRUE,
  digits = 0,
  title = NULL,
  filename = NULL,
  ...
)
```

## Arguments

- x:

  A table, xtabs, matrix, ftable, or structable object

- formula:

  Formula specifying row ~ col layout (passed to structable if needed)

- shade:

  What values determine cell shading: "residuals" (default), "freq",
  "pearson", or "deviance"

- model:

  A fitted model (loglm or glm) to compute residuals from. If NULL and
  shade involves residuals, uses independence model.

- expected:

  Expected frequencies (alternative to model)

- palette:

  Color palette function or vector. Default depends on shade type.

- legend:

  Logical, show color legend/scale?

- margins:

  Logical, include row/column totals?

- digits:

  Number of decimal places for displayed values

- title:

  Optional table title

- filename:

  Optional filename to save the table as an image. If provided, the
  table is saved using
  [`gtsave`](https://gt.rstudio.com/reference/gtsave.html). Supported
  formats include `.png`, `.svg`, `.pdf`, `.html`, `.rtf`, and `.docx`.
  The format is determined by the file extension.

- ...:

  Additional arguments passed to
  [`gtsave`](https://gt.rstudio.com/reference/gtsave.html) when
  `filename` is specified (e.g., `vwidth`, `vheight` for image
  dimensions)

## Value

A gt table object that can be further customized

## Details

This function provides a heatmap-style representation of a frequency
table, where background coloring is used to visualize patterns and
anomalies in the data. When shading by residuals (the default), cells
with large positive residuals (more observations than expected) are
shaded red, while cells with large negative residuals (fewer than
expected) are shaded blue. This makes it easy to identify cells that
deviate substantially from what would be expected under a given model
(by default, the independence model).

For cells with dark background colors, black text can be difficult to
read. This function automatically selects white or black text for each
cell based on which provides better contrast against the background
color. If the colorspace package is available,
[`contrast_ratio`](https://colorspace.R-Forge.R-project.org/reference/contrast_ratio.html)
is used to determine the optimal text color according to WCAG 2.1
guidelines. Otherwise, a fallback based on relative luminance (ITU-R
BT.709) is used.

In R Markdown (`.Rmd`) or Quarto (`.qmd`) documents, gt tables may not
render correctly in all output formats. The `filename` argument provides
a workaround: save the table as an image, then include it using
[`include_graphics`](https://rdrr.io/pkg/knitr/man/include_graphics.html).
For example:

    color_table(my_table, filename = "my_table.png")
    knitr::include_graphics("my_table.png")

For higher quality output, `.svg` format is recommended. You can control
the image dimensions using the `vwidth` and `vheight` arguments (passed
via `...`).

## Examples

``` r
if (FALSE) { # \dontrun{
# Basic usage - shade by residuals from independence
data(HairEyeColor)
HEC <- margin.table(HairEyeColor, 2:1)
color_table(HEC)

# Shade by frequencies instead
color_table(HEC, shade = "freq")

# 3-way table
color_table(HairEyeColor, formula = Eye ~ Hair + Sex)

# Save table as an image file
color_table(HEC, filename = "hair_eye_table.png")
color_table(HEC, filename = "hair_eye_table.svg", vwidth = 600)
} # }
```
