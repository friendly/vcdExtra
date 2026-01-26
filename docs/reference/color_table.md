# Display Frequency Table with Colored Cell Backgrounds

Creates a formatted table display of frequency data with cell
backgrounds colored according to observed frequencies or their residuals
from a loglinear model. This is an S3 generic function with methods for
different input types.

## Usage

``` r
color_table(x, ...)

# S3 method for class 'table'
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

# S3 method for class 'ftable'
color_table(
  x,
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

# S3 method for class 'structable'
color_table(
  x,
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

# S3 method for class 'data.frame'
color_table(
  x,
  formula = NULL,
  freq_col = NULL,
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

# S3 method for class 'matrix'
color_table(
  x,
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

# Default S3 method
color_table(x, ...)
```

## Arguments

- x:

  A table, xtabs, matrix, ftable, structable, or data.frame object

- ...:

  Additional arguments passed to methods

- formula:

  Formula specifying row ~ col layout (for multi-way tables)

- shade:

  What values determine cell shading: "residuals" (default), "freq",
  "pearson", or "deviance"

- model:

  A fitted model (loglm or glm) to compute residuals from. If NULL and
  shade involves residuals, uses an independence model for all factors.

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

- freq_col:

  Name of the frequency column. If NULL, looks for "Freq" or "count".

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

For multi-way tables (3 or more dimensions), residuals are computed from
the model of complete independence among all factors using
[`loglm`](https://rdrr.io/pkg/MASS/man/loglm.html). A message is printed
showing the chi-squared statistic, degrees of freedom, and p-value for
this test.

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

## Methods (by class)

- `color_table(table)`: Method for table objects (including result of
  xtabs)

- `color_table(ftable)`: Method for ftable objects

- `color_table(structable)`: Method for structable objects (vcd package)

- `color_table(data.frame)`: Method for data.frame in frequency form

- `color_table(matrix)`: Method for matrix objects

- `color_table(default)`: Default method

## Examples

``` r
if (FALSE) { # \dontrun{
# Basic usage with 2-way table - shade by residuals from independence
data(HairEyeColor)
HEC2 <- margin.table(HairEyeColor, 1:2)  # 2-way: Hair x Eye
color_table(HEC2)
# Prints: "Shading based on residuals from model of independence, X^2 = ..."

# Shade by frequencies instead (no message printed)
color_table(HEC2, shade = "freq")

# 3-way table - requires formula to specify layout
color_table(HairEyeColor, formula = Eye ~ Hair + Sex)
# Prints: "Shading based on residuals from model of complete independence, X^2 = ..."

# From a data.frame in frequency form (2-way)
hec_df <- as.data.frame(HEC2)
color_table(hec_df)

# Save table as an image file
color_table(HEC2, filename = "hair_eye_table.png")
} # }
```
