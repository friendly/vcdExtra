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
  values = c("freq", "residuals"),
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
  values = c("freq", "residuals"),
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
  values = c("freq", "residuals"),
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
  values = c("freq", "residuals"),
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
  values = c("freq", "residuals"),
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

  A `"table"`, `"xtabs"`, `"matrix"`, `"ftable"`, `"structable"`, or
  `"data.frame"` object

- ...:

  Additional arguments passed to methods

- formula:

  Formula specifying a `row_vars ~ col_vars` layout (for multi-way
  tables) to make them "flat" as defined for
  [`vcd::structable()`](https://rdrr.io/pkg/vcd/man/structable.html) and
  [`stats::ftable()`](https://rdrr.io/r/stats/ftable.html).

- values:

  What values to display in cells: `"freq"` for observed frequencies
  (default), or `"residuals"` to display the residual values. When
  `values = "residuals"`, margins are suppressed since residuals don't
  have meaningful totals.

- shade:

  What values determine cell shading: `"residuals"` (default), `"freq"`,
  `"pearson"`, or `"deviance"`

- model:

  A fitted model (loglm or glm) to compute residuals from. If NULL and
  shade involves residuals, uses an independence model for all factors.

- expected:

  Expected frequencies (alternative to `model`), a data structure of the
  same shape as `x`

- palette:

  Color palette function or vector for background colors. Default
  depends on shade type. When `shade = "freq"` the default is
  `palette = c("white", "firebrick")`; otherwise
  `c("#B2182B", "white", , "#2166AC")` ranging from red to blue for
  negative and positive residuals. The background colors are computed by
  interpolation using
  [`scales::col_numeric()`](https://scales.r-lib.org/reference/col_numeric.html).

- legend:

  Controls display of shading interpretation note: `TRUE` or `"note"`
  (default) adds a source note explaining the shading; `FALSE`
  suppresses the note.

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
  The file format is determined by the file extension. Other arguments
  can be passed to
  [`gtsave`](https://gt.rstudio.com/reference/gtsave.html) via `...`.

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
[`loglm`](https://rdrr.io/pkg/MASS/man/loglm.html), unless you specify a
model using the `model` or `expected` arguments. A message is printed
showing the chi-squared statistic, degrees of freedom, and p-value for
this test.

**Contrast shading**

For cells with dark background colors, black text can be difficult to
read. This function automatically selects white or black text for each
cell based on which provides better contrast against the background
color. If the colorspace package is available,
[`contrast_ratio`](https://colorspace.R-Forge.R-project.org/reference/contrast_ratio.html)
is used to determine the optimal text color according to WCAG 2.1
guidelines. Otherwise, a fallback based on relative luminance (ITU-R
BT.709) is used.

**Use in documents**

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

If you need a caption for cross-referencing (especially in Quarto or R
Markdown), you can use
[`gt::tab_caption()`](https://gt.rstudio.com/reference/tab_caption.html)

         gt_object |> tab_caption(caption = "Table 1: Pattern of Association in MyTable")
     

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
HEC <- margin.table(HairEyeColor, 1:2)  # 2-way: Hair x Eye
color_table(HEC)

# Shade by frequencies instead (no message printed)
color_table(HEC, shade = "freq")

# 3-way table - using a formula to specify layout
color_table(HairEyeColor, formula = Eye ~ Hair + Sex)

# Display residual values in cells instead of frequencies
color_table(HEC, values = "residuals")

# From a data.frame in frequency form (2-way)
hec_df <- as.data.frame(HEC)
color_table(hec_df)

# Save table as an image file
color_table(HEC, filename = "hair_eye_table.png")
} # }
```
