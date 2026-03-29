# Smart Display of Frequency Table with Colored Cell Backgrounds

`color_table()` creates a formatted, semi-graphic "heatmap" table
display of frequency data with cell backgrounds colored according to
observed frequencies or their residuals from a loglinear model. The goal
is to provide a "smart" tabular display of cross-classified frequency
data, with some abilities to highlight possibly interesting patterns or
unusual cells. This is an S3 generic function, providing methods for
different input types: `"table"`, `"xtabs"`, `"matrix"`, `"ftable"`,
`"structable"` or `"data.frame"`.

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
  legend = FALSE,
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
  legend = FALSE,
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
  legend = FALSE,
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
  legend = FALSE,
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
  legend = FALSE,
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
  (default) adds a source note explaining the shading; `FALSE` (default)
  suppresses the note, but a message is printed in the console.

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
  formats include `.png`, `.pdf`, `.html`, `.rtf`, and `.docx`. The file
  format is determined by the file extension. Other arguments can be
  passed to [`gtsave`](https://gt.rstudio.com/reference/gtsave.html) via
  `...`.

- freq_col:

  Name of the frequency column. If NULL, looks for "Freq" or "count".

## Value

A gt table object that can be further customized

## Details

This function provides a heatmap-style representation of a frequency
table, where background coloring is used to visualize patterns and
anomalies in the data. When shading by *residuals* (the default), cells
with large positive residuals (more observations than expected) are
shaded red, while cells with large negative residuals (fewer than
expected) are shaded blue. This makes it easy to identify cells that
deviate substantially from what would be expected under a given model
(by default, the independence model).

For multi-way tables (3 or more dimensions), residuals are computed from
the model of complete independence among all factors using
[`loglm`](https://rdrr.io/pkg/MASS/man/loglm.html). But you can specify
a model using the `model` or `expected` arguments, in a way similar to
that provided by
[`mosaic.glm()`](https://friendly.github.io/vcdExtra/reference/mosaic.glm.md).
A message is printed showing the chi-squared statistic, degrees of
freedom, and p-value for this test.

**Contrast shading**

For cells with dark background colors, black text can be difficult to
read. This function automatically selects white or black text for each
cell based on which provides better contrast against the background
color. If the colorspace package is available,
[`contrast_ratio`](https://rdrr.io/pkg/colorspace/man/contrast_ratio.html)
is used to determine the optimal text color according to WCAG 2.1
guidelines. Otherwise, a fallback based on relative luminance (ITU-R
BT.709) is used.

**Use in documents**

In R Markdown (`.Rmd`) or Quarto (`.qmd`) documents, gt tables render
natively in **HTML output** — simply return the `gt` object from a chunk
and knitr renders it automatically via gt's built-in `knit_print`
method. No `filename` argument is needed.

For **PDF or Word output**, gt does not render natively. Use the
`filename` argument to save the table as a `.png` image, then include it
with
[`include_graphics`](https://rdrr.io/pkg/knitr/man/include_graphics.html):

        color_table(my_table, filename = "my_table.png")
        knitr::include_graphics("my_table.png")

The `vwidth` and `vheight` arguments (passed via `...`) control the
image viewport size in pixels. Supported save formats are `.png`,
`.pdf`, `.html`, `.rtf`, and `.docx`.

For documents that target **multiple output formats**, a small helper
that branches on
[`is_html_output`](https://rdrr.io/pkg/knitr/man/output_type.html)
avoids duplicating code:

        gt_obj <- color_table(my_table)
        if (knitr::is_html_output()) {
          gt_obj
        } else {
          gt::gtsave(gt_obj, "my_table.png")
          knitr::include_graphics("my_table.png")
        }

If you need a caption or cross-reference label, use
[`gt::tab_caption()`](https://gt.rstudio.com/reference/tab_caption.html)
on the returned object:

        color_table(my_table) |>
          gt::tab_caption("Table 1: Pattern of association in MyTable")

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
