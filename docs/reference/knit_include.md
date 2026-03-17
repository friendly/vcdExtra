# Include an HTML-renderable object in any knitr output format

A pipe-friendly helper in Rmarkdown, Quarto, and other documents for
objects that render natively in HTML output (RStudio Viewer, HTML
documents) but need a convenient image fallback for PDF, Word, and other
non-HTML formats. It was designed to solve a problem in rendering the
result of
[`color_table()`](https://friendly.github.io/vcdExtra/reference/color_table.md),
and then generalized to provide for other similar cases.

## Usage

``` r
knit_include(
  x,
  file = tempfile("ki_", tmpdir = "."),
  width = 700,
  height = 400,
  ...
)
```

## Arguments

- x:

  Any R object. Specialised handling for `gt_tbl` and `htmlwidget`; all
  other classes are passed through unchanged.

- file:

  Base filename (no extension) for temporary files written when output
  is not HTML. Defaults to a session-unique temp file in the current
  directory so multiple calls don't collide.

- width:

  Viewport / screenshot width in pixels (non-HTML only).

- height:

  Viewport / screenshot height in pixels (non-HTML only).

- ...:

  Additional arguments forwarded to
  [`gtsave`](https://gt.rstudio.com/reference/gtsave.html) or
  [`webshot`](https://rstudio.github.io/webshot2/reference/webshot.html).

## Value

For `gt_tbl` / `htmlwidget` in non-HTML output: the result of
[`include_graphics`](https://rdrr.io/pkg/knitr/man/include_graphics.html).
Otherwise: `x` unchanged.

## Details

Supported classes (handled internally) solve this problem by saving the
image to a file and then inserting it in the document using
[`knitr::include_graphics()`](https://rdrr.io/pkg/knitr/man/include_graphics.html):

- `"gt_tbl"` (gt package) – saved via
  [`gtsave`](https://gt.rstudio.com/reference/gtsave.html)

- `"htmlwidget"` (htmlwidgets family: plotly, DT, leaflet, dygraphs,
  ...) – saved via
  [`saveWidget`](https://rdrr.io/pkg/htmlwidgets/man/saveWidget.html) +
  [`webshot`](https://rstudio.github.io/webshot2/reference/webshot.html)

Packages that handle cross-format output natively (flextable,
kableExtra, huxtable) do NOT need this helper.

For any other class, `x` is returned as-is in all output formats and
knitr's normal printing applies. This means the function is safe to use
in a pipe on any object: it only intervenes when it knows how to help!

## Examples

``` r
if (FALSE) { # \dontrun{
data(HairEyeColor)
HEC <- margin.table(HairEyeColor, 1:2)

## gt_tbl from color_table()
color_table(HEC, title = "Hair x Eye Color") |> knit_include()

## DT htmlwidget
DT::datatable(mtcars) |> knit_include(width = 900, height = 500)

## plotly htmlwidget
plotly::plot_ly(mtcars, x = ~wt, y = ~mpg) |> knit_include()

## Any other object passes through unchanged
lm(mpg ~ wt, data = mtcars) |> knit_include()
} # }
```
