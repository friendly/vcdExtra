# Using color_table() in R Markdown

## Overview

[`color_table()`](https://friendly.github.io/vcdExtra/reference/color_table.md)
produces a `gt` table object with cell backgrounds shaded by observed
frequencies or Pearson residuals from an independence (or user-supplied)
model. The goal is a “smart” tabular display that makes patterns of
association or unusual cells immediately visible.

When used interactively in RStudio, the returned `gt` object renders
directly in the **Viewer** panel — no extra steps needed. However, using
[`color_table()`](https://friendly.github.io/vcdExtra/reference/color_table.md)
in R Markdown (`.Rmd`) or Quarto (`.qmd`) documents requires some
special consideration because `gt` tables render natively only in **HTML
output**. For **PDF or Word output**, the table must be saved as a PNG
image and included with
[`knitr::include_graphics()`](https://rdrr.io/pkg/knitr/man/include_graphics.html).

The
[`knit_include()`](https://friendly.github.io/vcdExtra/reference/knit_include.md)
helper in this package handles that branching automatically. Piping a
`gt` table through
[`knit_include()`](https://friendly.github.io/vcdExtra/reference/knit_include.md)
returns it unchanged for HTML output, and saves a PNG fallback for all
other formats — so the same chunk knits correctly regardless of the
output format.

------------------------------------------------------------------------

## HTML output — return the gt object directly

For documents that are compiled to HTML,
[`color_table()`](https://friendly.github.io/vcdExtra/reference/color_table.md)
renders natively: just return the object from a chunk and `knitr`
handles it via `gt`’s built-in
[`knitr::knit_print()`](https://rdrr.io/pkg/knitr/man/knit_print.html)
method. No `filename` argument or extra wrapper is needed.

The first example shades cells by Pearson residuals from the
independence model (the default), making over- and under-represented
Hair×Eye combinations immediately visible.

``` r

data(HairEyeColor)
HEC <- margin.table(HairEyeColor, 1:2)   # collapse over Sex

color_table(HEC, title = "Hair \u00d7 Eye Color (residual shading)")
```

    ## Shading based on residuals from model of independence,
    ##  X^2 = 138.29, df = 9, p = 2.325e-25

[TABLE]

For comparison, shading by raw frequencies (rather than residuals) gives
a different picture — it highlights which combinations are simply most
common, not which ones deviate from independence.

``` r

color_table(HEC, shade = "freq", title = "Hair \u00d7 Eye Color (frequency shading)")
```

[TABLE]

For a three-way table, a `formula` argument controls the layout,
following the `col_vars ~ row_vars` convention used by
[`vcd::structable()`](https://rdrr.io/pkg/vcd/man/structable.html) and
[`stats::ftable()`](https://rdrr.io/r/stats/ftable.html): variable(s) on
the **left** of `~` become the column category (or nested column
spanners, if there’s more than one), and variable(s) on the **right**
become the row category (or separate row-label columns, if there’s more
than one). The legend note reproduces the chi-squared summary printed to
the console.

Here `Eye` is the single column variable, and `Hair` and `Sex` are two
row variables 014 shown as two separate stub columns instead of
concatenated labels like “Black_Male”:

``` r

color_table(HairEyeColor,
            formula = Eye ~ Hair + Sex,
            legend  = TRUE,
            title   = "Hair \u00d7 Eye \u00d7 Sex (complete independence residuals)")
```

    ## Re-fitting to get frequencies and fitted values
    ## Shading based on residuals from model of complete independence, X^2 = 164.92, df = 24, p = 0

[TABLE]

Swapping the formula around puts `Hair` and `Sex` on the columns
instead, shown as nested spanner headings, with `Eye` as the single row
variable:

``` r

color_table(HairEyeColor,
            formula = Hair + Sex ~ Eye,
            legend  = TRUE,
            title   = "Hair \u00d7 Sex as column spanners, Eye as rows")
```

    ## Re-fitting to get frequencies and fitted values
    ## Shading based on residuals from model of complete independence, X^2 = 164.92, df = 24, p = 0

[TABLE]

------------------------------------------------------------------------

## PDF / Word output — save image, then include it

For non-HTML output, save the table as a PNG and include with
[`knitr::include_graphics()`](https://rdrr.io/pkg/knitr/man/include_graphics.html).
Supported formats: `.png`, `.pdf`, `.html`, `.rtf`, `.docx`. The
`vwidth` and `vheight` arguments control the image viewport in pixels.

The chunk below is shown for illustration only (`eval=FALSE`); the HTML
approach above is sufficient when knitting to HTML.

``` r

color_table(HEC,
            title    = "Hair \u00d7 Eye Color",
            filename = "color_table_hec.png",
            vwidth   = 520,
            vheight  = 300)

knitr::include_graphics("color_table_hec.png")
```

------------------------------------------------------------------------

## Universal output — `knit_include()`

[`knit_include()`](https://friendly.github.io/vcdExtra/reference/knit_include.md)
eliminates the need to branch on output format by hand. Pipe the `gt`
object through it and the same chunk works correctly whether you are
knitting to HTML, PDF, or Word.

``` r

color_table(HEC,
            title = "Hair \u00d7 Eye Color") |>
  knit_include(width = 520, height = 300)
```

    ## Shading based on residuals from model of independence,
    ##  X^2 = 138.29, df = 9, p = 2.325e-25

[TABLE]

[`knit_include()`](https://friendly.github.io/vcdExtra/reference/knit_include.md)
also handles `htmlwidget` objects (plotly, DT, leaflet, …) the same way
— HTML output gets the interactive widget, PDF/Word output gets a
screenshot. Any other class is passed through unchanged, so it is safe
to use in a pipe on any object.

------------------------------------------------------------------------

## More examples

### Displaying residual values in cells

Setting `values = "residuals"` replaces the cell frequencies with the
Pearson residual values themselves. This is useful when the exact
magnitude of each deviation matters, not just its direction. Row and
column totals are suppressed automatically because residuals do not have
meaningful marginal sums.

``` r

color_table(HEC,
            values = "residuals",
            title  = "Hair \u00d7 Eye \u2014 Pearson residuals") |>
  knit_include(width = 520, height = 280)
```

    ## Shading based on residuals from model of independence,
    ##  X^2 = 138.29, df = 9, p = 2.325e-25

[TABLE]

### Multi-way table: PreSex data

The `PreSex` data cross-classifies four variables. Here `MaritalStatus`
is the single column variable, and the two sexual-attitude variables
(`PremaritalSex`, `ExtramaritalSex`) are row variables, shown as
separate stub columns rather than concatenated labels — making the
interaction structure easy to read. The legend note records the
goodness-of-fit for the complete-independence model.

``` r

data(PreSex, package = "vcd")
color_table(PreSex,
            formula = MaritalStatus ~ PremaritalSex + ExtramaritalSex,
            legend  = TRUE,
            title   = "Pre/Extra-marital Sex by Marital Status") |>
  knit_include(width = 520, height = 300)
```

    ## Re-fitting to get frequencies and fitted values
    ## Shading based on residuals from model of complete independence, X^2 = 270.14, df = 11, p = 0

[TABLE]

### Row and column variables together: Titanic data

Column spanners and row-label columns can appear in the same table. Here
`Class` and `Sex` (left of `~`) form nested column spanners, while `Age`
and `Survived` (right of `~`) are shown as two separate row-stub
columns:

``` r

data(Titanic)
color_table(Titanic,
            formula = Class + Sex ~ Age + Survived,
            legend  = TRUE,
            title   = "Titanic: Class × Sex as column spanners, Age × Survived as row stubs") |>
  knit_include(width = 620, height = 380)
```

    ## Re-fitting to get frequencies and fitted values
    ## Shading based on residuals from model of complete independence, X^2 = 1637.45, df = 25, p = 0

[TABLE]
