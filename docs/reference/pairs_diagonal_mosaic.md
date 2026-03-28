# Diagonal Panel Function for Pairs Plots of Contingency Tables

An enhanced replacement for
[`pairs_diagonal_mosaic`](https://rdrr.io/pkg/vcd/man/panel_pairs_diagonal.html)
from the vcd package. This version fixes two bugs in the original (the
`labeling` and `alternate_labels` arguments were hardcoded and ignored)
and changes the default labeling to
[`labeling_border`](https://rdrr.io/pkg/vcd/man/labeling_border.html).

## Usage

``` r
pairs_diagonal_mosaic(
  split_vertical = TRUE,
  margins = unit(0, "lines"),
  offset_labels = -0.4,
  offset_varnames = 0,
  gp = NULL,
  fill = "grey",
  labeling = labeling_border,
  alternate_labels = TRUE,
  ...
)

# S3 method for class 'table'
pairs(x, diag_panel = pairs_diagonal_mosaic, ...)
```

## Arguments

- split_vertical:

  Logical; passed to
  [`mosaic`](https://rdrr.io/pkg/vcd/man/mosaic.html). Default is
  `TRUE`.

- margins:

  A [`unit`](https://rdrr.io/r/grid/unit.html) object giving the margins
  around the mosaic. Default is `unit(0, "lines")`.

- offset_labels:

  Numeric; offset for the cell labels. Default is `-0.4`.

- offset_varnames:

  Numeric; offset for the variable name labels. Default is `0`.

- gp:

  A [`gpar`](https://rdrr.io/r/grid/gpar.html) object for the mosaic
  tiles, or `NULL` (default) to use `fill`.

- fill:

  Either a color name or a function mapping an integer to a vector of
  colors (e.g. a shading function). Default is `"grey"`.

- labeling:

  A labeling function such as
  [`labeling_border`](https://rdrr.io/pkg/vcd/man/labeling_border.html)
  or
  [`labeling_values`](https://rdrr.io/pkg/vcd/man/labeling_border.html).
  Default is
  [`labeling_border`](https://rdrr.io/pkg/vcd/man/labeling_border.html)
  (no cell counts shown).

- alternate_labels:

  Logical; whether to alternate label positions on the axes. Default is
  `TRUE`.

- ...:

  Additional arguments passed to the vcd `pairs.table` method.

- x:

  A contingency table (object of class `"table"`).

- diag_panel:

  A `"grapcon_generator"` function or a panel function for the diagonal
  cells. Defaults to the vcdExtra version of `pairs_diagonal_mosaic`.

## Value

A function of class `"grapcon_generator"` suitable for use as the
`diag_panel` argument in
[`pairs.table`](https://rdrr.io/pkg/vcd/man/pairs.table.html).

## Details

A companion `pairs.table` method is also provided that uses this
improved function as the default diagonal panel.

The function follows the `"grapcon_generator"` pattern used throughout
vcd: calling `pairs_diagonal_mosaic(...)` returns a *function* suitable
for passing as the `diag_panel` argument of
[`pairs.table`](https://rdrr.io/pkg/vcd/man/pairs.table.html).

The original
[`vcd::pairs_diagonal_mosaic`](https://rdrr.io/pkg/vcd/man/panel_pairs_diagonal.html)
has two bugs: (1) the `labeling` argument is accepted but then hardcoded
to `labeling_values` inside the returned function, and (2)
`alternate_labels` is similarly hardcoded to `TRUE`. This version fixes
both, making those arguments work as documented.

The default labeling scheme was changed from
[`labeling_values`](https://rdrr.io/pkg/vcd/man/labeling_border.html) to
[`labeling_border`](https://rdrr.io/pkg/vcd/man/labeling_border.html),
so as to disable cell counts by default. To enable cell counts, use
[`labeling_values`](https://rdrr.io/pkg/vcd/man/labeling_border.html).

`pairs.table` is an S3 method for
[`pairs`](https://rdrr.io/r/graphics/pairs.html) that overrides the
version in vcd solely to change the default `diag_panel` to the improved
`pairs_diagonal_mosaic` defined in this package. All actual rendering is
delegated to the vcd implementation, retrieved via
[`utils::getFromNamespace()`](https://rdrr.io/r/utils/getFromNamespace.html)
to avoid a `:::` check NOTE.

## See also

[`pairs.table`](https://rdrr.io/pkg/vcd/man/pairs.table.html),
[`pairs_diagonal_mosaic`](https://rdrr.io/pkg/vcd/man/panel_pairs_diagonal.html),
[`labeling_border`](https://rdrr.io/pkg/vcd/man/labeling_border.html),
[`labeling_values`](https://rdrr.io/pkg/vcd/man/labeling_border.html)

## Examples

``` r
data(Hoyt, package = "vcdExtra")
pred_tab <- margin.table(Hoyt, c("Rank", "Occupation", "Sex"))

# Default: no cell counts in diagonal panels
pairs(pred_tab)


# Show cell counts in diagonal panels
pairs(pred_tab, diag_panel_args = list(labeling = labeling_values))


# Use residual shading for off-diagonal panels
pairs(pred_tab, gp = shading_Friendly)

```
