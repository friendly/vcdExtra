# Plan: add `marginal = "density"` to `logist_plot()`

## Objective

Add a third marginal representation that displays the distribution of `x`
within the two response groups as filled, mirrored density curves:

- the density for `y = 0` grows upward from probability 0;
- the density for `y = 1` hangs downward from probability 1;
- the fitted logistic curve and confidence band are drawn over both densities.

This follows Michael's preference in `loghistplot-GK.md`: density should provide
a smoother alternative to the histograms, support alpha/line/shading control,
and expose the density bandwidth adjustment through `adjust`.

The first implementation is for the existing ungrouped logistic plot. Density
may later provide a cleaner route to multiple-group displays than stacked
histograms, but grouping remains a separate design and is not part of this
change.

## Architecture decision

Do not implement density mode with either cowplot or ggdist.

The exploratory density prototype predates `loghistplot3.R`. At that time,
histogram mode used cowplot to overlay three completed plots, so the notes
correctly recommended giving density the same architecture. Version 3 has since
replaced that composite with values precomputed and drawn directly in the main
plot's probability coordinates.

Apply the same version-3 architecture to density:

1. calculate the two densities with `stats::density()`;
2. convert their heights into the shared 0--1 probability coordinates;
3. draw them as native `geom_ribbon()` layers;
4. draw the fitted curve last.

This preserves the successful appearance of
`loghist-density-cowplot.png` while retaining an ordinary, extensible ggplot.
Post-hoc `labs()`, `theme()`, and other compatible additions will continue to
work.

Do not add a ggdist dependency. The existing `stat_slab()` experiments correctly
anchor the two shapes but fail to produce controlled, visible thickness on the
externally meaningful probability scale. Precomputed ribbon coordinates solve
that problem directly.

## Public interface

### Add the marginal choice

Change the choices everywhere from:

```r
marginal = c("hist", "points")
```

to:

```r
marginal = c("hist", "points", "density")
```

Keep `"hist"` first so the existing default does not change.

Update the generic documentation, all three S3 methods, examples, internal
implementation, and review comments that currently describe only two modes.

### Add `adjust`

Add an explicit argument to all methods and `.logist_plot_impl()`:

```r
adjust = 1
```

It is used only by `marginal = "density"` and is passed to
`stats::density(adjust = adjust)`. Validate it in the density branch as one
finite numeric value greater than zero.

Keep `adjust` out of `marginal.args`. It changes the density estimate rather
than merely styling the drawing layer, and Michael specifically identified it
as an important user-facing control.

`bins` remains specific to histogram mode and has no effect on density mode.

### Add a convenience wrapper

For symmetry with `logist_hist()` and `logist_point()`, add:

```r
logist_density <- function(...) {
  logist_plot(..., marginal = "density")
}
```

It should preserve all three calling conventions, including named formula
dispatch, by retaining the existing wrapper pattern `function(...)`.

### Color and layer customization

Retain the existing customization contract:

- `marginal.color` supplies the default density fill;
- `marginal.args` customizes the density ribbon layers and overrides defaults;
- `fit.color` and `fit.args` continue to control the fitted curve;
- plot-level customization continues through normal ggplot additions.

Do not add a separate `density.args` argument.

## Density computation

After the existing common input filtering and `.to_binary01()` conversion,
split the numeric predictor by the canonical response values 0 and 1.

Require at least two observations in each response group. If density estimation
still fails, catch the `stats::density()` error and report which response group
could not be estimated, retaining the original diagnostic in the message.

Calculate both estimates over the same visible predictor range:

```r
densities <- lapply(c(0, 1), function(lev) {
  stats::density(
    data$x[data$y == lev],
    from = min_x,
    to = max_x,
    adjust = adjust
  )
})
```

Using a common `from`/`to` range gives both layers aligned x coordinates and
keeps their generated geometry within the plot panel. Keep `stats::density()`'s
default grid size and kernel unless a demonstrated need for additional public
controls arises.

Validate that every generated `x` and density value is finite and that the
overall maximum density is finite and strictly positive.

## Shared height and coordinate mapping

Use a shared target height for the two densities:

```r
max_density <- max(vapply(densities, function(z) max(z$y), numeric(1)))
density_height <- 0.15
density_headroom <- max_density / density_height
```

This makes the tallest density peak occupy 15% of the probability panel on one
side. Because both groups share one `density_headroom`, the smaller peak retains
its height relative to the larger one; do not independently normalize each
group to 15%.

Transform the estimated values as follows:

```r
density_y0 <- data.frame(
  x = densities[[1]]$x,
  ymin = 0,
  ymax = densities[[1]]$y / density_headroom
)

density_y1 <- data.frame(
  x = densities[[2]]$x,
  ymin = 1 - densities[[2]]$y / density_headroom,
  ymax = 1
)
```

The first ribbon grows upward from 0. The second ribbon hangs downward from 1.
Neither can extend more than 0.15 into the panel.

Do not add a secondary density axis. The marginal shapes are intended to show
where observations supporting the logistic fit are concentrated; their raw
density units are not directly comparable to probabilities, and the headroom
transformation is a display mapping.

## Drawing layers

Create two `geom_ribbon()` layers with explicit data and mappings:

```r
ggplot2::geom_ribbon(
  data = density_y0,
  mapping = ggplot2::aes(x = .data$x, ymin = .data$ymin, ymax = .data$ymax),
  inherit.aes = FALSE,
  fill = marginal.color,
  colour = NA,
  alpha = 0.67,
  outline.type = "upper"
)
```

Use the equivalent layer for `density_y1`, with `outline.type = "lower"`.
These defaults mean that when a user supplies an outline color, the visible
density curves are outlined rather than the y=0/y=1 baselines. A user-supplied
`outline.type` in `marginal.args` may override this behavior for both ribbons.

Apply the same validated `marginal.args` to both density layers. User values
override the defaults established by `marginal.color`, `colour = NA`, `alpha`, and
`outline.type`.

Allow the documented ribbon styling controls needed for Michael's proposed
shading and line variations, including:

- `fill`
- `colour`/`color`
- `alpha`
- `linetype`
- `linewidth`
- `lineend`
- `linejoin`
- `outline.type`
- `na.rm`
- `show.legend`

Protect `data`, `mapping`, `stat`, `position`, `orientation`, and
`inherit.aes`. Reject `adjust` in `marginal.args` with a message directing the
user to the top-level `adjust` argument. Continue to reject unnamed,
duplicated, unknown, or conflicting arguments through the existing layer-list
validation.

The density branch's layer order should be:

1. the y=0 density ribbon;
2. the y=1 density ribbon;
3. the fitted curve and confidence band.

Drawing the fit last matches histogram mode and ensures it remains visible over
the filled marginal shapes.

## Probability scale

Use the histogram branch's primary probability scale without its count axis:

```r
ggplot2::scale_y_continuous(
  limits = c(0, 1),
  breaks = seq(0, 1, by = 0.2),
  expand = ggplot2::expansion(mult = 0)
)
```

Retain `coord_cartesian(xlim = c(min_x, max_x))`. This keeps both ribbons
anchored exactly at 0 and 1 and preserves the same visible x range as the other
marginal modes.

## Implementation structure

1. Extend all `marginal` choices while preserving `"hist"` as the default.
2. Add and forward `adjust = 1` through the three S3 methods.
3. Add `logist_density()`.
4. Split the current `if (marginal == "points") ... else ...` flow into three
   explicit branches so density cannot accidentally use histogram validation or
   geometry.
5. Extend `marginal.args` validation with a density-specific allowlist and
   protected list.
6. Add a small `.check_adjust()` helper or equivalent density-branch validation.
7. Compute the two densities and shared height transformation.
8. Build the two probability-coordinate ribbon data frames and layers.
9. Add the fitted layer last, followed by the probability scale and x
   coordinate.
10. Update roxygen documentation, examples, review notes, and references to
    "both" marginal modes.

Keep the density code close to the histogram branch: both precompute a marginal
distribution, map it into probability coordinates, and place the fit above the
marginal layers.

## Tests

### Interfaces and rendering

Test `marginal = "density"` through:

- vector input;
- two-column data-frame input;
- formula input;
- `logist_density()` with positional and named `formula=` calls.

For each relevant case, force both `ggplot_build()` and `ggplotGrob()` so lazy
statistical or graphical failures are detected.

### Response handling

Repeat the existing numeric, logical, factor, and character response tests in
density mode. Confirm all encodings produce the same canonical event direction
and rendered ribbon orientation.

### Geometry

Verify that:

- both estimates use the same visible x range;
- the y=0 ribbon has `ymin == 0` and grows upward;
- the y=1 ribbon has `ymax == 1` and grows downward;
- no density geometry extends outside `[0, 1]`;
- the tallest density reaches exactly 0.15 panel units, within numerical
  tolerance;
- the other density retains its relative peak height;
- the fitted layer follows both density layers;
- density mode has no secondary count axis.

### `adjust` and error handling

Verify that:

- `adjust = 1` reproduces the direct `stats::density()` calculation;
- a different valid `adjust` visibly changes the density geometry;
- zero, negative, non-finite, missing, non-numeric, or non-scalar `adjust`
  values produce clear errors;
- fewer than two observations in either response group produces a targeted
  density-estimation error;
- any non-finite or degenerate density result is rejected clearly;
- `bins` is not used or validated in density mode;
- `adjust` supplied inside `marginal.args` is rejected with guidance.

### Customization

Verify that:

- `marginal.color` sets both density fills;
- `marginal.args` overrides fill, outline color, alpha, linewidth, linetype, and
  outline type on both ribbons;
- `color` is normalized to `colour` consistently;
- protected and unsupported density-layer arguments are rejected;
- `fit.args` continues to customize the fitted layer independently;
- post-hoc `labs()` and `theme()` changes remain effective.

### Regression and visual comparison

Confirm that adding the third branch does not change the built data or rendered
defaults for `marginal = "hist"` and `marginal = "points"`.

Create a native-layer Donner density image and compare it with
`loghist-density-cowplot.png`. Check the mirrored directions, relative peak
heights, 15% maximum height, fill appearance, curve visibility, and clipping.
Small rasterization differences are acceptable; the distributional shapes and
coordinate mapping should agree.

## Documentation examples

Include a basic density example, bandwidth adjustment, layer customization, and
ordinary ggplot composition:

```r
logist_plot(survived ~ age, data = Donner, marginal = "density")

logist_density(
  survived ~ age,
  data = Donner,
  adjust = 1.25,
  marginal.args = list(
    fill = "grey70",
    colour = "black",
    linewidth = 0.4,
    alpha = 0.7
  )
) +
  ggplot2::labs(title = "Survival of the Donner Party") +
  ggplot2::theme_minimal()
```

## Deferred grouping extension

Do not combine density mode with a new `group=` interface in this change.
Michael's proposed multiple shaded/linetyped density stripes are promising, but
they require decisions about group input across all three S3 interfaces,
palette/legend behavior, vertical allocation, and whether densities should be
overlaid, stacked, or placed in separate stripes.

The native ribbon representation keeps that future work possible: an eventual
grouping implementation can add a group identifier to the precomputed density
data and allocate additional ribbon bands without returning to cowplot or
ggdist.

## Expected outcome

`marginal = "density"` will be a smooth, filled counterpart to the mirrored
histograms, with Michael's requested `adjust` control and styling through
`marginal.args`. It will remain a native ggplot, introduce no new package
dependency, preserve the current histogram default, and leave grouped density
displays as a deliberate follow-up feature.

## Post-hoc changes

### Reduce the density height to 15% per side

After visually reviewing the native ribbon implementation, reduce its maximum
height from the cowplot prototype's 25% per side to 15% per side. Filled density
ribbons occupy continuous horizontal space and therefore appear visually
heavier than histogram bars at the same maximum height. A 15% cap keeps the
density shapes legible while leaving more of the central probability region for
the fitted curve and confidence band.

Histogram mode retains its existing 25% cap. The density implementation uses:

```r
density_height <- 0.15
density_headroom <- max_density / density_height
```

This controls maximum vertical extent, not literal filled plot area. The two
response groups continue to share one headroom value, so their relative peak
heights are preserved. Keep this as an internal density default for now; if
user control is later justified, expose a semantic argument such as
`marginal.height` rather than the headroom multiplier.
