# Plan: render histogram marginals as rectangles in one ggplot

## Objective

Change the `marginal = "hist"` implementation of `logist_plot()` so that it
returns a normal, single ggplot object. This will make ordinary post-hoc ggplot
additions work as users expect, including:

```r
logist_plot(survived ~ age, data = Donner, marginal = "hist") +
  ggplot2::labs(title = "test") +
  ggplot2::theme(plot.title = ggplot2::element_text(face = "bold"))
```

The visual design should remain unchanged: the histogram for response value 0
grows upward from probability 0, the histogram for response value 1 hangs
downward from probability 1, and the fitted logistic curve is drawn over both.

## Why the current implementation needs to change

Histogram mode currently builds three independent ggplots (`p_main`,
`p_hist_y0`, and `p_hist_y1`) and combines them with `cowplot::ggdraw()` and
`cowplot::draw_plot()`. The component plots are converted to grobs during that
composition. A later `+ labs()` or `+ theme()` call modifies only the outer
ggdraw canvas and cannot update the already-captured component plots.

Rather than introduce a custom plot class and custom `+` method, draw the
histogram bars directly in the probability coordinate system of `p_main`.
The returned object will then retain standard ggplot behavior for printing,
`ggsave()`, `labs()`, `theme()`, and other compatible ggplot additions.

## Implementation steps

### 1. Separate the shared plot foundation from its data layers

Refactor the current construction of `p_main` into a shared base plot and a
reusable fitted-curve layer (or an equivalent helper). This is needed to control
layer order in both marginal modes.

The desired layer order is:

- `marginal = "hist"`: histogram rectangles, then fitted curve and confidence
  band;
- `marginal = "points"`: fitted curve and confidence band, then jittered points,
  preserving the current order.

Avoid adding rectangles to the current `p_main` after `geom_smooth()`, because
that would paint the histogram bars over the curve and confidence band. Also
avoid modifying `p_main$layers` directly; constructing the layers in the desired
order uses the public ggplot2 API.

### 2. Keep the existing bin validation and count calculations

Retain the current logic for:

- validating `bins`;
- calculating `bin_width` and `hist_breaks`;
- detecting non-finite, non-positive, or duplicated breaks;
- calculating `hist_counts` for response values 0 and 1;
- calculating `max_count`, `bin_no`, `count_ticks`, `count_positions`, and
  `count_labels`.

In particular, keep:

```r
bin_no <- 4 * max_count
```

This maps the tallest histogram bar to one quarter of the probability panel,
matching the existing display.

### 3. Convert the histogram counts to rectangle coordinates

Create a data frame with one row per bin per response value. With the default
30 bins it will contain 60 rows, and `geom_rect()` will draw one rectangle for
each row.

```r
hist_data <- rbind(
  data.frame(
    xmin = head(hist_breaks, -1L),
    xmax = tail(hist_breaks, -1L),
    ymin = 0,
    ymax = hist_counts[[1L]] / bin_no
  ),
  data.frame(
    xmin = head(hist_breaks, -1L),
    xmax = tail(hist_breaks, -1L),
    ymin = 1 - hist_counts[[2L]] / bin_no,
    ymax = 1
  )
)
```

The first block maps count `n` to the interval `[0, n / bin_no]`. The second
maps it to `[1 - n / bin_no, 1]`. These are the same transformations currently
used by the two overlaid histogram plots.

Zero-count rows may be retained. They produce zero-height rectangles and make
the correspondence between `hist_breaks` and the two count vectors explicit.

### 4. Draw the rectangles on the main plot

Add the histogram data with a layer such as:

```r
ggplot2::geom_rect(
  data = hist_data,
  mapping = ggplot2::aes(
    xmin = .data$xmin,
    xmax = .data$xmax,
    ymin = .data$ymin,
    ymax = .data$ymax
  ),
  inherit.aes = FALSE,
  fill = marg.color,
  alpha = 0.67
)
```

`inherit.aes = FALSE` is important: the rectangle data does not contain the
main plot's `x` and `y` columns. The four mapped columns are vectors, but ggplot2
evaluates them row by row and draws one rectangle per row.

Add the fitted curve after this layer so the curve and confidence band remain
visible over the bars.

### 5. Preserve the probability and count axes

Keep the existing primary y scale and duplicated secondary count axis:

```r
ggplot2::scale_y_continuous(
  limits = c(0, 1),
  breaks = seq(0, 1, by = 0.2),
  expand = ggplot2::expansion(mult = 0),
  sec.axis = ggplot2::dup_axis(
    breaks = count_positions,
    labels = count_labels,
    name = "Count"
  )
)
```

The rectangle transformations and secondary-axis transformations both use
`bin_no`, so the labels will continue to correspond to the bar heights.

Keep the existing x limits through `coord_cartesian()`.

### 6. Remove the histogram-only compositing code

Once the rectangle version is verified, remove from histogram mode:

- the local `marginal_hist()` function;
- `p_hist_y0` and `p_hist_y1`;
- the `cowplot::ggdraw()`/`cowplot::draw_plot()` return expression;
- themes and labels that existed only to make the two overlaid histogram plots
  transparent.

Return the native ggplot expression directly.

After integration into the package, check whether `cowplot` is used anywhere
else in production code. If it is not, remove it from the package dependencies.
Do not remove it solely on the basis of files under `dev/`, which may retain
historical experiments using cowplot.

### 7. Update documentation and development notes

Document that both marginal modes return ordinary ggplot objects and may be
extended with standard additions such as `labs()` and `theme()`.

Replace historical comments that describe cowplot as necessary for histogram
mode. Preserve any useful history in a development note rather than in the
eventual production R source.

Add an example demonstrating post-hoc customization in histogram mode.

## Verification plan

### Behavioral tests

For both `marginal = "hist"` and `marginal = "points"`:

- confirm the return value inherits from `ggplot`;
- force rendering with `ggplot2::ggplotGrob()` rather than testing construction
  alone;
- confirm `labs(title = ...)` produces a title grob;
- confirm `labs(x = ..., y = ...)` changes the rendered axis titles;
- confirm an incremental `theme()` call changes the requested theme element;
- confirm a complete theme, such as `theme_minimal()`, is applied normally;
- confirm the plot can be saved with `ggplot2::ggsave()`.

### Histogram geometry tests

- the rectangle data has `2 * bins` rows;
- the response-0 rectangles have `ymin == 0` and grow upward;
- the response-1 rectangles have `ymax == 1` and grow downward;
- no rectangle extends outside `[0, 1]`;
- the tallest rectangle in each direction is no taller than `0.25`;
- bin edges and counts agree with `graphics::hist(..., right = FALSE,
  include.lowest = TRUE, plot = FALSE)`;
- the fitted-curve layer follows the rectangle layer in histogram mode.

### Regression inputs

Exercise at least:

- the Donner formula example;
- vector and data-frame calling conventions;
- numeric, logical, factor, and character binary responses after their existing
  conversion to 0/1;
- sparse histograms where `max_count == 1`;
- multiple valid values of `bins`;
- missing/non-finite predictor filtering;
- the existing invalid-bin and degenerate-range error cases.

### Visual regression

Create a reference image for the Donner histogram example and compare the new
single-plot rendering with the current cowplot rendering. Check:

- bar direction and height;
- bin alignment at the minimum and maximum x values;
- curve and confidence-band visibility;
- primary probability labels and secondary count labels;
- clipping at y = 0 and y = 1;
- title, subtitle, caption, and axis-label placement after `labs()`;
- theme changes to the panel, grid, text, and plot background.

Small rasterization differences are acceptable, but the statistical content
and coordinate mapping should be unchanged.

## Expected outcome

Histogram mode will no longer return a drawing canvas containing frozen plot
grobs. It will return the same kind of native ggplot object as points mode, so
normal ggplot composition works without a special wrapper class or new public
customization arguments.

One limitation should be documented or tested explicitly: replacing the y scale
after construction may invalidate the secondary count-axis mapping. Adding
labels and themes does not replace that scale and is fully supported.
