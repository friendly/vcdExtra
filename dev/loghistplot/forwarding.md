# Plan: forwarding graphical arguments in `logist_plot()`

## Decision

Keep `...` for S3 dispatch and convenience-wrapper forwarding. After the selected
method has consumed its arguments, require `...` to be empty so misspelled or
unsupported arguments produce an error.

Do not forward a flat `...` to ggplot2 layers. Instead, add two explicitly scoped
argument lists:

```r
fit.args = list()
marginal.args = list()
```

- `fit.args` controls the fitted logistic curve and confidence band.
- `marginal.args` controls the active marginal drawing layer selected by
  `marginal`.
- Plot-level customization continues to use ordinary ggplot additions such as
  `+ labs()` and `+ theme()`.

Prefer `fit.args` to `smooth.args`: it describes the plot feature rather than
committing the public interface to the current `geom_smooth()` implementation.

Do not add separate `hist.args`, `point.args`, or future `density.args`
arguments. Only one marginal representation is active in a call, and adding a
new argument list for every representation would make the interface grow
unnecessarily.

## Retain the color convenience arguments

Keep the existing arguments:

```r
fit.color = "steelblue"
marginal.color = "orange"
```

They cover the most common customization without requiring users to construct
argument lists. Retain the existing American spelling for consistency with the
current interface.

Their semantic defaults are:

- `fit.color` supplies both the fitted line color and confidence-band fill.
- With `marginal = "points"`, `marginal.color` supplies the point color.
- With `marginal = "hist"`, `marginal.color` supplies the rectangle fill.
- With `marginal = "density"`, `marginal.color` supplies the density fill.

The argument lists allow these coupled defaults to be separated when desired.
For example, `fit.args = list(fill = "lightblue")` changes the confidence-band
fill without changing the fitted line.

## Precedence

Merge graphical arguments in this order:

1. Internal layer defaults.
2. `fit.color` or `marginal.color`, as applicable.
3. User values in `fit.args` or `marginal.args`.

Later values override earlier values. Thus the explicitly scoped argument lists
have final precedence over the convenience color arguments and other graphical
defaults.

For example:

```r
logist_plot(
  survived ~ age,
  data = Donner,
  marginal = "points",
  fit.color = "navy",
  fit.args = list(fill = "lightblue", linewidth = 2),
  marginal.args = list(shape = 21, size = 2, alpha = 0.6)
)
```

## Layer routing

### Fitted curve

Merge `fit.args` into the arguments used to construct the fitted-curve layer.
The function must continue to control the statistical definition of the fit:

```r
method = "glm"
formula = y ~ x
method.args = list(family = "binomial")
```

Initially, treat those structural arguments as protected rather than allowing
`fit.args` to turn the layer into a different model. Graphical arguments such as
`colour`, `fill`, `linewidth`, `linetype`, `alpha`, and `se` may be customized.

### Marginal points

For `marginal = "points"`, merge `marginal.args` into the arguments used to
construct `geom_point()`. The current color, alpha, and jitter position are
defaults that the user may override, subject to the protected-argument rules
below.

### Marginal histograms

For `marginal = "hist"`, merge `marginal.args` into the internal `geom_rect()`
layer. Version 3 calculates histogram counts and rectangle coordinates itself;
it does not use `geom_histogram()`.

Consequently, `marginal.args` in histogram mode customizes the rendered
rectangles, for example:

```r
marginal.args = list(
  fill = "grey70",
  colour = "black",
  linewidth = 0.2,
  alpha = 0.8
)
```

Histogram computation remains controlled by explicit function arguments such
as `bins`. Do not accept `binwidth`, `boundary`, or `closed` as graphical
passthrough arguments unless they are implemented as first-class computation
options and kept synchronized with the rectangle coordinates and secondary
count axis.

### Marginal densities

For `marginal = "density"`, route `marginal.args` to the density
drawing layer actually used by the implementation. Do not promise specifically
that they are `geom_density()` arguments unless density mode genuinely uses
`geom_density()`.

Important statistical controls such as `adjust` should be considered for an
explicit top-level argument. This improves discoverability and avoids hiding
computation inside a nominally graphical argument list.

## Validation and protected arguments

Validate both argument lists before constructing any layers:

- Each must be a list.
- All entries must be named.
- Duplicate names must produce an error.
- Unknown or unsupported arguments should produce an error rather than be
  silently ignored.
- Normalize the `color` alias to `colour` before merging. Error if both forms
  are supplied with competing values.

Protect arguments that define the function's data flow and required geometry.
At minimum, do not allow either list to replace:

- `data`
- `mapping`
- `inherit.aes`
- `stat`

Also protect `method`, `formula`, and `method.args` in `fit.args`. Decide
explicitly whether `position` may be overridden for each marginal mode. It is a
useful customization for points, but changing it for precomputed histogram
rectangles could invalidate the design.

Validation should be specific to the active layer. An argument valid for
points need not be valid for histograms, but the resulting error should name
`marginal.args`, the selected marginal mode, and the unsupported entry.

## Implementation outline

1. Add `fit.args = list()` and `marginal.args = list()` to all three public S3
   methods and to `.logist_plot_impl()`.
2. Forward both lists explicitly from each method into the shared
   implementation.
3. Leave the generic and convenience wrappers using `...` for dispatch and call
   forwarding.
4. Retain `rlang::check_dots_empty()` in `.logist_plot_impl()`.
5. Add an internal helper that validates a layer-argument list, normalizes
   aliases, rejects protected names, and merges user values over defaults.
6. Construct layers from the merged argument lists, avoiding duplicate formal
   arguments. A controlled `do.call()` or `rlang::exec()` call is preferable to
   placing `...` beside hard-coded layer arguments.
7. Preserve the current layer order: rectangles before the fitted curve in
   histogram mode, and the fitted curve before points in point mode.
8. Update roxygen documentation with the routing, precedence, and
   mode-dependent validity rules.

## Tests

Add tests covering:

- default output is unchanged when both argument lists are empty;
- `fit.color` and `marginal.color` retain their current behavior;
- `fit.args` overrides fitted-layer defaults and `fit.color`;
- `marginal.args` overrides marginal-layer defaults and `marginal.color`;
- fitted line color and confidence-band fill can be controlled independently;
- point shape, size, alpha, and jitter customization;
- histogram fill, outline color, linewidth, linetype, and alpha customization;
- protected arguments are rejected with clear errors;
- unnamed, duplicated, or non-list inputs are rejected;
- unsupported arguments are rejected for the selected marginal mode;
- `color`/`colour` alias handling is deterministic;
- misspelled ordinary arguments left in `...` still fail through
  `check_dots_empty()`;
- all marginal modes still build and render successfully after customization;
- histogram rectangle coordinates and the secondary count axis remain
  unchanged by purely graphical customization.

Force rendering with `ggplot2::ggplotGrob()` or `ggplot2::ggplot_build()` in
addition to checking the constructed objects, because some ggplot2 argument
errors and warnings are lazy.

## Documentation examples

Include one simple convenience example and one advanced example:

```r
# common customization
logist_plot(survived ~ age, data = Donner,
            fit.color = "navy", marginal.color = "goldenrod")

# layer-specific customization
logist_plot(
  survived ~ age,
  data = Donner,
  marginal = "hist",
  fit.args = list(linewidth = 2, fill = "lightblue"),
  marginal.args = list(colour = "black", linewidth = 0.2, alpha = 0.8)
) +
  ggplot2::labs(title = "Survival of the Donner Party") +
  ggplot2::theme_minimal()
```

This separates three levels of customization cleanly: color conveniences for
common use, scoped argument lists for layer details, and native ggplot additions
for the overall plot.
