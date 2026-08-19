# Plan: add grouping to `logist_plot()`

## Objective

Add an optional grouping variable so `logist_plot()` can display separate
logistic fits and marginal distributions for two or more groups. The initial
grouping feature will support:

- `marginal = "points"`, using colour to distinguish groups;
- `marginal = "density"`, using colour plus separate stacked density lanes;
- no grouped form of `marginal = "hist"`.

The grouped density design is inspired by the compact orange strips in
`loghist-ggdist-naive.png`, but it will use the native, precomputed density
geometry already established in `logist-plot.R` (canonical v3). The reference image's slabs
rendered almost flat because the ggdist thickness calculation did not work, so
its literal thickness is not a usable measurement. Treat it as a visual target
for narrow density strips rather than as geometry to reproduce exactly.

## Scope and non-goals

Grouping means conditioning both the fitted model and the marginal display on
one discrete variable. It does not mean faceting, adding predictors to one
multivariable model, or fitting an interaction model. Each group receives its
own binomial GLM of `y` on `x`, matching ggplot2's normal grouped
`geom_smooth()` behavior.

Do not add `y ~ x | group` formula syntax. Keep grouping explicit through a
`group =` argument; reserve formula conditioning syntax for a possible future
faceting interface.

Do not introduce cowplot, ggdist, patchwork, or another plotting dependency.
All supported modes must continue to return a native ggplot object.

## Public interface

### Add `group = NULL`

Add `group = NULL` to all three S3 methods and to `.logist_plot_impl()`.
The meaning of a non-`NULL` value depends on the calling convention:

- In `logist_plot.default(x, y, group = ...)`, `group` is a vector with the
  same length as `x` and `y`.
- In `logist_plot.data.frame(x, group = ...)`, `group` is a column selector,
  using the same single-name or single-position rules as `xvar` and `yvar`.
- In `logist_plot.formula(formula, data, group = ...)`, `group` is a column
  selector in `data`. A character name is the clearest documented form, for
  example `group = "sex"`.

Forward the resolved grouping vector and a derived label to the shared
implementation. Convenience wrappers should inherit grouping automatically:

```r
logist_point(survived ~ age, data = Donner, group = "sex")
logist_density(survived ~ age, data = Donner, group = "sex")
```

Do not add separate `groupvar` or formula-conditioning arguments. A single
`group =` concept is easier to document, while method-specific resolution is
already how `x` and `y` are handled.

### Add `group.colors = NULL`

Add one optional palette argument:

```r
group.colors = NULL
```

When it is `NULL`, use ggplot2's default discrete colour and fill scales. When
supplied, require a character vector with at least one colour per observed
group. Accept either:

- an unnamed vector, applied in the documented group-level order; or
- a named vector containing every observed group label.

Use the same palette for fitted lines, confidence bands, points, density fills,
density outlines, and the combined legend. Add matching manual colour and fill
scales when `group.colors` is supplied.

In grouped mode, `fit.color` and `marginal.color` are inactive because scalar
colours cannot distinguish groups. Document this rather than silently mixing
the scalar defaults with mapped group colours. Similarly, reject `colour` or
`fill` inside `fit.args` or `marginal.args` when grouping is active, with an
error directing the user to `group.colors`. Other valid layer customizations,
such as alpha, linewidth, linetype, shape, and point size, remain available.

### Legend behavior

Derive the legend title from the grouping input:

- the deparsed vector expression for the default method;
- the selected column name for the data-frame and formula methods.

Map both `colour` and `fill` to the same internal group factor, give the two
scales the same name and breaks, and verify that ggplot2 produces one combined
legend rather than separate colour/fill legends. Preserve factor level order;
for non-factors, use a deterministic sorted order so row order cannot change
the palette or density-lane assignment.

## Common group validation

Resolve and validate the grouping input before constructing the plot:

1. Reject matrix, array, data-frame, or list-valued grouping inputs.
2. Require `group` to have the same length as `x` and `y` in the default
   method.
3. Include `group` in the common complete-case filtering so any row missing
   `x`, `y`, or `group` is removed consistently.
4. Convert the remaining values to an internal factor with stable levels while
   retaining user-facing labels.
5. Require at least two observed groups after filtering. A one-level grouping
   variable is an error rather than an alternative route to the existing
   ungrouped display.
6. Require every group to contain both canonical response outcomes, 0 and 1.
   Otherwise its binomial smooth is not a comparable group-specific fit and
   may fail lazily during plot building.
7. Require usable predictor variation within every group. Report the offending
   group directly instead of relying on a later `geom_smooth()` warning.

Keep `group = NULL` on the existing ungrouped path. Its plot data, geometry,
colours, scales, and validation should remain unchanged.

## `marginal = "hist"`

Do not support grouping for mirrored histograms. Stacked or overlapping
group-coloured bars obscure both the marginal distributions and the fitted
curves, while separate histogram panels would turn this into a faceting or plot
composition feature rather than a small extension.

If `group` is non-`NULL` and `marginal = "hist"`, fail early with a targeted
message such as:

```text
Grouping is not supported for `marginal = "hist"`; use
`marginal = "points"` or `marginal = "density"`.
```

Do not silently ignore the grouping variable, stack bars, dodge bars, or fall
back to facets.

## `marginal = "points"`

Add the internal group factor to the base plot data and map both colour and
group:

```r
ggplot2::aes(
  x = .data$x,
  y = .data$y,
  colour = .data$group,
  fill = .data$group,
  group = .data$group
)
```

This lets `geom_smooth(method = "glm", family = "binomial")` fit one curve and
confidence band per group, while `geom_point()` uses the same group colours and
the existing vertical jitter. Retain the current `0--1` coordinate range and
the existing layer order unless visual testing shows that points should be
drawn before the fitted lines.

Do not pre-fit the group models solely to draw them; use ggplot2's grouped
smoother after the common validation has ruled out structurally invalid groups.
Force plot building in tests so actual fitting failures are still detected.

## `marginal = "density"`

### Outward stacked lanes

Grouped densities must sit outside the response-probability interval rather
than cover the fitted curves:

- the lower stack begins at `y = 0` and grows downward;
- the upper stack begins at `y = 1` and grows upward;
- the first group occupies the lane adjacent to 0 or 1;
- each additional group occupies the next lane farther outward.

Use factor level order for lane order, matching the palette and legend order.

Give every group an equal, fixed lane height:

```r
density_lane_height <- 0.05
```

Thus each density stripe is at most 5% of the original `0--1` response-panel
height, giving the compact appearance requested from
`loghist-ggdist-naive.png`. Do not divide one fixed margin among the groups.
With `G` groups, extend the displayed y range to:

```r
c(-G * density_lane_height, 1 + G * density_lane_height)
```

Keep this height as an internal design constant initially. If visual testing
later establishes a real need for public control, add a semantic argument such
as `marginal.height`; do not expose the density headroom calculation itself.

The existing ungrouped density behavior remains separate and unchanged: its
two inward-facing ribbons share one scaling factor and reach at most 15% of the
`0--1` panel. That 15% rule is not reused or divided for grouping.

### Density computation and normalization

For each group, estimate two densities: one for `y = 0` and one for `y = 1`.
Use the shared overall visible x range and the existing top-level `adjust`
argument:

```r
stats::density(group_x, from = min_x, to = max_x, adjust = adjust)
```

Require at least two observations in every group-by-response cell and retain
the existing finite/non-negative density checks. Error messages should name
both the group and original response level.

Normalize each group's response pair together. For group `g`, calculate the
maximum across its `y = 0` and `y = 1` estimates, then map that maximum to one
`density_lane_height`. This preserves the relative peak heights of the two
response-specific densities within a group while ensuring every group's lane
remains visible. Do not normalize the two response densities independently.

The resulting density heights do not encode group sample sizes, and peak
heights should not be compared quantitatively between lanes. Document that the
stripes show the shape and location of each conditional predictor distribution;
the group counts can be conveyed separately if needed.

### Ribbon coordinates

For zero-based group index `i`, define the lower and upper lane baselines as:

```r
lower_baseline <- -i * density_lane_height
upper_baseline <- 1 + i * density_lane_height
```

If `scaled_density` ranges from zero to `density_lane_height`, construct:

```r
# y = 0: outward from 0, then progressively farther downward
ymin <- lower_baseline - scaled_density
ymax <- lower_baseline

# y = 1: outward from 1, then progressively farther upward
ymin <- upper_baseline
ymax <- upper_baseline + scaled_density
```

Build one precomputed ribbon data set containing `x`, `ymin`, `ymax`, `group`,
and `response`, or equivalent separate data sets if that keeps outline handling
clear. Continue using native `geom_ribbon()` layers with
`inherit.aes = FALSE`; do not return to ggdist.

Map ribbon fill and outline colour to the internal group factor. Use an outline
that traces the density edge rather than the lane baseline. Prefer a lighter
default fill alpha than the current ungrouped ribbon if necessary after visual
testing, because multiple opaque strips would appear heavy; the coloured
density outline should remain the primary group cue.

### Probability axis

Extend the continuous y scale to include all density lanes, but show tick marks
and labels only for meaningful response probabilities from 0 to 1:

```r
ggplot2::scale_y_continuous(
  limits = c(-G * density_lane_height,
             1 + G * density_lane_height),
  breaks = seq(0, 1, by = 0.2),
  expand = ggplot2::expansion(mult = 0)
)
```

Do not add labels below 0 or above 1 and do not add a secondary density axis.
The outward lane coordinates are display positions, not probabilities.

Draw all density ribbons first and all fitted curves and confidence bands
afterward. Although the outward ribbons should not overlap the fit region,
keeping the fit last preserves the established layer-order contract.

## Implementation structure

1. Add and document `group = NULL` and `group.colors = NULL` in all public
   methods and the shared implementation.
2. Resolve a group vector and label in each S3 method without adding formula
   conditioning syntax.
3. Extend the internal data frame and complete-case filtering to include group
   when supplied.
4. Add helpers for deterministic group-level creation, grouped-cell
   validation, and optional palette validation.
5. Error immediately for grouped histogram mode.
6. Add mapped colour/fill/group aesthetics to the grouped points and smoother
   layers while preserving the ungrouped construction path.
7. Generalize the density computation to group-by-response cells.
8. Build fixed-height outward ribbon lanes in deterministic group order.
9. Expand only the grouped-density y limits; retain probability-only breaks.
10. Add shared default or manual colour/fill scales and one legend.
11. Update roxygen documentation, examples, comments, and development notes.

Avoid rewriting the ungrouped paths merely to reduce duplication. Separate
grouped and ungrouped branches are acceptable where that makes backward
compatibility and geometry easier to verify.

## Tests

### Calling conventions and wrappers

Test grouping through:

- vector `x`, `y`, and `group` inputs;
- data-frame input with group selected by name and by position;
- formula input with group selected from `data`;
- `logist_point()` and `logist_density()` wrappers;
- positional and named `formula =` dispatch.

Force both `ggplot_build()` and `ggplotGrob()` for successful cases.

### Validation

Verify clear errors for:

- grouped histogram mode;
- mismatched vector lengths;
- missing, nonexistent, ambiguous, or invalid group selectors;
- matrix, data-frame, list, or otherwise unsupported group inputs;
- fewer than two observed groups after filtering;
- groups missing response level 0 or 1;
- inadequate predictor variation within a group;
- fewer than two observations in any density group-by-response cell;
- invalid or incomplete `group.colors` values.

Confirm rows with missing `group` values follow the same removal policy as
missing `x` or `y`, and errors report levels using their original labels.

### Grouped points

Verify that:

- there is one fitted curve and confidence band per group;
- all retained observations appear in the point layer;
- points, fit lines, and confidence bands use consistent group mappings;
- group order and colours do not change when rows are reordered;
- the legend title and labels are correct;
- the y range remains 0 to 1.

### Grouped density geometry

For `G` groups, verify that:

- there are two density shapes per group;
- the lower stack begins at 0 and extends downward;
- the upper stack begins at 1 and extends upward;
- group lanes follow factor and legend order;
- each lane is no taller than 0.05 y-units;
- the larger peak in each response pair reaches exactly 0.05, within numerical
  tolerance;
- the other response density retains its relative height within that group;
- lanes do not overlap;
- y limits are `c(-0.05 * G, 1 + 0.05 * G)`;
- only probability breaks from 0 to 1 are labelled;
- `adjust` changes every relevant estimate as expected;
- the grouped density display has no secondary axis.

### Colours and layer customization

Verify ggplot2's default palette and both named and unnamed `group.colors`.
Confirm one palette is shared by fit, point/density, and legend layers. Confirm
that grouped calls reject colour/fill overrides in `fit.args` and
`marginal.args`, while supported non-colour customizations still work.

### Regression

Compare built layer data for ungrouped points, histograms, and densities before
and after the change. In particular, confirm that ungrouped density ribbons
remain inward-facing with the existing shared 15% maximum height, and that the
default ungrouped colours and lack of a group legend do not change.

Create visual examples with two and three groups. Check that the 5%-high lanes
remain recognizably shaped but visually compact, fitted curves remain dominant,
outward density strips are not clipped, and the legend order matches the lane
order. Revisit the internal 0.05 constant only if these rendered comparisons
show that the reference image's intended stripe size is not achieved.

## Documentation examples

Include one grouped-points example and one grouped-density example:

```r
logist_point(
  survived ~ age,
  data = Donner,
  group = "sex"
)

logist_density(
  survived ~ age,
  data = Donner,
  group = "sex",
  group.colors = c(Female = "#D55E00", Male = "#0072B2"),
  marginal.args = list(alpha = 0.35, linewidth = 0.6),
  fit.args = list(linewidth = 1.2)
)
```

Explain that each colour represents a separately fitted model and its two
conditional predictor distributions. State explicitly that grouped histograms
are unsupported and that grouped density height is a display normalization,
not an encoding of group sample size or response probability.

## Expected outcome

Grouping will add a clean conditional comparison for points and a compact
outward-stripe display for densities without compromising the fitted curves or
the existing ungrouped plots. Histogram grouping will fail clearly instead of
producing a cluttered chart. The implementation will remain a native ggplot,
use one stable colour identity across all grouped layers, and avoid treating the
existing ungrouped 15% density rule as a grouped-layout constraint.
