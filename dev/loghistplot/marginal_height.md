✅  [**GK: DONE**]

# Follow-up: controlling the vertical space given to marginal displays

Implemented in `logist-plot.R` (canonical v3). Follows on from `MF-Claude.md`
(the `...`-forwarding thread).

## The gap

There was no way to control how much of the 0--1 probability panel the
marginal display (histogram bars, density curve/lanes) was allowed to occupy.
It was baked in as a hardcoded constant, and it differed by mode:

| Mode | Canonical default | Meaning |
|---|---|---|
| `hist` | `bin_no <- 4 * max_count` | tallest bar reaches 1/4 (25%) of the panel |
| `density`, ungrouped | `density_height <- 0.15` | density curve capped at 15% of the panel, each side |
| `density`, grouped | `density_lane_height <- 0.05` | each group's lane is 5% of the original 0--1 panel height |

**`points` mode is not actually part of this gap** -- jitter amount is
already reachable today via the existing `marginal.args` mechanism, since
`position` is in that mode's `allowed` list in `.check_layer_args()`:

```r
logist_plot(survived ~ age, data = Donner, marginal = "points",
            marginal.args = list(position = ggplot2::position_jitter(h = 0.05)))
```

So this note is only about `hist` and `density` (both ungrouped and grouped).

## Why this isn't already covered by `marginal.args`

`marginal.args` (per `implemented-plans/forwarding.md`) only routes
*graphical* arguments into the constructed `geom_rect()`/`geom_ribbon()`
call -- colour, alpha, linewidth, and so on. The height cap is not a layer
argument at all; it's used to compute the `y`/`ymin`/`ymax` values of the
data *before* any layer is built (e.g. `hist_data$ymax <- hist_counts[[1]] /
bin_no`). This is the same reason `adjust` was pulled out to its own
top-level argument instead of living in `marginal.args` -- it's a
computation input, not a rendering detail, and `.logist_plot_impl()`
already errors if `adjust` is supplied inside `marginal.args` for exactly
this reason. A height argument should get the same treatment.

## Implemented argument

The implementation adds one new top-level argument, `marginal.height = NULL`, to
`logist_plot()`'s three public methods and to `.logist_plot_impl()`, with
`NULL` meaning "use the current per-mode default" so nothing changes for
existing callers:

```r
marginal.height = NULL   # default: 0.25 (hist), 0.15 (density, ungrouped),
                          # 0.05 (density, grouped)
```

For histograms and ungrouped densities, the value is the maximum fraction of
the fixed 0--1 panel that the marginal display may occupy on one side. For
grouped density it is the full lane height in the original probability-coordinate
units. It replaces `4 * max_count` in `bin_no`, `density_height`, and
`density_lane_height` directly (as `1 / marginal.height` where the code
previously divided by `bin_no`, or as a direct substitution for
`density_height`/`density_lane_height`).

One argument, not one per mode (`hist.height=`/`density.height=`/
`lane.height=`), for the same reason `forwarding.md` rejected per-mode
`*.args` names: only one marginal mode is active per call, so the meaning
of `marginal.height` is unambiguous from context, and it keeps the
already-sizeable argument list from growing by three instead of one.

## Resolved implementation decisions

1. **Validation bound differs by mode, and needs to be mode-aware.** In
   `hist` and ungrouped `density`, the display is clipped inside the fixed
   `[0, 1]` y-range on each side, so `marginal.height` above ~0.5 would
   visually collide with (or invert past) the opposite side -- worth an
   explicit upper bound there. The implementation accepts values through 0.5.
   Grouped density instead *expands* `density_limits` outward
   beyond `[0, 1]` to fit each lane (see `density_limits <-
   c(-length(group.levels) * density_lane_height - ..., 1 +
   length(group.levels) * density_lane_height + ...)`), so it isn't
   bounded by the panel the same way -- but an unreasonably large value
   there would still produce a degenerate, mostly-empty plot, so some
   sanity ceiling is still worth having (just a looser one). The implementation
   uses 1 for grouped density lanes.

2. **Should `density_lane_gap`/`density_outline_padding` scale with a
   custom `marginal.height`, or stay fixed?** v3 computed both as fixed
   *proportions* of `density_lane_height` (`gap <- height * 0.2`, `padding
   <- height * 0.15`), so they'd naturally scale if `density_lane_height`
   became configurable. v4 hardcoded both as fixed *absolute* values
   (`0.01`, `0.0075`) independent of its smaller `density_lane_height` --
   which was a deliberate choice for the thin-strip look (see its comment:
   "Keep the inter-lane gap and outer stroke padding at their version-3
   absolute sizes so the narrow strips remain clearly separated"), but it
   means whichever of v3/v4 is kept, making `density_lane_height`
   user-configurable needs an explicit answer for what happens to
   `density_lane_gap`/`density_outline_padding` at the extremes -- a much
   larger custom lane height with v4's fixed absolute gap would look
   fine, but a much *smaller* one could make the gap swamp the strip
   entirely. Recommend deriving both from whichever `marginal.height` is
   in effect (proportionally, like v3 did) rather than hardcoding either,
   so the visual proportions hold regardless of the value chosen -- unless
   there's a reason to keep them independently fixed that isn't captured
   here. The implementation scales the gap using the canonical v3 proportion,
   `gap <- height * 0.2`. Padding scales as `height * 0.15` but retains the
   canonical `0.0075` as a minimum, because the outline stroke has a fixed
   physical width and can otherwise intersect the panel border for very thin lanes.

## Implementation

1. Add `marginal.height = NULL` to `logist_plot.default/.data.frame/
   .formula` and `.logist_plot_impl()`, forwarded the same way `adjust` is.
2. Add a `.check_marginal_height()` validator (single positive finite
   numeric; mode-aware upper bound per point 1 above) mirroring the
   existing `.check_bins()`/`.check_adjust()` pattern.
3. Resolve the actual value used per mode near the top of each branch:
   `height <- marginal.height %||% <mode default>`, then use it wherever
   `4 * max_count` / `0.15` / `0.05` currently appear.
4. Keep the existing `adjust`-in-`marginal.args` rejection as the template
   for rejecting `marginal.height`-shaped keys if anyone tries to pass
   something like `height`/`lane_height` inside `marginal.args` instead.
5. Update roxygen: new `@param marginal.height`, and a note in the
   `@details` "Comparison"-style block (or wherever `bins=`/`adjust=` are
   currently documented) that this is a computation input, not a graphical
   one, same framing as `adjust`.

## Tests

Matching the style already used in `forwarding.md`/`grouping.md`:

- default output (bar/curve/lane extents) is unchanged when
  `marginal.height = NULL`;
- a custom value visibly changes the rendered `ymax`/`ymin` extents (and,
  for `hist`, the secondary count-axis tick positions, since those are
  also derived from `bin_no`);
- histogram count ticks remain symmetrically positioned at their exact count
  values; when a small height would put adjacent labels less than 0.05 panel
  units apart, ticks are thinned evenly, with overlap-aware guide rendering as
  a final safeguard;
- out-of-range values (`<= 0`, non-finite, too large for the active mode)
  are rejected with a clear error naming `marginal.height`;
- grouped density: confirm `density_limits`, `density_lane_gap`, and
  `density_outline_padding` all track a custom `density_lane_height`
  consistently (whichever answer point 2 above lands on);
- `marginal.height` supplied for `marginal = "points"` is ignored with an
  explicit warning directing the user to control vertical jitter through
  `marginal.args`, for example with
  `ggplot2::position_jitter(h = <desired height>)`. This is intentionally
  more informative than silently ignoring the new argument, while remaining
  less disruptive than an error for calls that share arguments across modes.
