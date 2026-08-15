# Extending `logist_plot()` to multiple groups (color/group)

Exploratory notes (2026-08-09), prompted by wanting separate fitted curves for two or more
groups within one plot -- e.g. Donner data, separate `survived ~ age` fits for `sex` (Male/
Female). Not implemented. Written to capture what this would take and where the real
difficulty is, before deciding whether it belongs in this iteration.

Verified empirically against the Donner data (see `dev/explore_grouping*.R`-style scratch
scripts, not checked in) rather than reasoned about in the abstract -- results below are from
actually running ggplot2 against `age`/`survived`/`sex`.

## The easy 80%: `marginal = "points"`

Adding `aes(colour = group)` to the existing `ggplot()` call is close to a drop-in change.
ggplot2's own grouping machinery does the rest: one separate `glm(y ~ x, family = binomial)`
fit per group, one ribbon per group, one point color per group, and an automatic legend --
all for free, because this is exactly the mechanism `.to_binary01()` was written to keep
*out* of the `y` aesthetic, now deliberately invoked on a *different* variable where it's
exactly the right tool.

Confirmed with:

```r
ggplot(Donner, aes(x = age, y = survived, colour = sex)) +
  geom_point(alpha = 0.5, position = position_jitter(w = 0, h = 0.02)) +
  geom_smooth(method = "glm", formula = y ~ x, method.args = list(family = "binomial"),
              se = TRUE) +
  coord_cartesian(ylim = c(0, 1))
```

Produces two correctly-separated curves and bands (Female: steeper decline; Male: shallower --
matches `coef(age)` fit separately per sex: Female -0.046, Male -0.026) with a clean legend.
No structural rework needed for this branch -- just thread a new grouping vector through to
`aes(colour = ...)`.

## The hard part: `marginal = "hist"`

The mirrored-histogram design is the obstacle. Right now `marginal_hist()` builds exactly two
small ggplot objects (one per response level), makes their axes/background/border transparent,
and overlays them full-size on top of `p_main` via `cowplot::ggdraw() + draw_plot()`. That
design assumes there are exactly two things to draw and stack directly on top of the main
panel -- it has no natural slot for a third dimension.

Tried the direct extension -- adding `fill = group` to each `geom_histogram()` (stacked bars,
one stack per response-level panel) -- and it works mechanically, but:

- The result is legible as a *standalone* pair of stacked-bar panels, but the actual function
  overlays these transparently on top of the curve panel at full canvas size. Stacked,
  multi-colored bars under two curves (also now colored by group) is a lot going on in one
  chart; the current design's visual clarity (two clean humps flanking one curve) doesn't
  survive the addition well.
- Color is now doing double duty: distinguishing response-level panels was never
  color-coded before (that was `marg.color`, a single scalar) -- now color needs to
  distinguish *group*, in both the histograms and the curves, while `fit.color`/`marg.color`
  as currently designed only support one color each. Either those arguments stop meaning
  what they mean today when grouping is active, or grouped mode needs its own separate
  palette argument (e.g. `group.colors=`, or just hand off to ggplot2's default discrete
  scale and drop `fit.color`/`marg.color` when `group` is supplied).
- A structurally different option -- one full mirrored-histogram set of panels *per group*,
  faceted side by side -- would actually preserve the current visual clarity per facet, but
  is a much bigger rewrite: `cowplot::ggdraw()/draw_plot()` composite full-canvas overlays,
  which don't compose with `facet_wrap()` the way a normal ggplot2 layer would. It would mean
  looping over group levels, building N independent cowplot composites, and arranging *those*
  side by side (e.g. via `cowplot::plot_grid()` or `patchwork::wrap_plots()` at the outer
  level) -- workable, but real new code, not a parameter addition.

## Other things a `group=` argument would touch

- **Where does the grouping variable come from, per calling convention?** For the
  `default`/vector method, presumably a third vector argument (`group=`). For the
  `data.frame` method, presumably a `groupvar=` column selector, matching the `xvar=`/`yvar=`
  pattern already there. For the `formula` method, this is its own design question: does
  `logist_plot(survived ~ age, data = Donner, group = "sex")` work (group named outside the
  formula), or should the formula syntax itself carry it (e.g. `survived ~ age | sex`,
  lattice/`Formula`-package style conditioning)? The latter is more idiomatic for an R
  plotting function but requires either the `Formula` package or hand-rolled parsing --
  `stats::model.frame()` alone doesn't support `|` conditioning syntax.
- **Legend title / labeling**: would want to default to the deparsed/column-name of the
  grouping variable, the same way `xlab`/`ylab` already auto-derive -- consistent, not a new
  problem, just another thing to wire up.
- **What happens with `xvar=`/`yvar=` validation, NA/Inf/constant-x handling, and
  `.to_binary01()`** once a third variable enters the picture -- all of that logic currently
  only reasons about `x` and `y`; a `group` column would need to flow through the same
  `data.frame(x=, y=, group=)` + `complete.cases()` handling, which is mechanical but touches
  most of `.logist_plot_impl()`.

## Recommendation

Split this into two separate, independently-shippable pieces rather than one `group=` feature:

1. **`marginal = "points"` + grouping** -- low risk, already verified to work cleanly with
   ggplot2's built-in mechanism. Could reasonably go into this iteration if wanted.
2. **`marginal = "hist"` + grouping** -- genuinely harder, no clean solution found yet among
   the options above (stacked-in-place loses visual clarity; per-group faceting is a real
   rewrite of the compositing approach). Recommend deferring past this iteration, and treating
   it as its own design discussion (with Gavin) rather than bolting it on now.

If (1) is wanted now: probably still worth explicitly erroring (rather than silently doing
something confusing) if `group=` is supplied together with `marginal = "hist"`, until (2) is
actually designed.

---

# A third `marginal=` option: `"density"`, and whether `ggdist` should replace `cowplot`

Second round of exploratory notes (2026-08-09), same status -- not implemented, verified
empirically against the Donner data.

## `marginal = "density"`: straightforward, same architecture as `"hist"`

Filled density curves instead of bars for the two response-level groups, mirrored the same
way. This turns out to be a close cousin of the existing `"hist"` implementation, not a new
architecture -- same `cowplot::ggdraw() + draw_plot()` compositing, same transparent-axes
trick, same mirroring via `scale_y_reverse()` for the y=1 panel. The one real substitution:
`geom_histogram()` -> `geom_density()`, and the bin-count headroom logic
(`bin_no <- 4 * max_count`) needs a direct analog for density:

```r
dens0 <- density(x[y == 0]); dens1 <- density(x[y == 1])
headroom <- 4 * max(dens0$y, dens1$y)   # same "4x" convention as bin_no
# then scale_y_continuous(limits = c(0, headroom), expand = expansion(mult = 0)) / the
# scale_y_reverse() equivalent for the mirrored panel
```

First attempt without the headroom scaling balloons to fill the entire canvas height (the
panel's own [0, max_density] range gets stretched to the *full* [0,1] canvas by
`cowplot::draw_plot()`, exactly the same failure mode `bin_no` already exists to prevent in
histogram mode -- I hit this directly before adding the headroom calc, good confirmation the
two geoms need the same treatment). With headroom scaling added, the result reads as a clean,
smoother-looking cousin of the existing histogram mode -- arguably nicer for continuous `x`
where age has enough range that a density curve doesn't look as blocky as bars.

Implementation-wise, this is a small, low-risk addition to the existing code: a third
`marginal=` branch, mostly duplicating the existing `"hist"` branch's compositing structure
with `geom_density()` + a `headroom` calc swapped in for `geom_histogram()` + `bin_no`. Worth
noting the `bins=` argument becomes irrelevant for `"density"` (density estimation doesn't
bin) -- would need its own tuning knob if wanted (`adjust=`, matching `stats::density()`'s own
bandwidth-adjustment argument), or just use `density()`'s default bandwidth selection and skip
exposing a knob for now.

## Should `ggdist` replace `cowplot` for the compositing?

`ggdist::stat_slab()` has a `side=` argument (`"top"`/`"bottom"`/`"both"`/etc.) that looks, on
paper, like it directly solves "the trick of inverting the display for the 1 group" --
built-in flip, no manual `scale_y_reverse()` needed. Tested this directly: built one *single*
`ggplot` object with two `stat_slab()` layers (`side = "top"` anchored at `aes(y = 1)`,
`side = "bottom"` anchored at `aes(y = 0)`) plus the `geom_smooth()` curve, all sharing one
axis -- no `cowplot` compositing at all.

Result: the orientation genuinely works out of the box -- the `side = "top"` slab renders
hugging y=1 and pointing the right direction, `side = "bottom"` hugs y=0, no manual reversal
needed. That part of the "trick" really is solved structurally.

But getting the slabs to actually *fill* a visible, proportionate shape against the shared
[0,1] probability axis did not work, in two attempts that took two different paths through
`ggdist`'s API (script: `dev/loghist-extensions-test.R`, section 2):

- **Naive**: `normalize = "none"` + tune `scale=`/`height=` directly. Per
  `?ggdist::stat_slabinterval`, `normalize = "none"` means "values are taken as is with no
  normalization (this should probably only be used with functions whose values are in
  [0,1])" -- our `density()` values are ~0.003-0.02, nowhere near [0,1], so this plots raw,
  tiny magnitudes directly as thickness. Flat result, and in hindsight an easy one to
  explain: wrong tool for values outside [0,1].
- **Documented mechanism**: `ggdist` ships a dedicated scale for exactly this,
  `ggdist::scale_thickness_shared(limits = c(0, K))`, meant to align/control the `thickness`
  aesthetic's output range across layers (the docs specifically note `thickness` is
  per-geom-normalized by default, and this scale is how you override that). Used it as
  documented -- `normalize = "panels"` (each slab's own max -> 1) plus
  `scale_thickness_shared(limits = c(0, 4))` to map that onto a chosen range. **Still
  rendered flat.** This is the more telling result: it's not a case of "used the wrong
  parameter," it's the mechanism the docs point to for this exact problem, applied as
  documented, not producing the expected output.

`ggdist`'s thickness/scale/normalize system is built around a slab expressing its own
distribution's shape along an axis that's *meaningful to the slab itself* (a violin's width,
a halfeye's density) -- not around being anchored at fixed points on an *externally
meaningful* shared axis (here, the probability scale) the way this design needs. Getting that
right may still be possible, but it did not turn out to be the quick win the `side=` parameter
initially suggested, even going through the documented `scale_thickness_shared()` path.

**Bottom line:** implement `"density"` using the same proven `cowplot` architecture already in
place for `"hist"`, not `ggdist`. `ggdist` is still worth keeping in mind for a genuinely
different feature later -- e.g. a richer standalone distributional summary (halfeye,
raincloud) as its own thing, not repurposed to sit at fixed y=0/y=1 anchors on a shared axis --
but that's a different design, not a replacement for the current compositing approach.

## Possible question to file with `ggdist`

Worth raising with the `ggdist` maintainers (github.com/mjskay/ggdist/issues) rather than
concluding this is definitely a dead end -- the fact that the *documented* mechanism for
controlling thickness didn't produce the expected result suggests either a real gap, a
misunderstanding of an interaction that isn't obvious from the docs, or a genuine bug. Draft
question, reprex-ready:

> **Anchoring `stat_slab()` at a fixed position on an externally-meaningful shared axis, with
> a controlled fill height**
>
> I'm trying to draw two `stat_slab()` distribution shapes on a plot where the y-axis is a
> probability scale (0-1) shared with an unrelated `geom_smooth(method = "glm", family =
> binomial)` curve -- one slab's baseline anchored at y=0 growing upward, one anchored at
> y=1 growing downward (`side = "top"`/`"bottom"` handles the direction correctly). What I
> can't figure out is how to make the slab's peak reach a *specific, known height* in y-axis
> data units (e.g. "the slab should reach 0.15 up from its y=0 baseline"), so it reads as a
> proportionate marginal distribution against the shared 0-1 axis rather than either
> vanishing (raw density values are tiny) or overflowing.
>
> Tried `normalize = "none"` + `scale=`/`height=` (expected per-docs to not apply here, since
> raw density values aren't in [0,1] -- confirmed flat, as expected). Then tried the
> documented alignment mechanism, `normalize = "panels"` + `scale_thickness_shared(limits =
> c(0, K))` -- still renders flat/negligible regardless of `K`. Minimal reprex:
>
> ```r
> library(ggplot2)
> set.seed(1)
> d <- data.frame(x = c(rnorm(100, 20, 5), rnorm(100, 40, 8)),
>                  grp = rep(c(0, 1), each = 100))
>
> ggplot(d, aes(x = x)) +
>   ggdist::stat_slab(data = subset(d, grp == 1), aes(y = 1), fill = "orange",
>                      side = "top", normalize = "panels") +
>   ggdist::stat_slab(data = subset(d, grp == 0), aes(y = 0), fill = "orange",
>                      side = "bottom", normalize = "panels") +
>   ggdist::scale_thickness_shared(limits = c(0, 4)) +
>   coord_cartesian(ylim = c(0, 1))
> ```
>
> Is there a documented way to control the output height of a slab in the units of an
> *externally meaningful* shared axis (as opposed to normalizing within/across slabs, which
> is what `normalize=` and `scale_thickness_shared()` both seem to do)? Is this a supported
> use case at all, or is `stat_slab()` fundamentally designed around the slab's own axis being
> the meaningful one?
>
> Versions: R 4.6.1, ggplot2 4.0.3, ggdist 3.3.3.

Not filed yet -- write-up above is ready to go if/when Michael wants to send it.
