# Notes on the `...` forwarding question (MF + Claude, 2026-08-17)

Design notes only -- nothing here is implemented. For Gavin to pick up whenever
convenient.

## The thread so far

GK's question (top of `loghistplot-GK.md`): should `...` in `logist_plot()`
stay silently ignored, error on unrecognized names, or forward into
`theme()`/`geom_point()`/`geom_histogram()` for customization? Motivating
case: `adjust=` for a future `marginal = "density"` mode (also noted in
`loghistplot-extensions.md`).

MF's reply (email): agrees it's tricky because you don't want to feed
arguments to ggplot functions that will "bitch, sometimes bitterly" on
unrecognized names. Suggested looking at how `car::scatterplot()` handles
this -- separate list-valued arguments per plot component, e.g.
`smooth_args = list(...)`, `hist_args = list(...)`, rather than one shared
`...`.

## Current state of the code (loghistplot3.R)

Worth noting since it changes the shape of the decision:

- `rect-plan.md` is done -- `marginal = "hist"` now returns a single native
  ggplot (via `geom_rect()`), same as `marginal = "points"`. Both modes build
  on a shared `p_base`. This matters because it means whichever `*_args=`
  design gets picked applies uniformly to both branches of
  `.logist_plot_impl()`, not one composited-canvas branch and one normal one.
- `...` is currently fully reserved: every public entry point
  (`logist_plot.default/.data.frame/.formula`) accepts and forwards `...`,
  but `.logist_plot_impl()` immediately calls `rlang::check_dots_empty()`.
  So today, passing any extra argument anywhere already errors -- there's no
  silent-ignore behavior to preserve.
- The file already uses the list-args pattern MF suggested, once, for a
  different reason: `fit_layer <- geom_smooth(method.args = list(family =
  "binomial"), ...)`. That's `geom_smooth()`'s own convention for passing
  arguments through to the fitting function it wraps (`glm()`), not
  something `logist_plot()` invented. But it's a useful precedent already
  living in the file for the "named list per sub-component" shape.
- Two geoms already share overlapping hardcoded aesthetics: `fit_layer` sets
  `colour`/`fill`/`alpha`/`linewidth` from `fit.color`; the histogram
  `geom_rect()` sets `fill`/`alpha` from `marg.color`. Any `...` design needs
  to say what happens if a user also asks for `fill=`/`alpha=` through it.

## Options

**A. Blanket-forward `...` to whichever geom `marginal=` selects (GK's
suggestion in the notes).** E.g. `marginal = "points"` forwards `...` to
`geom_point()`. Simple, minimal API surface. Two problems: 

(1) it only covers the *marginal* geom -- `adjust=` for a future density mode is
handled, but there's no slot for customizing the fit curve (`geom_smooth()`)
or, in histogram mode, disambiguating a bare argument meant for `geom_rect()`
from one meant for the smoother, since both are drawn in the same call. 

(2) a single flat `...` can't safely reach two different geoms in histogram
mode at once (rect + smooth) without name collisions -- ggplot2 argument
names like `alpha`, `colour`, `linewidth` are common to many geoms and would
need to go to different layers depending on which one the user meant.

**B. Component-scoped list arguments, as MF suggested.** One list-valued
parameter per drawn component: something like `fit.args = list()` for the
`geom_smooth()` layer and `marg.args = list()` for the marginal-distribution
layer (`geom_rect()`/`geom_point()`/, later, `geom_density()`). Each list is
merged over the current hardcoded defaults with `modifyList()` and expanded
into the geom call with `do.call()` (or `rlang::inject()` /
`!!!` splicing, if the package already leans on rlang elsewhere -- it does,
`rlang::check_dots_empty()`/`.data` are already imported).

- Naming: match the existing dotted style (`fit.color`, `marg.color`)
  rather than snake_case, i.e. `fit.args=`/`marg.args=` over
  `fit_args=`/`hist_args=` -- mixing underscore and dot conventions in the
  same parameter list would look accidental. `marg.args` (not `hist.args`)
  also reads correctly once `marginal = "density"` exists and the same
  parameter is feeding `geom_density()` instead of the histogram rects --
  no new `density.args=` needed, `adjust=` just becomes
  `marg.args = list(adjust = 2)` and the motivating example from GK's email
  is covered for free.
  
- `...` itself stays reserved (`rlang::check_dots_empty()` unchanged), so a
  stray/misspelled *top-level* argument (e.g. someone typing
  `logist_plot(..., adjust = 2)` directly instead of inside `marg.args`)
  still fails loudly instead of vanishing -- this is the "catch typos"
  half of GK's original either/or, satisfied as a side effect rather than a
  separate decision.
  
- Collisions with the hardcoded aesthetics (`fill`, `alpha`, `colour`,
  `linewidth` currently driven by `fit.color`/`marg.color`): treat
  `fit.args`/`marg.args` as authoritative and let them override, same as
  `method.args` already does for `geom_smooth()`'s `family=`. Simplest rule,
  no new precedence concept, and it means `fit.color=`/`marg.color=` stay as
  convenient shortcuts for the common case rather than a second competing
  styling channel.

**C. Status quo (reserved, errors on anything extra).** Safest, but blocks
the `adjust=` use case GK actually wants, so not really a live option --
included only for completeness.

## Recommendation

Option B, matching MF's email. It's the only one of the three that scales
cleanly to a third geom (`marginal = "density"`) without adding a third
top-level parameter, avoids the cross-geom name-collision problem in
histogram mode, and reuses a pattern (`list()` of args, merged and spliced
into a `do.call()`) that's already precedented in this file via
`method.args`. Two follow-up questions worth Gavin's judgment when he
implements this, not answered here:

1. Should `fit.args`/`marg.args` be validated against the target geom's
   actual formals before the `do.call()` (friendlier error pointing at
   `logist_plot()`), or left to fail inside ggplot2's own call (simpler,
   but the error will reference `geom_smooth()`/`geom_rect()` internals
   instead of `logist_plot()`)?
   
2. Whether `marg.args` in histogram mode should reach `geom_rect()` itself
   (bin geometry is computed by hand, not by `geom_histogram()` -- see
   `rect-plan.md` step 3) or only make sense once `marginal = "density"`
   exists. If the former, worth listing which `geom_rect()` arguments are
   actually safe to expose (e.g. `linetype`, `colour` for bar outlines)
   versus which are computed internally and shouldn't be overridable
   (`xmin`/`xmax`/`ymin`/`ymax`).
