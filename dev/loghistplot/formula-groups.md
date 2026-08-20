# Could `group=` be expressed in the formula as `y ~ x | group`?

Design notes only -- nothing implemented. Prompted by wanting `logist_plot()`'s
formula interface to match the `y ~ x | strata` convention `CMHtest()` already
uses elsewhere in this package.

## Short answer

Yes, it's feasible -- but only by hand-parsing the formula the same way
`CMHtest.formula()` already does (`R/CMHtest.R:238-239`), not by handing a
`|`-formula to `stats::model.frame()`/`terms()` directly. Base R's formula
machinery has no built-in notion of `|` as a conditioning operator (that's a
convention specific packages like `lme4`/`Formula`/survival's `Surv()` bolt on
via their own parsing) -- to base R, `|` is just the ordinary logical-OR infix
operator, and `model.frame()` will try to *evaluate* `x | group` rather than
split on it.

## Verified: what happens today

```r
logist_plot(survived ~ age | sex, data = Donner, marginal = "points")
#> Warning message:
#> In Ops.factor(age, sex) : '|' not meaningful for factors
#> Error: `x` must be numeric; found class "logical"
```

`model.frame()` evaluates `age | sex` as `Ops.factor(age, sex)` (`sex` is a
factor, so `|` dispatches there), which warns and returns `NA`, coerced to
logical -- `.logist_plot_impl()` only catches this by accident, because the
bogus result happens to be non-numeric. This is worse than a clean error: it's
misleading (nothing here mentions grouping, `|`, or a formula problem), and if
the predictor and the intended grouping variable were both plain
numeric/logical instead of one being a factor, `x | group` would evaluate to
an actual (nonsensical) 0/1 vector *without erroring at all* -- a silent-wrong-
result landmine, not just an unclear message.

## What `terms()` does with `|` (also checked directly)

```r
terms(y ~ x | g)
#> attr(,"term.labels")
#> [1] "x | g"
```

`terms()` treats `x | g` as a single opaque term label -- it doesn't split on
`|` either. So there's no way to get base R's own formula/model.frame pipeline
to do this splitting; it has to happen before the formula ever reaches
`model.frame()`, on the original unevaluated formula object (or its deparsed
string).

## The existing precedent: `CMHtest.formula()`

```r
fstr <- strsplit(paste(deparse(formula), collapse = ""), "~")
vars <- strsplit(strsplit(gsub(" ", "", fstr[[1]][2]), "\\|")[[1]], "\\+")
varnames <- vars[[1]]
condnames <- if (length(vars) > 1) vars[[2]] else NULL
```

Deparses the formula to a string, splits on `~` then on `\\|`, and further
splits each side on `\\+` for multiple variables. This is string manipulation,
not a formula-object operation -- it works, but it's also exactly why it's
fragile in ways `logist_plot()` doesn't currently have to worry about (spacing,
backtick-quoted names with `|`/`+` inside them, `.`-expansion). `logist_plot()`
would only need the single-conditioning-variable case, not `CMHtest`'s
multi-variable `+`-combined strata, so it can be considerably simpler.

## Design sketch

1. In `logist_plot.formula()`, before calling `stats::model.frame()`: deparse
   `formula`, split on `~` then on `\\|` (mirroring `CMHtest.formula()`'s
   approach, minimally).
2. If there's no `|`, behave exactly as today.
3. If there's a `|` with exactly one variable on the right, extract that name,
   rebuild a plain two-sided `y ~ x` formula (without the `|`) for
   `model.frame()`, and resolve the grouping variable via the *existing*
   `.resolve_col(data, group_name, "group")` -- i.e., feed it into the same
   validated path the explicit `group=` argument already uses, not a new one.
4. If there's a `|` with more than one variable (`y ~ x | g1 + g2`), error
   explicitly rather than silently combining them -- `group=` itself only
   supports a single grouping vector today (see `.as_group_factor()`), so
   formula-based grouping shouldn't promise more than the argument form does.
5. **Conflict rule, needs an explicit decision**: what if both a formula
   `| group` clause *and* the `group=` argument are supplied in the same call?
   Recommend erroring ("supply `group` either in the formula or as an
   argument, not both") rather than silently preferring one -- picking a
   silent winner is exactly the class of surprising-behavior bug this file's
   own history (`logist-plot-history.md`) has already had to fix multiple
   times for other arguments.
6. Scope: `.formula` method only. `.default`/`.data.frame` already take
   `group=` as a plain column name/vector -- there's no formula there to graft
   `|` onto, so they're unaffected.

## Is it worth doing?

Reasonable ergonomic win, low technical risk *if* implemented via string-
parsing before `model.frame()` (never by handing `|` to base R's formula
machinery directly, per the landmine above). Main cost is that it adds a
second way to say the same thing `group=` already says, which needs the
conflict rule above to stay unambiguous, plus its own tests: `|` with zero/one/
two+ variables, conflict with explicit `group=`, and confirming the rebuilt
formula still validates response/predictor count the same way the current
`ncol(mf) != 2` check does.

The main argument *for* it is package-wide vocabulary consistency -- someone
who already knows `CMHtest(Freq ~ right + left | gender, data = ...)` gets the
same mental model for `logist_plot(survived ~ age | sex, data = Donner)` for
free, without needing to know a separate `group=` argument exists.
