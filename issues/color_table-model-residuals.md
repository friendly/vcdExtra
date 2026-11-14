# `color_table()`: misplaced residuals with `model =`, and console output via `cat()`

Found 2026-11-14 while using `color_table()` for a three-way table in the Agatha Christie blog
post (`friendly.github.io/blog/drafts/AgathaChristie/R/AgathaChristie.R`, chunk `three-way`).
vcdExtra 0.9.8. The blog post works around both problems for now. Nothing here has been changed
in the package yet.

## 1. BUG: residuals from a user-supplied `model` go into the wrong cells (multi-way tables)

**Severity: high.** The table looks fine but some cells are silently shaded wrong.

In `.color_table_impl()` (`R/color_table.R`, around line 597), the `model` branch does:

```r
resid_arr <- residuals(model, type = "pearson")
resid_mat <- matrix(as.vector(resid_arr), nrow = dims[1], ncol = dims[2])
```

`as.vector()` runs over the *original* array order (first dimension fastest). The displayed
table is flattened by `structable(formula, ...)`, which makes the *last* row variable vary
fastest. When there is more than one row (or column) variable, the two orders differ and the
residuals are put into the wrong cells. The default independence branch (around line 637) does
it correctly: it re-flattens the residual array with `vcd::structable(formula, data = resid_arr)`.

### Reproducible example

```r
library(vcdExtra)
tab <- as.table(array(c(135, 10, 1, 6, 2, 2, 2, 8), dim = c(2, 2, 2),
  dimnames = list(Related   = c("Not related", "Related"),
                  Financial = c("No gain", "Gain"),
                  Killer    = c("No", "Yes"))))
mod <- MASS::loglm(~ Related * Financial + Killer, data = tab, fitted = TRUE)
r   <- residuals(mod, type = "pearson")

as.matrix(structable(Killer ~ Related + Financial, data = r))  # correct layout
matrix(as.vector(r), nrow = 4)                                 # what color_table() uses

color_table(tab, formula = Killer ~ Related + Financial, model = mod)
```

The correct residuals for Killer = Yes are -2.81, **3.47, 0.98**, 6.28. `color_table()` shades
them as -2.81, **0.98, 3.47**, 6.28: the "Not related / Gain" and "Related / No gain" rows
are swapped.

**Workaround (used in the blog):** order the table's dimensions so that `as.vector()` happens
to match the structable order. Here that means `xtabs(~ Financial + Related + Killer)`, with
the formula still `Killer ~ Related + Financial`.

### Fix ideas

- In the `model` branch, do what the independence branch does: take the residual array and
  re-flatten it with the same `structable()` call that was used for `x` (`formula` if given,
  otherwise the default). It would be simplest to factor this into one helper,
  `.flatten_like_x(arr, formula)`, used by every branch, so the branches can't drift apart.
- Even better: compute the residuals on the *unflattened* table in every branch and flatten
  once at the end, together with the frequencies.
- Check that `dimnames(resid_mat)` match `dimnames(x)` before shading, and `stop()` if they
  don't. That guards against this kind of silent mismatch in general.
- For `glm` models (the docs say `model` may be loglm or glm): `residuals.glm()` returns a
  vector in data-frame row order, not an array. Check how that case is handled. It probably
  needs to be reshaped to an array using the model frame's factors before it is flattened.
- **Check the `expected =` branch as well.** It computes `(x - expected) / sqrt(expected)` with `x`
  already flattened. The docs say `expected` has "the same shape as `x`", but for a multi-way
  table it is unclear whether that means the array or the flat matrix. Accept the array, and
  flatten it with the same helper.
- Add a test: a 2x2x2 table with an asymmetric model and two row variables, checking that each
  shaded cell's residual matches `structable(formula, residuals(mod))`. Also test the column side,
  with two column variables.

## 2. Console output via `cat()` that can't be suppressed

`color_table()` always prints the shading note with `cat()` (lines ~604, 610, 649, 663), even
when `legend = TRUE` already puts the same text under the table as a `gt` source note. In
Quarto/R Markdown this adds a duplicate line of output, and `#| message: false` can't remove it.
The blog currently has to use:

```r
invisible(capture.output(ct <- color_table(...)))
ct
```

### Fix ideas (pick one)

- **Replace `cat()` with `message()`.** This is the standard R convention for informational
  output. Users can then silence it with `suppressMessages()` or `message: false`. This is the
  minimal fix.
- **Only print when the note isn't shown**, i.e. `if (!show_legend) message(shading_message)`.
  `legend = TRUE` puts it in the table; `legend = FALSE` sends it to the console. This matches
  what the `legend` docs already describe ("FALSE suppresses the note, but a message is printed
  in the console").
- Optionally add a `verbose` / `quiet` argument, or respect an option such as
  `getOption("vcdExtra.verbose", TRUE)`, if console output should be controllable on its own.
- The `X^2 = ...` part could also be stored as an attribute of the returned gt object (e.g.
  `attr(gt, "shading")`), so it can still be retrieved programmatically when nothing is printed.

Recommended: do the second option, using `message()`.

## 3. Related: "Re-fitting ..." from MASS

`residuals.loglm()` prints "Re-fitting to get frequencies and fitted values" with `cat()` when the
model was fit without `fitted = TRUE`. This comes from MASS, not vcdExtra, but `color_table()`
could avoid it: when `model` is a loglm without `$fitted`, refit it quietly with
`update(model, fitted = TRUE)`, or compute the Pearson residuals directly. Otherwise, the docs
could say to pass `fitted = TRUE`.

## 4. Smaller things noticed at the same time

- **No labels for multiple row variables.** With `formula = Killer ~ Related + Financial` the two
  stub columns show only level names ("No"/"Yes"), and nothing says which variable is which.
  Add the variable names as stub-column headers (`gt::tab_stubhead()`, or labels on the stub
  columns), as the column spanner already does for column variables. The blog works around this
  by relabeling the levels ("Not related"/"Related", "No gain"/"Gain").
- **`@param legend` doc contradicts itself**: it says both "`TRUE` or `"note"` (default)" and
  "`FALSE` (default)". The code default is `FALSE`. It also uses `\code{}`; convert it to markdown.
- **`@param palette` doc typo**: `c("#B2182B", "white", , "#2166AC")` has an empty element.
- `@param filename` still uses `\code{\link[gt]{gtsave}}`. Change it to `[gt::gtsave()]`.
