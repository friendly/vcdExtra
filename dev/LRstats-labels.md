# Better labeling in `LRstats()` output

## Motivation

Currently, `LRstats()` labels models using their object names (e.g., `joint.1`, `joint.2`, ...),
which are uninformative about what each model actually represents.
With `get_models()` now available, we can offer an option to label rows with the model formula
or bracket notation instead.

For example, with the Titanic sequential joint independence models, the current output is:

```
Likelihood summary table:
           AIC    BIC LR Chisq Df Pr(>Chisq)
joint.1 509.95 509.33   475.81  3  < 2.2e-16 ***
joint.2 478.75 479.14   412.60  3  < 2.2e-16 ***
joint.3 257.88 264.83   159.10  7  < 2.2e-16 ***
joint.4 833.36 858.28   671.96 15  < 2.2e-16 ***
```

With `label = "formula"`, the output would become:

```
Likelihood summary table:
                                AIC    BIC LR Chisq Df Pr(>Chisq)
= Class                      509.95 509.33   475.81  3  < 2.2e-16 ***
(Class) (Sex)                478.75 479.14   412.60  3  < 2.2e-16 ***
(Class,Sex) (Age)            257.88 264.83   159.10  7  < 2.2e-16 ***
[Class,Sex,Age] [Survived]   833.36 858.28   671.96 15  < 2.2e-16 ***
```

## Implementation

### 1. Add `label` argument to `LRstats.glmlist()` and `LRstats.loglmlist()`

Both methods get a new argument `label = c("name", "formula")`.

- `"name"` (default): current behavior, using `names(object)`.
- `"formula"`: use `get_models(object)` to get model strings for row labels.

After the `rval <- do.call(rbind, rval)` step, override `rownames(rval)` when `label = "formula"`:

```r
if (match.arg(label) == "formula") {
  rownames(rval) <- get_models(object)
}
```

### 2. Modify `LRstats.default()` — line 163

Line 163 currently sets row names from the call:

```r
rownames(rval) <- as.character(sapply(match.call(), deparse)[-1L])[1:nmodels]
```

This is what the `.glmlist` and `.loglmlist` methods inherit when they call
`lapply(object, LRstats.default)` — each individual model gets a row name like `object[[1L]]`.
The list methods then replace these with `names(object)` via `do.call(rbind, rval)`.

No change is needed to `LRstats.default()` itself. The label override happens in the
list methods after the rbind.

### 3. Pass `abbrev` through

Also pass `...` (or an explicit `abbrev` argument) to `get_models()` so users can
request abbreviated labels, e.g., `LRstats(tit.joint, label = "formula", abbrev = 2)`.

### 4. Update documentation

- Add `@param label` to the `LRstats` roxygen docs.
- Add examples showing `label = "formula"` usage.
