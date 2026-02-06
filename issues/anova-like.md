# ANOVA-like term-by-term tests for loglinear models

## The gap

vcdExtra provides good tools for comparing *different models* against each other:

- **`seq_loglm()`** fits a sequence of models to marginal sub-tables
- **`Kway()`** fits all k-way models (0-way through saturated), giving pooled tests
  for all terms of each order via `anova()`
- **`LRstats()`** gives compact AIC/BIC/G^2 summaries for a `glmlist` or `loglmlist`

What is missing is a way to assess the contribution of **individual terms** within
a single fitted model -- the loglinear analog of `anova()` (Type I) or
`car::Anova()` (Type II/III) for linear models.

For example, given a model `[AB][AC][BC]` for a 3-way table, a natural
question is: how much does each two-way association (AB, AC, BC) contribute
to the model? This requires computing, for each term, the change in G^2 when
that term alone is dropped.

### What already exists elsewhere

- **`stats::anova(glm_obj)`** gives sequential (Type I) tests, but these are
  order-dependent and not available for `loglm` objects.
- **`car::Anova(glm_obj, type="II")`** gives partial association tests, but only
  for `glm` objects, not `loglm`.
- **`stats::drop1(glm_obj, test="Chisq")`** gives single-term-deletion tests for
  `glm`, but not for `loglm`.
- **`MASS::dropterm()`** is a similar facility for `loglm` objects but is
  relatively obscure.

None of these provide association **strength** (effect size) measures.

## Proposed solutions

### 1. `Anova.loglm()` -- partial association tests for loglm objects

A `car::Anova` method for `loglm` objects that performs Type II tests
by dropping each term and computing Delta-G^2.

```r
Anova.loglm(mod, type = c("II", "III"))
```

Implementation approach:
- Parse the model formula to identify terms (main effects + interactions)
- For each term, construct the reduced model formula excluding that term
  (while respecting hierarchy for Type II)
- Refit with `loglm()` and compute Delta-G^2, Delta-df
- Return an anova-style data frame

Output would look like:

```
Analysis of Deviance Table (Type II tests)

         LR Chisq  Df  Pr(>Chisq)
A:B       45.23     6   < 0.001
A:C       12.81     4     0.012
B:C        3.42     3     0.331
```

Note: registering a method for `car::Anova` requires either importing car or
using a standalone function name (e.g., `LRanova()`), since it is a generic
from the `car` package.

### 2. `drop1.loglm()` -- single-term deletion tests

A simpler alternative that extends `stats::drop1()` to work with `loglm` objects:

```r
drop1.loglm(object, scope, test = "Chisq")
```

This is analogous to `MASS::dropterm()` but integrated into the standard
`drop1()` framework. For each droppable term, report Delta-G^2 and
Delta-Pearson-X^2 together.

### 3. Effect size measures -- `assoc_strength()`

The most novel contribution. For each term in a loglinear model, compute
measures of the **strength** of that association, not just its significance.

```r
assoc_strength(model)
```

Candidate measures:

| Measure | Description | Range |
|---------|-------------|-------|
| Partial G^2 | Change in G^2 when term is dropped | 0 to Inf |
| Partial omega^2 | (Delta-G^2 - Delta-df) / (G^2_indep + N) | 0 to 1 |
| Entropy-based R^2 | Delta-G^2 / G^2_baseline | 0 to 1 |
| Partial Cramer's V | Derived from partial chi-sq for 2-way terms | 0 to 1 |

The **entropy-based R^2** (proportion of G^2 accounted for by each term relative
to some baseline such as mutual independence) is perhaps the most interpretable.
It parallels eta-squared in ANOVA: what fraction of the total association in
the table is attributable to this particular term?

For two-way interaction terms specifically, the partial Delta-G^2 can be
converted to Cramer's V or a similar measure that accounts for table dimensions,
giving an interpretable effect size on a 0-1 scale.

### 4. `LRanova()` -- unified term-level analysis

A single function that combines significance testing with effect sizes:

```r
LRanova(model, baseline = NULL, type = c("II", "III"))
```

This would work for both `loglm` and `glm` (Poisson family) objects,
returning a data frame with:

```
Term-level Analysis of Association

         Delta-G^2  Df  Pr(>Chisq)  Partial R^2  Cramer's V
A:B        45.23     6    < 0.001     0.342         0.28
A:C        12.81     4      0.012     0.097         0.15
B:C         3.42     3      0.331     0.026         0.07
```

The `baseline` argument controls the reference model for R^2 computation
(default: mutual independence).

## Implementation notes

### Extracting and manipulating loglm terms

`loglm` stores terms differently from `glm`. The formula typically uses
variable names or numeric indices:
```r
mod <- loglm(~ A + B + C + A:B + A:C + B:C, data = tab)
mod <- loglm(~ 1 + 2 + 3 + 1:2 + 1:3 + 2:3, data = tab)
```

The model object stores:
- `mod$formula` -- the original formula
- `mod$margin` -- list of generating class terms (highest-order terms)
- `mod$lrt` and `mod$pearson` -- overall G^2 and X^2
- `mod$df` -- residual degrees of freedom

To drop a term, reconstruct the formula without that term's margin and refit.
The `mod$margin` list (generating class) is the most reliable way to
identify terms -- it contains the highest-order terms that define the model.
Removing one element from this list and refitting gives the reduced model.

### Relationship to existing vcdExtra functions

- **`LRstats()`** could gain a `terms = TRUE` option that triggers
  term-level analysis instead of model-level comparison
- **`seq_loglm()`** already does something related for sequential marginal
  models; the new functions would complement this for arbitrary models
- **`Kway()`** gives pooled tests by order; the new functions give
  individual-term tests within a specific model

### Handling the glm case

For `glm` objects with `family = poisson`, `car::Anova()` already provides
Type II/III tests. The new contribution would be:
1. Adding effect size measures alongside the tests
2. Providing a unified interface that works for both `loglm` and `glm`
3. Displaying results in bracket notation familiar to loglinear model users

## References

- Brown, M.B. (1976). Screening effects in multidimensional contingency tables.
  *Applied Statistics*, 25, 37-46. (Partial association tests)
- Agresti, A. (2013). *Categorical Data Analysis*, 3rd ed. Wiley.
  Ch. 9 on model building and comparison.
- Goodman, L.A. (1970). The multivariate analysis of qualitative data.
  *JASA*, 65, 226-256.
