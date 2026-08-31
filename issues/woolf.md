# The `woolf_test()` row/column decomposition is NOT generally valid

## Summary

For a $2 \times 2 \times R \times C$ table, `woolf_test(x,
decompose = TRUE)` currently describes the overall Woolf statistic as an
ANOVA-like decomposition into row, column, and residual components:

\[
Q_{\mathrm{total}} = Q_{\mathrm{row}} + Q_{\mathrm{column}} +
Q_{\mathrm{residual}}.
\]

The implementation does not, however, construct all three terms from the same
set of stratum-specific log odds ratios and the same weighted quadratic form.
It calculates:

1. the total statistic from the $RC$ original $2 \times 2$ strata;
2. the row statistic after summing the counts over $C$;
3. the column statistic after summing the counts over $R$; and
4. the residual as `total - rows - cols`.

Collapsing a contingency table generally changes both its odds ratio and the
Woolf inverse-variance weight. Odds ratios are non-collapsible, so a marginal
odds ratio obtained by summing counts is not generally an average of the
conditional odds ratios that entered the total statistic. Consequently, the
row and column statistics are tests on two different marginal tables, not
orthogonal components of the original $RC$-stratum statistic.

The degrees of freedom happen to add correctly,

\[
(R-1) + (C-1) + (R-1)(C-1) = RC-1,
\]

but this arithmetic identity does not make the statistics additive. In
particular, `rows + cols` can exceed `total`, making the residual negative. A
negative value cannot be a chi-squared statistic, and its reported chi-squared
p-value is therefore invalid.

The ordinary, non-decomposed Woolf test is not affected. The problem is
confined to the interpretation and inference produced by `decompose = TRUE`.

## Two separable issues: weighting and sequential partitioning

An earlier version of this document proposed "use weighted least squares" as
*the* fix (see Fix 2 below), which conflates two independent design
decisions. It helps to separate them, by analogy with Type I ("sequential")
vs. Type II/III sums of squares in regression:

1. **How each stratum is weighted.** The stratum-level responses here are
   $y_{ij} = \log\widehat\theta_{ij}$, with Woolf inverse-variance weights
   $w_{ij}$. Using $w_{ij}$ (WLS) rather than equal weights (OLS) is an
   efficiency choice, justified because strata differ in precision. It has
   nothing to do with additivity.

2. **How Row and Column sums of squares are combined.** In a *complete*
   $R \times C$ layout with one observation per cell (exactly our case: one
   $y_{ij}$ per stratum), the row and column sum-to-zero contrasts are
   **automatically orthogonal under equal weights** -- this is the standard
   two-way ANOVA fact that a balanced, complete factorial design has
   Type I = Type II = Type III sums of squares. So with equal weights, the
   simultaneous (order-independent) split
   $S_0 = (S_0 - S_R) + (S_0 - S_C) + S_A$
   is *already* exact, with no need for sequential fitting at all --
   `twoway::twoway()`'s plain mean-polish computes exactly this, for free
   (see [`woolf_twoway(x, weighted = FALSE)`](../R/woolf_twoway.R)).

   Once **unequal** weights are introduced, that orthogonality is generally
   lost. Additivity is then only guaranteed by a *sequential* (Type I-style)
   construction -- fitting nested models in a chosen order -- exactly as in
   weighted regression: sequential SS always telescope to the model SS
   regardless of weighting or collinearity, but Type II/III SS generally do
   not sum to the model SS once predictors are non-orthogonal.

The bug documented below is a failure of *both* good practice at once: Rows
and Columns are computed from two different collapsed-count tables (each
with its own re-derived odds ratio and weight), rather than as nested
submodels fit to the *same* per-stratum $y_{ij}$/$w_{ij}$. That would be
non-additive even with equal weights -- switching to WLS alone, without also
switching to sequential (nested-model) fitting, would not have fixed it.

## Current calculation

For original stratum $(i,j)$, let

\[
y_{ij} = \log(\widehat\theta_{ij}), \qquad
w_{ij} = \left(\frac{1}{n_{11ij}} + \frac{1}{n_{12ij}} +
                     \frac{1}{n_{21ij}} + \frac{1}{n_{22ij}}\right)^{-1}.
\]

The overall Woolf statistic is

\[
Q_0 = \sum_{i,j} w_{ij}(y_{ij} - \bar y_w)^2.
\]

This is a weighted residual sum of squares for an intercept-only model fitted
to the $RC$ log odds ratios. A component of $Q_0$ must be formed by a
projection or nested-model comparison using these same responses and weights.

Instead, the current row calculation first constructs

\[
n_{ab,i+} = \sum_j n_{ab,ij}
\]

and then calculates new values $y^*_{i+}$ and $w^*_{i+}$ from those
collapsed counts. The column calculation independently constructs another set
of values $y^*_{+j}$ and $w^*_{+j}$. Neither calculation is a projection of
the vector $y_{ij}$ under the inner product defined by $w_{ij}$. Subtracting
these two statistics from $Q_0$ merely forces a numerical identity; it does
not produce an interaction sum of squares.

The marginal row and column results may still be useful as **separate Woolf
tests of marginal odds ratios**. They should not be called components of the
conditional $RC$-stratum test, and their residual difference should not be
given a chi-squared reference distribution.

## Counterexample

**Source of the data:** this is not a real dataset. It is constructed
(synthetic) data, chosen only to make the failure large and unambiguous. It
is a $2 \times 2 \times 3 \times 2$ table ($R$ has 3 levels, $C$ has 2), with
these six $2 \times 2$ strata:

| `C` | `R` | $n_{11}$ | $n_{12}$ | $n_{21}$ | $n_{22}$ |
|---|---|---:|---:|---:|---:|
| C1 | R1 | 51 | 138 | 34 | 129 |
| C1 | R2 | 43 | 26 | 187 | 143 |
| C1 | R3 | 186 | 152 | 29 | 170 |
| C2 | R1 | 48 | 24 | 39 | 181 |
| C2 | R2 | 40 | 90 | 83 | 163 |
| C2 | R3 | 43 | 29 | 1 | 78 |

Built into a `2x2x3x2` array `x` (dimensions `Group`, `Outcome`, `R`, `C`)
and run through the *current, unmodified* `woolf_test()`:

```r
x <- array(0, dim = c(2, 2, 3, 2),
           dimnames = list(Group = c("g1", "g2"), Outcome = c("o1", "o2"),
                            R = c("R1", "R2", "R3"), C = c("C1", "C2")))
x[,, "R1", "C1"] <- matrix(c(51, 34, 138, 129), 2)
x[,, "R2", "C1"] <- matrix(c(43, 187, 26, 143), 2)
x[,, "R3", "C1"] <- matrix(c(186, 29, 152, 170), 2)
x[,, "R1", "C2"] <- matrix(c(48, 39, 24, 181), 2)
x[,, "R2", "C2"] <- matrix(c(40, 83, 90, 163), 2)
x[,, "R3", "C2"] <- matrix(c(43, 1, 29, 78), 2)

woolf_test(x, decompose = TRUE)
```

which gives the actual output (vcdExtra 0.9.9):

```
Woolf-test on Homogeneity of Odds Ratios (no 4-way association)

Data:          x
OR variables:  Group, Outcome
Strata:        R, C

Overall homogeneity test:
  X-squared = 85.2300, df = 5, p-value = 1.11e-16

Decomposition:
  Rows (R): X-squared = 88.2505, df = 2, p-value = 0
  Cols (C): X-squared = 12.7162, df = 1, p-value = 0.0003625
  Residual: X-squared = -15.7367, df = 2, p-value = 1

Note: Overall = Rows + Columns + Residual
```

The residual is negative, and its reported p-value of 1 is meaningless.

`Fungicide`, the example currently used in `?woolf_test`, happens to give a
small positive residual, and so does not reveal the problem. But this is not
only a synthetic-data artifact: `Detergent` (a real, published dataset
already bundled with vcdExtra; see `?Detergent`), rearranged so that
`Temperature` (2 levels) and `Water_softness` (3 levels) are the strata,
reproduces the same failure with real data:

```
Decomposition:
  Rows (Temperature):    X-squared = 2.7742, df = 1, p-value = 0.09579
  Cols (Water_softness): X-squared = 5.3351, df = 2, p-value = 0.06942
  Residual:              X-squared = -0.0961, df = 2, p-value = 1
```

(see `issues/Woolf_examples.R` for the full, runnable example).

## Possible fixes

### 1. Disable the decomposition

The safest immediate fix is to deprecate or temporarily reject
`decompose = TRUE`, while retaining the ordinary test. At minimum, the current
output should stop calling the result an ANOVA-like decomposition and stop
reporting a residual chi-squared p-value.

The two collapsed analyses could instead be exposed explicitly as optional
descriptive marginal tests. Clear naming would be important, for example
`row_marginal` and `column_marginal`, with documentation saying that they do
not add to the conditional homogeneity statistic.

This is the lowest-risk release fix, but it removes the factorial breakdown
that motivated the option.

### 2. A sequential (Type I-style) partition, using WLS weights

As explained above, additivity comes from fitting a *sequence* of nested
models, not from switching to WLS per se -- WLS is the separate, justified
choice of weights ($w_{ij}$, the Woolf inverse variances) that this sequence
should use, given that strata differ in precision. A valid Woolf-scale
analysis can be obtained by retaining $y_{ij}$ and $w_{ij}$ and fitting
weighted least-squares models to the $R \times C$ layout. Define:

- $S_0$: weighted SSE for the intercept-only model;
- $S_R$: weighted SSE for the row-only model;
- $S_C$: weighted SSE for the column-only model; and
- $S_A$: weighted SSE for the additive row-plus-column model.

Here $S_0$ is exactly the overall Woolf statistic, apart from numerical
roundoff. The additive-model residual

\[
Q_{R:C} = S_A
\]

is a nonnegative measure of row-by-column interaction on the log-odds-ratio
scale, with $(R-1)(C-1)$ degrees of freedom.

An exact sequential partition can be formed in either order. A row-first
partition is

\[
Q_R = S_0-S_R, \qquad
Q_{C\mid R} = S_R-S_A, \qquad
Q_{R:C}=S_A.
\]

It satisfies

\[
S_0 = Q_R + Q_{C\mid R} + Q_{R:C}
\]

with degrees of freedom $R-1$, $C-1$, and $(R-1)(C-1)$, respectively. A
column-first partition is equally valid:

\[
Q_C = S_0-S_C, \qquad
Q_{R\mid C} = S_C-S_A, \qquad
Q_{R:C}=S_A.
\]

These are standard nested weighted-model comparisons. Under the same
large-sample normal approximation used by the Woolf test, their quadratic-form
statistics have the corresponding approximate chi-squared distributions.

**Worked illustration.** Using the same synthetic counterexample table above
(`x`, from `woolf_test(x)$LOR` and `$LOR_se`), fit `y ~ 1`, `y ~ row`,
`y ~ col`, `y ~ row + col` by WLS with `weights = 1 / LOR_se^2`, and take the
weighted SSE of each (`S0`, `SR`, `SC`, `SA`):

```
S0 = 85.230   SR = 30.610   SC = 85.047   SA = 21.695
```

(`S0` matches the overall Woolf statistic above, `85.2300`, up to rounding.)
Row-first partition:

```
Q_R      = S0 - SR = 54.620   df = 2
Q_{C|R}  = SR - SA =  8.915   df = 1
Q_{R:C}  = SA      = 21.695   df = 2
sum = 54.620 + 8.915 + 21.695 = 85.230 = S0  ✓ nonnegative, additive
```

Column-first partition:

```
Q_C      = S0 - SC =  0.183   df = 1
Q_{R|C}  = SC - SA = 63.352   df = 2
Q_{R:C}  = SA      = 21.695   df = 2
sum = 0.183 + 63.352 + 21.695 = 85.230 = S0  ✓ nonnegative, additive
```

Both partitions are valid, additive, and nonnegative -- exactly the two
properties the current `decompose = TRUE` output lacks. But they disagree
sharply on how much is attributed to Rows vs. Columns ($Q_R = 54.62$ vs.
$Q_{R\mid C} = 63.35$; $Q_C = 0.18$ vs. $Q_{C\mid R} = 8.92$), which is the
"important limitation" below made concrete: **the choice of order is not
cosmetic here**, so an implementation cannot silently pick one.

As a sanity check, refitting the same four models **unweighted** (equal
weights, i.e. `weights = 1` throughout) gives `S0 - SR = 11.201`,
`S0 - SC = 3.081`, `SA = 2.639`, and
$S_0 - S_R - S_C + S_A = -4.4\times10^{-16} \approx 0$ -- confirming that
Rows and Columns *are* orthogonal once weights are equal, so the row-first
and column-first partitions coincide exactly, and simultaneous
(order-independent) sums of squares are already additive with no sequential
machinery needed. This is exactly the `woolf_twoway(x, weighted = FALSE)`
case (plain Tukey mean-polish). It is only the switch to unequal Woolf
weights -- appropriate here on efficiency grounds -- that reintroduces
order-dependence and makes a sequential construction necessary.

The important limitation is that the row and column subspaces are not
generally orthogonal under unequal Woolf weights. Therefore the two valid
sequential decompositions can allocate different amounts to rows and columns.
The API and print method must show the order and label the second term as
adjusted for the first; for example, `Rows` followed by `Columns | Rows`.

A prototype can be based on weighted fits of:

```r
y ~ 1
y ~ row
y ~ column
y ~ row + column
```

using one observation per original stratum and `weights = w`. A direct
model-matrix/QR implementation would avoid depending on the extra behavior of
`lm()` and would make rank and tolerance handling explicit.

### 3. Report partial main-effect tests without claiming additivity

If order-independent questions are more important than an additive partition,
report adjusted nested-model tests:

\[
Q_{R\mid C} = S_C-S_A, \qquad
Q_{C\mid R} = S_R-S_A, \qquad
Q_{R:C}=S_A.
\]

These answer whether rows contribute after columns, whether columns contribute
after rows, and whether the additive model is inadequate. They are useful
inferential tests, but their three statistics do **not** generally sum to
$S_0$. The output should call them partial or adjusted tests, not a
decomposition.

This is analogous to choosing partial rather than sequential sums of squares
in an unbalanced ANOVA.

### 4. Use a symmetric descriptive allocation

If a symmetric additive attribution is desired, average each main effect's
contribution over the two possible entry orders (a two-predictor Shapley
allocation):

\[
Q_R^{\mathrm{avg}} = \tfrac12[(S_0-S_R)+(S_C-S_A)],
\]

\[
Q_C^{\mathrm{avg}} = \tfrac12[(S_0-S_C)+(S_R-S_A)].
\]

Then

\[
S_0 = Q_R^{\mathrm{avg}} + Q_C^{\mathrm{avg}} + S_A.
\]

This allocation is symmetric and nonnegative, but the averaged main-effect
shares do not have the simple chi-squared distributions and degrees of freedom
of nested-model differences. It should be presented as descriptive attribution,
not as three conventional hypothesis tests.

### 5. Fit a count-scale loglinear model

A more fundamental alternative is to model the original $2 \times 2 \times R
\times C$ counts. In a hierarchical Poisson loglinear model, variation of the
association between the first two variables can be represented by interactions
of their association term with `R`, `C`, and `R:C`. Nested likelihood-ratio
tests can assess:

- change of the odds ratio over `R`;
- change over `C`, adjusted for `R` (or in the reverse order); and
- the `R:C` interaction in the log odds ratios.

This avoids treating estimated log odds ratios and inverse-variance weights as
fixed data, and it can be preferable for sparse tables. As with weighted least
squares, sequential main-effect allocations are order-dependent unless the
relevant model subspaces are orthogonal. The exact hierarchy and nuisance
terms must be specified carefully before implementation.

## Recommended course

1. Immediately remove or warn on the current residual inference, because it
   can return a negative chi-squared statistic.
2. Preserve collapsed row and column tests only if they are explicitly labeled
   as separate marginal analyses.
3. For a replacement `decompose` option, use the original cell log odds
   ratios and Woolf weights, fit *sequentially* (nested models in a chosen
   order). Both parts matter: WLS alone (without sequential fitting) would
   not restore additivity, and sequential fitting alone (without WLS) would
   restore additivity but discard the efficiency gain from weighting by
   precision.
4. Offer either:
   - an explicitly ordered sequential decomposition, which adds exactly; or
   - order-independent partial tests, which are not described as additive.
5. Consider a symmetric averaged allocation only as an optional descriptive
   summary without chi-squared p-values.

For backward compatibility, a possible interface is:

```r
woolf_test(
  x,
  decompose = c("none", "sequential", "partial", "attribution"),
  order = c("rows", "columns")
)
```

Changing the existing logical `decompose = TRUE` silently to a new statistical
meaning would be risky. A deprecation cycle or a clearly documented release
change would make the transition easier to detect.

## Tests needed for a replacement

Any replacement should include tests that:

- reproduce the ordinary Woolf statistic when no decomposition is requested;
- verify $S_0$ against a direct calculation from the original log odds ratios
  and weights;
- verify the sequential sum identity to numerical tolerance;
- verify nonnegativity of all nested-model differences, allowing only a small
  floating-point tolerance;
- verify the degrees of freedom and p-values for both entry orders;
- show that reversing the order changes only the sequential allocation, not
  the total or interaction term;
- use the counterexample above as a regression test;
- cover equal weights, strongly unequal weights, zero cells/continuity
  correction, $R=1$ or $C=1$, and unnamed dimensions;
- with **equal weights specifically**, verify that the row-first and
  column-first partitions coincide (order no longer matters) and match the
  simultaneous split already computed by `woolf_twoway(x, weighted = FALSE)`
  -- i.e. that the sequential machinery correctly reduces to plain two-way
  ANOVA when weighting is not a factor; and
- compare the implementation with an independently constructed weighted
  model-matrix projection.

Property-based tests over many randomly generated positive tables would be
particularly useful for ensuring that a purported chi-squared component never
becomes materially negative.

## Related code

`breslow_day_test(x, decompose = TRUE)` currently uses the same general pattern:
it computes row and column statistics from collapsed tables and defines the
residual by subtraction. It should be audited separately. The weighted
least-squares solution above is specific to the Woolf statistic and should not
be transferred mechanically to the Breslow-Day test; that test needs a
derivation based on its own score/count-scale formulation or its decomposition
should likewise be disabled.
