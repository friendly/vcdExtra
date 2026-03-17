# Loglinear Model Utilities

These functions generate lists of terms to specify a loglinear model in
a form compatible with [`loglin`](https://rdrr.io/r/stats/loglin.html)
and also provide for conversion to an equivalent
[`loglm`](https://rdrr.io/pkg/MASS/man/loglm.html) specification or a
shorthand character string representation.

They allow for a more conceptual way to specify such models by a
function for their type, as opposed to just an uninterpreted list of
model terms and also allow easy specification of marginal models for a
given contingency table. They are intended to be used as tools in
higher-level modeling and graphics functions, but can also be used
directly.

## Usage

``` r
joint(nf, table = NULL, factors = 1:nf, with = nf)

conditional(nf, table = NULL, factors = 1:nf, with = nf)

mutual(nf, table = NULL, factors = 1:nf)

saturated(nf, table = NULL, factors = 1:nf)

markov(nf, factors = 1:nf, order = 1)

loglin2formula(x, env = parent.frame())

loglin2string(x, brackets = c("[", "]"), sep = ",", collapse = " ", abbrev)
```

## Source

Code from Henrique Dallazuanna, <wwwhsd@gmail.com>, R-help 7-4-2013

## Arguments

- nf:

  number of factors for which to generate model

- table:

  a contingency table used for factor names, typically the output from
  [`table`](https://rdrr.io/r/base/table.html)

- factors:

  names of factors used in the model when `table` is not specified

- with:

  indices of the factors against which others are considered
  conditionally independent

- order:

  order of the markov chain

- x:

  a list of terms in a loglinear model, such as returned by `joint`,
  `conditional`, ...

- env:

  environment in which to evaluate the formula

- brackets:

  characters to use to surround model terms. Either a single character
  string containing two characters or a character vector of length two.

- sep:

  characters used to separate factor names within a term

- collapse:

  characters used to separate terms

- abbrev:

  Unused as yet

## Details

The main model specification functions, `conditional`, `joint`,
`markov`, ..., `saturated`, return a list of vectors indicating the
marginal totals to be fit, via the `margin` argument to
[`loglin`](https://rdrr.io/r/stats/loglin.html). Each element of this
list corresponds to a high-order term in a hierarchical loglinear model,
where, e.g., a term like `c("A", "B")` is equivalent to the
[`loglm`](https://rdrr.io/pkg/MASS/man/loglm.html) term `"A:B"` and
hence automatically includes all low-order terms.

Note that these can be used to supply the `expected` argument for the
default [`mosaic`](https://rdrr.io/pkg/vcd/man/mosaic.html) function,
when the data is supplied as a contingency table.

The table below shows some typical results in terms of the standard
shorthand notation for loglinear models, with factors A, B, C, ...,
where brackets are used to delimit the high-order terms in the loglinear
model.

|                    |                   |                         |                               |
|--------------------|-------------------|-------------------------|-------------------------------|
| **function**       | **3-way**         | **4-way**               | **5-way**                     |
| `mutual`           | \[A\] \[B\] \[C\] | \[A\] \[B\] \[C\] \[D\] | \[A\] \[B\] \[C\] \[D\] \[E\] |
| `joint`            | \[AB\] \[C\]      | \[ABC\] \[D\]           | \[ABCE\] \[E\]                |
| `joint (with=1)`   | \[A\] \[BC\]      | \[A\] \[BCD\]           | \[A\] \[BCDE\]                |
| `conditional`      | \[AC\] \[BC\]     | \[AD\] \[BD\] \[CD\]    | \[AE\] \[BE\] \[CE\] \[DE\]   |
| `condit (with=1)`  | \[AB\] \[AC\]     | \[AB\] \[AC\] \[AD\]    | \[AB\] \[AC\] \[AD\] \[AE\]   |
| `markov (order=1)` | \[AB\] \[BC\]     | \[AB\] \[BC\] \[CD\]    | \[AB\] \[BC\] \[CD\] \[DE\]   |
| `markov (order=2)` | \[A\] \[B\] \[C\] | \[ABC\] \[BCD\]         | \[ABC\] \[BCD\] \[CDE\]       |
| `saturated`        | \[ABC\]           | \[ABCD\]                | \[ABCDE\]                     |

`loglin2formula` converts the output of one of these to a model formula
suitable as the `formula` for of
[`loglm`](https://rdrr.io/pkg/MASS/man/loglm.html).

`loglin2string` converts the output of one of these to a string
describing the loglinear model in the shorthand bracket notation, e.g.,
`"[A,B] [A,C]"`.

## See also

Other loglinear models:
[`assoc_graph()`](https://friendly.github.io/vcdExtra/reference/assoc_graph.md),
[`get_model()`](https://friendly.github.io/vcdExtra/reference/get_model.md),
[`glmlist()`](https://friendly.github.io/vcdExtra/reference/glmlist.md),
[`plot.assoc_graph()`](https://friendly.github.io/vcdExtra/reference/plot.assoc_graph.md),
[`seq_loglm()`](https://friendly.github.io/vcdExtra/reference/seq_loglm.md)
