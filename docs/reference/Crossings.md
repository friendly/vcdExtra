# Crossings Interaction of Factors

Given two ordered factors in a square, n x n frequency table,
`Crossings` creates an n-1 column matrix corresponding to different
degrees of difficulty in crossing from one level to the next, as
described by Goodman (1972).

## Usage

``` r
Crossings(...)
```

## Arguments

- ...:

  Two factors

## Value

For two factors of `n` levels, returns a binary indicator matrix of
`n*n` rows and `n-1` columns.

## Details

Instead of treating all mobility as equal, this model posits that the
difficulty of moving between categories increases with the number of
boundaries (or "crossings") that must be crossed, and that associations
between categories decrease with their separation.

## References

Goodman, L. (1972). Some multiplicative models for the analysis of
cross-classified data. In: *Proceedings of the Sixth Berkeley Symposium
on Mathematical Statistics and Probability*, Berkeley, CA: University of
California Press, pp. 649-696.

## See also

[`glm`](https://rdrr.io/r/stats/glm.html),
[`gnm`](https://rdrr.io/pkg/gnm/man/gnm.html) for model fitting
functions for frequency tables;
[`Diag`](https://rdrr.io/pkg/gnm/man/Diag.html),
[`Mult`](https://rdrr.io/pkg/gnm/man/Mult.html),
[`Symm`](https://rdrr.io/pkg/gnm/man/Symm.html),
[`Topo`](https://rdrr.io/pkg/gnm/man/Topo.html) for similar extensions
to terms in model formulas.

## Author

Michael Friendly and Heather Turner

## Examples
