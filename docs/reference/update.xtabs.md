# Update method for a `xtabs` object

Provides an `update` method for `"xtabs"` objects, typically by removing
terms from the formula to collapse over them.

## Usage

``` r
# S3 method for class 'xtabs'
update(object, formula., ..., evaluate = TRUE)
```

## Arguments

- object:

  An existing `"xtabs"` object

- formula.:

  Changes to the formula ? see
  [`update.formula`](https://rdrr.io/r/stats/update.formula.html) for
  details

- ...:

  Additional arguments to the call, or arguments with changed values.

- evaluate:

  If `TRUE`, evaluate the new call else return the call

## Value

If `evaluate == TRUE`, the new `"xtabs"` object, otherwise the updated
call

## See also

[`update.formula`](https://rdrr.io/r/stats/update.formula.html) for
details on updates to model formulae

[`margin.table`](https://rdrr.io/r/base/marginSums.html) does something
similar,
[`collapse.table`](https://friendly.github.io/vcdExtra/reference/collapse.table.md)
collapses category levels

## Author

Michael Friendly

## Examples

``` r
vietnam.tab <- xtabs(Freq ~ sex + year + response, data=Vietnam)

update(vietnam.tab, formula = ~ . -year)
#>         response
#> sex        A   B   C   D
#>   Female  71 105 369  37
#>   Male   730 633 950 252


```
