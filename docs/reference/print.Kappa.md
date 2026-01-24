# Print Kappa

This is a replacement for the `print.Kappa` method in `vcd`, adding
display of `z` values to the `vcd` version and optional confidence
intervals.

## Usage

``` r
# S3 method for class 'Kappa'
print(
  x,
  digits = max(getOption("digits") - 3, 3),
  CI = FALSE,
  level = 0.95,
  ...
)
```

## Arguments

- x:

  A Kappa object

- digits:

  number of digits to print

- CI:

  Include confidence intervals in the display?

- level:

  confidence level

- ...:

  Other arguments

## Value

Returns the Kappa object, invisibly.

## See also

[`confint.Kappa`](https://rdrr.io/pkg/vcd/man/Kappa.html)

## Author

Michael Friendly

## Examples

``` r
data("SexualFun")
Kappa(SexualFun)
#>             value     ASE     z
#> Unweighted 0.1293 0.06860 1.885
#> Weighted   0.2374 0.07832 3.031
print(Kappa(SexualFun), CI=TRUE)
#>             value     ASE     z    lower  upper
#> Unweighted 0.1293 0.06860 1.885 -0.00512 0.2638
#> Weighted   0.2374 0.07832 3.031  0.08388 0.3909

# stratified 3-way table
apply(MSPatients, 3, Kappa)
#> $Winnipeg
#>             value     ASE     z
#> Unweighted 0.2079 0.05046 4.121
#> Weighted   0.3797 0.05167 7.350
#> 
#> $`New Orleans`
#>             value     ASE     z
#> Unweighted 0.2965 0.07850 3.777
#> Weighted   0.4773 0.07303 6.535
#> 
```
