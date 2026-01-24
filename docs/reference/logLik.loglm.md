# Log-Likelihood of a loglm Object

Calculates the log-likelihood value of the `loglm` model represented by
`object` evaluated at the estimated coefficients.

## Usage

``` r
# S3 method for class 'loglm'
logLik(object, ..., zero = 1e-10)
```

## Arguments

- object:

  A `loglm` object

- ...:

  For compatibility with the S3 generic; not used here

- zero:

  value used to replace zero frequencies in calculating the
  log-likelihood

## Value

Returns an object of class `logLik`. This is a number with one
attribute, `"df"` (degrees of freedom), giving the number of (estimated)
parameters in the model.

## Details

It allows the use of [`AIC`](https://rdrr.io/r/stats/AIC.html) and
[`BIC`](https://rdrr.io/r/stats/AIC.html), which require that a `logLik`
method exists to extract the corresponding log-likelihood for the model.

If cell frequencies have not been stored with the `loglm` object (via
the argument `keep.frequencies = TRUE`), they are obtained using
`update`.

This function calculates the log-likelihood in a way that allows for
non-integer frequencies, such as the case where 0.5 has been added to
all cell frequencies to allow for sampling zeros. If the frequencies
still contain zero values, those are replaced by the value of `start`.

For integer frequencies, it gives the same result as the corresponding
model fit using [`glm`](https://rdrr.io/r/stats/glm.html), whereas
[`glm`](https://rdrr.io/r/stats/glm.html) returns `-Inf` if there are
any non-integer frequencies.

## See also

[`loglm`](https://rdrr.io/pkg/MASS/man/loglm.html),
[`AIC`](https://rdrr.io/r/stats/AIC.html),
[`BIC`](https://rdrr.io/r/stats/AIC.html),

## Author

Achim Zeileis

## Examples

``` r
data(Titanic, package="datasets")

require(MASS)
titanic.mod1 <- loglm(~ (Class * Age * Sex) + Survived, data=Titanic)
titanic.mod2 <- loglm(~ (Class * Age * Sex) + Survived*(Class + Age + Sex), data=Titanic)
titanic.mod3 <- loglm(~ (Class * Age * Sex) + Survived*(Class + Age * Sex), data=Titanic)

logLik(titanic.mod1)
#> 'log Lik.' -399.6822 (df=17)
AIC(titanic.mod1, titanic.mod2, titanic.mod3)
#>              df      AIC
#> titanic.mod1 17 833.3644
#> titanic.mod2 22 283.9687
#> titanic.mod3 23 267.9503
BIC(titanic.mod1, titanic.mod2, titanic.mod3)
#>              df      BIC
#> titanic.mod1 17 858.2819
#> titanic.mod2 22 316.2149
#> titanic.mod3 23 301.6622

# compare with models fit using glm()
titanic <- as.data.frame(Titanic)
titanic.glm1 <- glm(Freq ~ (Class * Age * Sex) + Survived,
                    data=titanic, family=poisson)
titanic.glm2 <- glm(Freq ~ (Class * Age * Sex) + Survived*(Class + Age + Sex),
                    data=titanic, family=poisson)
titanic.glm3 <- glm(Freq ~ (Class * Age * Sex) + Survived*(Class + Age * Sex),
                    data=titanic, family=poisson)

logLik(titanic.glm1)
#> 'log Lik.' -399.6822 (df=17)
AIC(titanic.glm1, titanic.glm2, titanic.glm3)
#>              df      AIC
#> titanic.glm1 17 833.3644
#> titanic.glm2 22 283.9687
#> titanic.glm3 23 267.9503
BIC(titanic.glm1, titanic.glm2, titanic.glm3)
#>              df      BIC
#> titanic.glm1 17 858.2819
#> titanic.glm2 22 316.2149
#> titanic.glm3 23 301.6622

```
