# logLik method for loglm objects, to allow use of AIC() and BIC()
# with MASS::loglm, giving comparable results to the use of these
# functions with glm(..., family=poisson) models.

# allow for non-integer frequencies

logLik.loglm <- function(object, ...) {
  fr <- if(!is.null(object$frequencies)) unclass(object$frequencies) else {
    unclass(update(object, keep.frequencies = TRUE)$frequencies)
  }
  df <- prod(dim(fr)) - object$df
  structure(sum((log(fr) - 1) * fr - lgamma(fr + 1))  - object$deviance/2,
    df = df, class = "logLik")
}


