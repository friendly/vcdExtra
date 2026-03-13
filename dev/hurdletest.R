# pscl::hurdletest

hurdletest <- function (object, ...) 
{
  stopifnot(inherits(object, "hurdle"))
  stopifnot(object$dist$count == object$dist$zero)
  stopifnot(all(sort(names(object$coefficients$count)) == sort(names(object$coefficients$zero))))
  stopifnot(requireNamespace("car"))
  nam <- names(object$coefficients$count)
  lh <- paste("count_", nam, " = ", "zero_", nam, sep = "")
  rval <- car::linearHypothesis(object, lh, ...)
  attr(rval, "heading")[1] <- "Wald test for hurdle models\n\nRestrictions:"
  return(rval)
}

#pscl:::coef.hurdle

coef.hurdle <- function (object, model = c("full", "count", "zero"), ...) 
{
  model <- match.arg(model)
  rval <- object$coefficients
  rval <- switch(model, full = structure(c(rval$count, rval$zero), 
                                         .Names = c(paste("count", names(rval$count), sep = "_"), 
                                                    paste("zero", names(rval$zero), sep = "_"))), count = rval$count, 
                 zero = rval$zero)
  rval
}