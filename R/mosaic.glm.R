
## TODO: move to utility.R
is.discrete.model <- function(model)
	all(attr(terms(model), "dataClasses")[-1] %in% c("factor", "ordered"))


`mosaic.glm` <-
function(x, residuals=NULL, ..., residuals_type="deviance") {
  require(vcd)
	if (!inherits(x,"glm")) stop("mosaic.glm requires a glm object")
	if (!is.discrete.model(x)) stop("only factors are allowed")

# Allow it to work for any family, where all predictors are factors
# -- how to test this?
#	if (x$family$family != "poisson") stop("mosaic.glm requires a poisson family glm")
	
	xlevels <- x$xlevels
	df.residual <- x$df.residual
	data <- x$data
	# if a data.frame, extract the frequencies, re-shape as a table
  if (inherits(data, "data.frame")) {
    data <- array(x$y, dim=lapply(xlevels,length), dimnames=xlevels) 
  }

  if (missing(residuals))
	  residuals <- residuals(x, type=residuals_type)
  # reshape the residuals to conform to the structure of data
  residuals <- array(residuals, dim=lapply(xlevels,length), dimnames=xlevels) 

#  Problem:  to pass the df= argument via gp_args, we suppress any user-supplied other gp_args supplied through dots
#  Solution: ???
  mosaic(data, residuals=residuals, residuals_type=residuals_type, gp_args=list(df=df.residual), ...)
}

