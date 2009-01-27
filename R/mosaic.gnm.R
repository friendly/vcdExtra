`mosaic.gnm` <-
function(x,  residuals=NULL, ..., residuals_type="deviance") {
	if (!inherits(x,"gnm")) stop("mosaic.gnm requires a gnm object")
	if (!is.discrete.model(x)) stop("only factors are allowed")
	
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
  if (!inherits(residuals, "table") && !is.null(x$table.attr)) {
        attributes(residuals) <- x$table.attr
  } 
  else {
        residuals <- array(residuals, dim=lapply(xlevels,length), dimnames=xlevels) 
  }
 
  mosaic(data, residuals=residuals, residuals_type=residuals_type, gp_args=list(df=df.residual), ...)

}

