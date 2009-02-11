## TODO: fix xlevels to exclude special terms like Diag(), Mult, etc
## TODO: should have a way to specify a subset of the variables to be shown in the mosaic

`mosaic.gnm` <-
		function(x, panel=mosaic, type=c("observed", "expected"), residuals=NULL, 
				residuals_type = c("pearson", "deviance", "rstandard"), gp = shading_hcl, 
				gp_args = list(), ...) 
{
	
	require(vcd)
	
	if (!inherits(x,"gnm")) stop("mosaic.gnm requires a gnm object")
	#if (!is.discrete.model(x)) stop("only factors are allowed")
	
	xlevels <- x$xlevels
	## gnm objects can include some special terms that are functions of the primary table
	## variables.  Need to exclue these for the mosaic display.  Note sure if this is the best way
	special <- grep("[[:punct:]]", names(xlevels))
	if (length(special)) xlevels <- xlevels[-special]

	df.residual <- x$df.residual
	# gnm objects dont have a $data component
	#observed <- x$data
	# if a data.frame, extract the frequencies, re-shape as a table
#	if (inherits(observed, "data.frame")) {
#		observed <- array(x$y, dim=lapply(xlevels,length), dimnames=xlevels) 
#	}
	observed <- array(x$y, dim=lapply(xlevels,length), dimnames=xlevels) 
	expected <- array(fitted(x), dim=lapply(xlevels,length), dimnames=xlevels) 	
	
	type <- match.arg(tolower(type), c("observed", "expected"))
	if (any(observed < 0)) stop("requires a non-negative response vector")
	
	residuals_type <- match.arg(tolower(residuals_type), c("pearson", "deviance", "rstandard"))
	if (missing(residuals))
		residuals <- if (residuals_type=="rstandard") rstandard(x)
				else residuals(x, type=residuals_type)
	
	# reshape the residuals to conform to the structure of data
	if (!inherits(residuals, "table") && !is.null(x$table.attr)) {
		attributes(residuals) <- x$table.attr
	} 
	else {
		residuals <- array(residuals, dim=lapply(xlevels,length), dimnames=xlevels) 
	}
	
	gp <- if (inherits(gp, "grapcon_generator")) 
				do.call("gp", c(list(observed, residuals, expected, x$df.residual), 
								as.list(gp_args)))
			else gp
	
	panel(observed, residuals=residuals, expected=expected, type=type,
			residuals_type=residuals_type, gp=gp, ...)
	
}

