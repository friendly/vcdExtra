# mosaic plot for a glm object
#  Allow it to use residuals of any type computed by residuals()
#  or to pass residuals calculated by another function, e.g., rstandard(), rstudent()
#
#  Allow to apply to any model with discrete factors
# last modified: 3/6/2009 1:51PM
#  - fixed buggy version using ideas from vcd:::plot.loglm
#  - now use $data component when it is a table

## TODO: move to utility.R
#is.discrete.model <- function(model)
# 	all(attr(terms(model), "dataClasses")[-1] %in% c("factor", "ordered"))


`mosaic.glm` <-	function(x, formula = NULL,
                         panel=mosaic, type=c("observed", "expected"),
                         residuals=NULL,
                         residuals_type = c("pearson", "deviance", "rstandard"),
                         gp = shading_hcl, gp_args = list(), ...)
{

	require(vcd)
	if (!inherits(x,"glm")) stop("mosaic.glm requires a glm object")
	# this should work for any glm family, where all predictors are factors
	# But maybe not sensible for non-count data??
	#if (!is.discrete.model(x)) stop("only factors are allowed")


	df.residual <- x$df.residual
	observed <- x$data
	if (inherits(observed, "table")) {
		#
		xlevels <- dimnames(x$data)
	}
        else if (!is.null(formula)) {
            ft <- terms(formula, data = observed)
            ff <- model.frame(ft, data = observed)
            xlevels <- .getXlevels(ft, ff)
        }
        else if (inherits(observed, "data.frame")) {
            ## ok if equivalent to as.data.frame(xtab)
            attr.terms <- attributes(terms(x))
            response <- attr.terms$variables[[2]] #always response?
            ## assume everything else is an indexing factor (for now)
            factors <- eval(substitute(subset(observed, select = -response)))
            ft <- terms(~ ., data = factors)
            ok <- TRUE
            if (all(attr(ft, "dataClasses") == "factor")) {
                xlevels <- .getXlevels(ft, model.frame(ft, data = factors))
                ## check cross-classifying
                per.cell <- c(tapply(x$y, factors, length))
                if (ok <- isTRUE(all(per.cell == 1))) {
                    f <- formula(ft)
                    warning("no formula provided, assuming ", deparse(f), "\n",
                            call. = FALSE)
                }
            }
            if (!ok)
                stop("cannot identify indexing factors from ", substitute(x),
                     "$data - please provide formula", call. = FALSE)
        }
        else
            stop("cannot identify indexing factors - please provide formula",
                 call. = FALSE)

	# if a data.frame, extract the frequencies, re-shape as a table
	if (inherits(observed, "data.frame"))
            observed <- array(x$y, dim=lapply(xlevels,length), dimnames=xlevels)

	expected <- array(fitted(x), dim=lapply(xlevels,length), dimnames=xlevels)

	type <- match.arg(tolower(type), c("observed", "expected"))
	if (any(observed < 0)) stop("requires a non-negative response vector")

	residuals_type <- match.arg(tolower(residuals_type), c("pearson", "deviance", "rstandard"))
	if (missing(residuals))
		residuals <- if (residuals_type=="rstandard") rstandard(x)
				else residuals(x, type=residuals_type)
	# reshape the residuals to conform to the structure of data
	residuals <- array(residuals, dim=lapply(xlevels,length), dimnames=xlevels)

	gp <- if (inherits(gp, "grapcon_generator"))
				do.call("gp", c(list(observed, residuals, expected, x$df.residual),
								as.list(gp_args)))
			else gp

	panel(observed, residuals=residuals, expected=expected, type=type,
			residuals_type=residuals_type, gp=gp, ...)
}

