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
        if (is.null(formula)) {
            if (is.environment(observed)) observed <- model.frame(x)
            if (inherits(observed, "table"))
                formula <- reformulate(names(dimnames(observed)))
            else if (inherits(observed, "data.frame")) {
                ## ok if equivalent to as.data.frame(xtab)
                attr.terms <- attributes(terms(x))
                resp <- deparse(attr.terms$variables[[2]])
                factors <- setdiff(colnames(observed), resp)
                ## assume everything bar response is an indexing factor (for now)
                formula <- reformulate(factors)
                ok <- TRUE
                ## check cross-classifying
                per.cell <- tapply(observed[,1], observed[factors], length)
                if (ok <- isTRUE(all(per.cell == 1))) {
                    warning("no formula provided, assuming ", deparse(formula),
                            "\n", call. = FALSE)
                }
                if (!ok)
                    stop("cannot identify indexing factors from ", substitute(x),
                         "$data - please provide formula", call. = FALSE)
            }
            else
                stop("cannot identify indexing factors - please provide one-sided formula for factors in plot",
                     call. = FALSE)
        }
        if (length(formula) == 3) formula <- formula[-2]
        ## get indexing factors allowing for missing data, subset etc

        factors <- do.call("model.frame", list(formula = formula, data = observed,
                                               subset = x$call$subset))
        if (!is.null(x$na.action) && inherits(x$na.action, c("omit", "exclude")))
            factors <- factors[-x$na.action,]

	# if necessary create observed table
	if (!is.table(observed))
            observed <- as.table(tapply(x$y, factors, sum))

	expected <- as.table(tapply(fitted(x), factors, sum))

	type <- match.arg(tolower(type), c("observed", "expected"))
	if (any(observed < 0, na.rm = TRUE))
            stop("requires a non-negative response vector")

	residuals_type <- match.arg(tolower(residuals_type), c("pearson", "deviance", "rstandard"))
	if (missing(residuals))
		residuals <- if (residuals_type=="rstandard") rstandard(x)
				else residuals(x, type=residuals_type)
	# reshape the residuals to conform to the structure of data
	residuals <- as.table(tapply(residuals, factors, sum))

	gp <- if (inherits(gp, "grapcon_generator"))
				do.call("gp", c(list(observed, residuals, expected, x$df.residual),
								as.list(gp_args)))
			else gp

	panel(observed, residuals=residuals, expected=expected, type=type,
			residuals_type=residuals_type, gp=gp, ...)
}

