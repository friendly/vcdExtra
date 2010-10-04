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

        if (is.null(formula) && inherits(observed, "table"))
            formula <- reformulate(names(dimnames(observed)))

        if (is.null(formula)) {
            if (is.environment(observed)) observed <- model.frame(x)
            else {
                if (inherits(observed, "table"))
                    observed <- as.data.frame(observed)
                if (!is.null(x$call$subset))
                    observed <- subset(observed, eval(x$call$subset, observed))
                if (!is.null(x$na.action))
                    observed <- observed[-x$na.action,]
            }
            ## get all factors excluding response
            factors <- sapply(observed, inherits, "factor")
            resp <- as.character(x$formula[[2]])
            factors <- observed[setdiff(colnames(observed[factors]), resp)]
            ## drop unused levels
            for(nm in names(factors)) {
                f <- factors[[nm]]
                if(is.factor(f) &&
                   length(unique(f[!is.na(f)])) < length(levels(f)))
                    factors[[nm]] <- factors[[nm]][, drop = TRUE]
            }
            ok <- TRUE
            ## check cross-classifying
            per.cell <- tapply(observed[,1], factors, length)
            if (ok <- isTRUE(all(per.cell == 1))) {
              warning("no formula provided, assuming ", deparse(formula),
                      "\n", call. = FALSE)
            }
            if (!ok)
              stop("cannot identify indexing factors from ", substitute(x),
                   "$data - please provide formula", call. = FALSE)
        }
        else {
            if (length(formula) == 3) formula <- formula[-2]
            ## get indexing factors allowing for missing data, subset etc
            factors <- do.call("model.frame", list(formula = formula, data = observed,
                                                   subset = x$call$subset,
                                                   na.action = na.pass))
            ## following loop needed due to bug in model.frame.default (fixed for R 2.12)
            for(nm in names(factors)) {
                f <- factors[[nm]]
                if(is.factor(f) && length(unique(f[!is.na(f)])) < length(levels(f)))
                    factors[[nm]] <- factors[[nm]][, drop = TRUE]
            }
            if (!is.null(x$na.action))
                factors <- factors[-x$na.action,]
        }

        observed <- as.table(tapply(x$y, factors, sum))
	expected <- as.table(tapply(fitted(x), factors, sum))

	type <- match.arg(tolower(type), c("observed", "expected"))
	if (any(observed < 0, na.rm = TRUE))
            stop("requires a non-negative response vector")

        ## reshape the residuals to conform to the structure of data

        ## if one residual per cell, use residuals_type
        if (nlevels(interaction(factors)) == length(x$y)) {
            residuals_type <- match.arg(tolower(residuals_type),
                                        c("pearson", "deviance", "rstandard"))
            if (missing(residuals))
                residuals <- if (residuals_type=="rstandard") rstandard(x)
                else residuals(x, type=residuals_type)
            residuals <- as.table(tapply(residuals, factors, sum))
        }
        ## for marginal views, use aggregated working residuals
        else {
            if (missing(residuals)) residuals(x, type = "working")
            residuals <- meanResiduals(x, factors)
            residuals_type <- "working" #what is this used for?
        }

	gp <- if (inherits(gp, "grapcon_generator"))
				do.call("gp", c(list(observed, residuals, expected, x$df.residual),
								as.list(gp_args)))
			else gp

	panel(observed, residuals=residuals, expected=expected, type=type,
			residuals_type=residuals_type, gp=gp, ...)
}

## convenience functions for sieve and assoc plots
sieve.glm <-
		function (x, ...)
{
	mosaic(x, panel = sieve, ...)
}

assoc.glm <-
		function (x, ...)
{
	mosaic(x, panel = assoc, ...)
}

