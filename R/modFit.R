##
## One-line summary of model fit for a glm/loglm object
##



#' Brief Summary of Model Fit for a glm or loglm Object
#'
#' Formats a brief summary of model fit for a `glm` or `loglm`
#' object, showing the likelihood ratio Chisq (df) value and or AIC.  Useful
#' for inclusion in a plot title or annotation.
#'
#'
#' @name modFit
#' @aliases modFit modFit.loglm modFit.glm
#' @param x A `glm` or `loglm` object
#' @param \dots Arguments passed down
#' @param stats One or more of `chisq` or `aic`, determining the statistics displayed.
#' @param digits Number of digits after the decimal point in displayed statistics.
#'
#' @return A character string containing the formatted values of the chosen statistics.
#' @author Michael Friendly
#'
#' @seealso  \code{\link{LRstats}}
#'
#' @keywords utilities models
#' @examples
#'
#' data(Mental)
#' require(MASS)
#' (Mental.tab <- xtabs(Freq ~ ses + mental, data=Mental))
#' (Mental.mod <- loglm(~ses + mental, Mental.tab))
#' Mental.mod
#' modFit(Mental.mod)
#'
#' # use to label mosaic()
#' mosaic(Mental.mod, main=paste("Independence model,", modFit(Mental.mod)))
#'
#' @export
`modFit` <-
function(x, ...) UseMethod("modFit")

#' @param stats statistics to print: one or more of `"chisq"`, `"aic"`
#' @param digits number to digits to use in the print method
#' @rdname modFit
#' @export
modFit.glm <- function(x, stats="chisq", digits=2, ...) {
	if (!inherits(x,"glm")) stop("modFit requires a glm object")
	result <- NULL
	if ("chisq" %in% stats)
		result <- paste("G^2(",x$df.residual,")=",
				formatC(x$deviance,digits=digits,format="f"),sep="")
	if ("aic" %in% stats)
		result <- paste(result, " AIC=", formatC(x$aic,digits=digits,format="f"),sep="")
	result
}


#' @rdname modFit
#' @export
modFit.loglm <- function(x, stats="chisq", digits=2, ...) {
	if (!inherits(x,"loglm")) stop("modFit requires a loglm object")
	result <- NULL
	if ("chisq" %in% stats)
		result <- paste("G^2(",x$df,")=",
				formatC(x$deviance,digits=digits,format="f"),sep="")
	if ("aic" %in% stats) {
	    aic<-x$deviance-x$df*2
		result <- paste(result, " AIC=", formatC(aic,digits=digits,format="f"),sep="")
		}
	result
}

