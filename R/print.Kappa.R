# Print method for Kappa: Add a column showing z values
## DONE: now set digits
## DONE: now include CI


#' Print Kappa
#'
#' This is a replacement for the `print.Kappa` method in `vcd`,
#' adding display of `z` values to the `vcd` version and optional
#' confidence intervals.
#'
#'
#' @param x A Kappa object
#' @param digits number of digits to print
#' @param CI Include confidence intervals in the display?
#' @param level confidence level
#' @param \dots Other arguments
#' @return Returns the Kappa object, invisibly.
#' @author Michael Friendly
#' @seealso \code{\link[vcd]{confint.Kappa}}
#' @keywords htest category
#' @export
#' @examples
#'
#' data("SexualFun")
#' Kappa(SexualFun)
#' print(Kappa(SexualFun), CI=TRUE)
#'
#' # stratified 3-way table
#' apply(MSPatients, 3, Kappa)
#'
print.Kappa <-
		function (x, digits=max(getOption("digits") - 3, 3), CI=FALSE, level=0.95, ...)
{
	tab <- rbind(x$Unweighted, x$Weighted)
	z <- tab[,1] / tab[,2]
	tab <- cbind(tab, z)
	if (CI) {
		q <-  qnorm((1 + level)/2)
		lower <- tab[,1] - q * tab[,2]
		upper <- tab[,1] + q * tab[,2]
		tab <- cbind(tab, lower, upper)
	}

	rownames(tab) <- names(x)[1:2]
	print(tab, digits=digits, ...)
	invisible(x)
}
