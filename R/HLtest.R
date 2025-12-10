# Functions for Hosmer Lemeshow test
# original function downloaded from 
# http://sas-and-r.blogspot.com/2010/09/example-87-hosmer-and-lemeshow-goodness.html
#
# see also: MKmisc::gof.test for more general versions





#' Hosmer-Lemeshow Goodness of Fit Test
#' 
#' The `HLtest` function computes the classical Hosmer-Lemeshow (1980)
#' goodness of fit test for a binomial `glm` object in logistic regression
#' 
#' The general idea is to assesses whether or not the observed event rates
#' match expected event rates in subgroups of the model population. The
#' Hosmer-Lemeshow test specifically identifies subgroups as the deciles of
#' fitted event values, or other quantiles as determined by the `g`
#' argument. Given these subgroups, a simple chisquare test on `g-2` df is
#' used.
#' 
#' In addition to `print` and `summary` methods, a `plot` method
#' is supplied to visualize the discrepancies between observed and fitted
#' frequencies.
#' 
#' 
#' @aliases HosmerLemeshow HLtest plot.HLtest print.HLtest rootogram.HLtest
#' summary.HLtest
#' @param model A `glm` model object in the `binomial` family
#' @param g Number of groups used to partition the fitted values for the GOF
#' test.
#' @param x,object A `HLtest` object
#' @param \dots Other arguments passed down to methods
#' @return A class `HLtest` object with the following components:
#' \item{table}{A data.frame describing the results of partitioning the data
#' into `g` groups with the following columns: `cut`, `total`,
#' `obs`, `exp`, `chi`} \item{chisq}{The chisquare statistics}
#' \item{df}{Degrees of freedom} \item{p.value}{p value} \item{groups}{Number
#' of groups} \item{call}{`model` call} %% ...
#' @author Michael Friendly
#' @seealso \code{\link[vcd]{rootogram}}, ~~~
#' @family association tests
#' @references
#' 
#' Hosmer, David W., Lemeshow, Stanley (1980).  A goodness-of-fit test for
#' multiple logistic regression model. *Communications in Statistics,
#' Series A*, 9, 1043-1069.
#' 
#' Hosmer, David W., Lemeshow, Stanley (2000).  *Applied Logistic
#' Regression*, New York: Wiley, ISBN 0-471-61553-6
#' 
#' Lemeshow, S. and Hosmer, D.W.  (1982).  A review of goodness of fit
#' statistics for use in the development of logistic regression models.
#' *American Journal of Epidemiology*, 115(1), 92-106.
#' 
#' @importFrom vcd rootogram
#' @keywords htest
#' @examples
#' 
#' 
#' data(birthwt, package="MASS")
#' # how to do this without attach?
#' attach(birthwt)
#' 	race = factor(race, labels = c("white", "black", "other"))
#' 	ptd = factor(ptl > 0)
#' 	ftv = factor(ftv)
#' 	levels(ftv)[-(1:2)] = "2+"
#' 	bwt <- data.frame(low = factor(low), age, lwt, race,
#'     	smoke = (smoke > 0), ptd, ht = (ht > 0), ui = (ui > 0), ftv)
#' detach(birthwt)
#' 
#' options(contrasts = c("contr.treatment", "contr.poly"))
#' BWmod <- glm(low ~ ., family=binomial, data=bwt)
#' 
#' (hlt <- HLtest(BWmod))
#' str(hlt)
#' summary(hlt)
#' plot(hlt)
#' 
#' # basic model
#' BWmod0 <- glm(low ~ age, family=binomial, data=bwt)
#' (hlt0 <- HLtest(BWmod0))
#' str(hlt0)
#' summary(hlt0)
#' plot(hlt0)
#' 
#' 
#' 
#' @export HLtest
HLtest <- HosmerLemeshow <- function(model, g=10) {
	if (!inherits(model, "glm")) stop("requires a binomial family glm")
	if (!family(model)$family == 'binomial') stop("requires a binomial family glm")
	y <- model$y
	yhat <- model$fitted.values
  cutyhat = cut(yhat,
     breaks = quantile(yhat, probs=seq(0, 1, 1/g)), 
     include.lowest=TRUE)
  obs = xtabs(cbind(1 - y, y) ~ cutyhat)
  exp = xtabs(cbind(1 - yhat, yhat) ~ cutyhat)

  chi = (obs - exp)/sqrt(exp)
	# browser()
  table <- data.frame(cut=dimnames(obs)$cutyhat,
                      total= as.numeric(apply(obs, 1, sum)),
                      obs=as.numeric(as.character(obs[,1])),
                      exp=as.numeric(as.character(exp[,1])), 
                      chi=as.numeric(as.character(chi[,1]))
                         )

  rownames(table) <- 1:g
  chisq = sum(chi^2)
  p = 1 - pchisq(chisq, g - 2)
  result <- list(table=table, chisq=chisq, df=g-2, p.value=p, groups=g, call=model$call)
  class(result) <- "HLtest"
  return(result)
}

#' @rdname HLtest
#' @exportS3Method 
print.HLtest <- function(x, ...) {
	heading <- "Hosmer and Lemeshow Goodness-of-Fit Test"
	df <- data.frame("ChiSquare"=x$chisq, df=x$df, "P_value"= x$p.value)
	cat(heading,"\n\n")
	cat("Call:\n")
	print(x$call)
	print(df, row.names=FALSE)
	invisible(x)
}

# Q: how to print **s next to larg chisq components?
#' @rdname HLtest
#' @exportS3Method 
summary.HLtest <- function(object, ...) {
	heading <- "Partition for Hosmer and Lemeshow Goodness-of-Fit Test"
	cat(heading,"\n\n")
	print(object$table)
	print(object)
}

## Q: how to display any large chi residuals on the bars??
#' @rdname HLtest
#' @exportS3Method 
rootogram.HLtest <- function(x, ...) {
	rootogram(as.numeric(x$table$obs), as.numeric(x$table$exp), 
		xlab="Fitted value group", names=1:x$groups, ...) 
}

#' @rdname HLtest
#' @exportS3Method 
plot.HLtest <- function(x, ...) {
	rootogram.HLtest(x, ...)
}


