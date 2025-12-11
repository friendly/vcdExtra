# logLik method for loglm objects, to allow use of AIC() and BIC()
# with MASS::loglm, giving comparable results to the use of these
# functions with glm(..., family=poisson) models.

# allow for non-integer frequencies
# allow for zero frequencies, with a zero= argument



#' Log-Likelihood of a loglm Object
#'
#' Calculates the log-likelihood value of the `loglm` model represented by
#' `object` evaluated at the estimated coefficients.
#'
#' It allows the use of \code{\link[stats]{AIC}} and \code{\link[stats]{BIC}},
#' which require that a `logLik` method exists to extract the
#' corresponding log-likelihood for the model.
#'
#' If cell frequencies have not been stored with the `loglm` object (via
#' the argument `keep.frequencies = TRUE`), they are obtained using
#' `update`.
#'
#' This function calculates the log-likelihood in a way that allows for
#' non-integer frequencies, such as the case where 0.5 has been added to all
#' cell frequencies to allow for sampling zeros.  If the frequencies still
#' contain zero values, those are replaced by the value of `start`.
#'
#' For integer frequencies, it gives the same result as the corresponding model
#' fit using \code{\link[stats]{glm}}, whereas \code{\link[stats]{glm}} returns
#' `-Inf` if there are any non-integer frequencies.
#'
#' @param object A `loglm` object
#' @param \dots For compatibility with the S3 generic; not used here
#' @param zero value used to replace zero frequencies in calculating the
#' log-likelihood
#' @return Returns an object of class `logLik`.  This is a number with one
#' attribute, `"df"` (degrees of freedom), giving the number of
#' (estimated) parameters in the model.
#' @author Achim Zeileis
#' @seealso \code{\link[MASS]{loglm}}, \code{\link[stats]{AIC}},
#' \code{\link[stats]{BIC}},
#' @keywords models htest
#' @examples
#'
#' data(Titanic, package="datasets")
#'
#' require(MASS)
#' titanic.mod1 <- loglm(~ (Class * Age * Sex) + Survived, data=Titanic)
#' titanic.mod2 <- loglm(~ (Class * Age * Sex) + Survived*(Class + Age + Sex), data=Titanic)
#' titanic.mod3 <- loglm(~ (Class * Age * Sex) + Survived*(Class + Age * Sex), data=Titanic)
#'
#' logLik(titanic.mod1)
#' AIC(titanic.mod1, titanic.mod2, titanic.mod3)
#' BIC(titanic.mod1, titanic.mod2, titanic.mod3)
#'
#' # compare with models fit using glm()
#' titanic <- as.data.frame(Titanic)
#' titanic.glm1 <- glm(Freq ~ (Class * Age * Sex) + Survived,
#'                     data=titanic, family=poisson)
#' titanic.glm2 <- glm(Freq ~ (Class * Age * Sex) + Survived*(Class + Age + Sex),
#'                     data=titanic, family=poisson)
#' titanic.glm3 <- glm(Freq ~ (Class * Age * Sex) + Survived*(Class + Age * Sex),
#'                     data=titanic, family=poisson)
#'
#' logLik(titanic.glm1)
#' AIC(titanic.glm1, titanic.glm2, titanic.glm3)
#' BIC(titanic.glm1, titanic.glm2, titanic.glm3)
#'
#'
#' @export
logLik.loglm <- function(object, ..., zero=1E-10) {
	fr <- if(!is.null(object$frequencies)) unclass(object$frequencies) else {
				unclass(update(object, keep.frequencies = TRUE)$frequencies)
			}
	df <- prod(dim(fr)) - object$df
	if (any(fr==0)) {
		fr <- as.vector(fr)
		fr[fr==0] <- zero
	}
	structure(sum((log(fr) - 1) * fr - lgamma(fr + 1))  - object$deviance/2,
			df = df, class = "logLik")
}


