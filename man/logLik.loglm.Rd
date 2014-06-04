\name{logLik.loglm}
\alias{logLik.loglm}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Log-Likelihood of a loglm Object
}
\description{
Calculates the log-likelihood value of the \code{loglm} model represented by \code{object} 
evaluated at the estimated coefficients.

It allows the use of \code{\link[stats]{AIC}} and \code{\link[stats]{AIC}}, which require
that a \code{logLik} method exists to extract the corresponding log-likelihood for the 
model.
}
\usage{
\method{logLik}{loglm}(object, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{object}{
A \code{loglm} object
}
  \item{\dots}{
For compatibility with the S3 generic; not used here
}
}
\details{
If cell frequencies have not been stored with the \code{loglm} object
(via the argument \code{keep.frequencies = TRUE}), they are obtained
using \code{update}.

This function calculates the log-likelihood in a way that allows for non-integer
frequencies, such as the case where 0.5 has been added to all cell frequicies
to allow for sampling zeros.

For integer frequencies, it gives the same result as the corresponding
model fit using \code{\link[stats]{glm}}, whereas \code{\link[stats]{glm}}
returns \code{-Inf} if there are any non-integer frequencies.

}
\value{
Returns an object of class \code{logLik}. 
This is a number with one attribute, \code{"df"} (degrees of freedom), 
giving the number of (estimated) parameters in the model. }

%\references{
%%% ~put references to the literature/web site here ~
%}
\author{
Achim Zeileis
}
%\note{
%%%  ~~further notes~~
%}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
\code{\link[MASS]{loglm}}, \code{\link[stats]{AIC}}, \code{\link[stats]{BIC}},
}
\examples{
data(Titanic, package="datasets")  

require(MASS)
titanic.mod1 <- loglm(~ (Class * Age * Sex) + Survived, data=Titanic)
titanic.mod2 <- loglm(~ (Class * Age * Sex) + Survived*(Class + Age + Sex), data=Titanic)
titanic.mod3 <- loglm(~ (Class * Age * Sex) + Survived*(Class + Age * Sex), data=Titanic)

logLik(titanic.mod1)
AIC(titanic.mod1, titanic.mod2, titanic.mod3)
BIC(titanic.mod1, titanic.mod2, titanic.mod3)

# compare with models fit using glm()
titanic <- as.data.frame(Titanic)
titanic.glm1 <- glm(Freq ~ (Class * Age * Sex) + Survived, 
                    data=titanic, family=poisson)
titanic.glm2 <- glm(Freq ~ (Class * Age * Sex) + Survived*(Class + Age + Sex), 
                    data=titanic, family=poisson)
titanic.glm3 <- glm(Freq ~ (Class * Age * Sex) + Survived*(Class + Age * Sex), 
                    data=titanic, family=poisson)

logLik(titanic.glm1)
AIC(titanic.glm1, titanic.glm2, titanic.glm3)
BIC(titanic.glm1, titanic.glm2, titanic.glm3)

}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{models}
\keyword{htest}% __ONLY ONE__ keyword per line
