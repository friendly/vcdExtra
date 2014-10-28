\name{Summarise}
\alias{Summarise}
\alias{Summarise.glmlist}
\alias{Summarise.default}

%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Brief Summary of Model Fit for glm and loglm Models
}
\description{
For \code{glm} objects, the \code{print} and \code{summary} methods give
too much information if all one wants to see is a brief summary of model
goodness of fit, and there is no easy way to display a compact comparison of
model goodness of fit for a collection of models fit to the same data.
All \code{loglm} models have equivalent glm forms, but the 
\code{print} and \code{summary} methods give quite different results.

\code{Summarise} provides a brief summary for one or more models
fit to the same dataset
for which \code{logLik} and \code{nobs} methods exist
(e.g., \code{glm} and \code{loglm} models).
%This implementation is experimental, and is subject to change.

}
\usage{
Summarise(object, ...)

\method{Summarise}{glmlist}(object, ..., saturated = NULL, sortby = NULL)
\method{Summarise}{default}(object, ..., saturated = NULL, sortby = NULL)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{object}{
a fitted model object for which there exists a logLik method to extract the corresponding log-likelihood}
  \item{\dots}{
optionally more fitted model objects
}
  \item{saturated}{
saturated model log likelihood reference value (use 0 if deviance is not available)
}
  \item{sortby}{
either a numeric or character string specifying
the column in the result by which the rows are sorted (in decreasing order)}
}
\details{
The function relies on residual degrees of freedom for the LR chisq test being available
in the model object.  This is true for objects inheriting from 
\code{lm}, \code{glm}, \code{loglm}, \code{polr}
and \code{negbin}.
}
\value{
A data frame (also of class \code{anova}) with columns 
\code{c("AIC", "BIC", "LR Chisq", "Df", "Pr(>Chisq)")}.
Row names are taken from the names of the model object(s).
}
%\references{
%% ~put references to the literature/web site here ~
%}
\author{
Achim Zeileis
}
%\note{
%%  ~~further notes~~
%}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
\code{\link[stats]{logLik}}, \code{\link[stats]{glm}}, \code{\link[MASS]{loglm}}, 

\code{\link{logLik.loglm}}, \code{\link{modFit}}
}
\examples{
data(Mental)
indep <- glm(Freq ~ mental+ses,
                family = poisson, data = Mental)
Summarise(indep)
Cscore <- as.numeric(Mental$ses)
Rscore <- as.numeric(Mental$mental)

coleff <- glm(Freq ~ mental + ses + Rscore:ses,
                family = poisson, data = Mental)
roweff <- glm(Freq ~ mental + ses + mental:Cscore,
                family = poisson, data = Mental)
linlin <- glm(Freq ~ mental + ses + Rscore:Cscore,
                family = poisson, data = Mental)
                
# compare models
Summarise(indep, coleff, roweff, linlin)

}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{models}
%\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
