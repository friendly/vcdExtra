\name{HLtest}
\alias{HosmerLemeshow}
\alias{HLtest}
\alias{plot.HLtest}
\alias{print.HLtest}
\alias{rootogram.HLtest}
\alias{summary.HLtest}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Hosmer-Lemeshow Goodness of Fit Test
}
\description{
The \code{HLtest} function computes the classical Hosmer-Lemeshow (1980) goodness of fit test
for a binomial \code{glm} object in logistic regression

The general idea is to assesses whether or not the observed event rates match expected event rates in subgroups of the 
model population. The Hosmer-Lemeshow test specifically identifies subgroups as the deciles of fitted event values,
or other quantiles as determined by the \code{g} argument.
Given these subgroups, a simple chisquare test on \code{g-2} df is used.

In addition to \code{print} and \code{summary} methods, a \code{plot} method is
supplied to visualize the discrepancies between observed and fitted frequencies.
}
\usage{

HosmerLemeshow(model, g = 10)

HLtest(model, g = 10)

\method{print}{HLtest}(x, ...)
\method{summary}{HLtest}(object, ...)
\method{plot}{HLtest}(x, ...)
\method{rootogram}{HLtest}(x, ...)

}
%- maybe also 'usage' for other objects documented here.
\arguments{

  \item{model}{
A \code{glm} model object in the \code{binomial} family
}
  \item{g}{
Number of groups used to partition the fitted values for the
GOF test.
}


  \item{x, object}{
A \code{HLtest} object
}
  \item{\dots}{
Other arguments passed down to methods
}


}
%\details{
%%%  ~~ If necessary, more details than the description above ~~
%}
\value{
A class \code{HLtest} object  with the following components:
%%  If it is a LIST, use
  \item{table}{A data.frame describing the results of partitioning the data into \code{g}
  	groups with the following columns: \code{cut}, \code{total}, \code{obs}, \code{exp}, \code{chi}}
  \item{chisq}{The chisquared statistics}
  \item{df}{Degrees of freedom}
  \item{p.value}{p value}
  \item{groups}{Number of groups}
  \item{call}{\code{model} call}
%% ...
}
\references{

Hosmer, David W., Lemeshow, Stanley (1980). 
A goodness-of-fit test for multiple logistic regression model.
\emph{Communications in Statistics, Series A}, 9, 1043-1069.

Hosmer, David W., Lemeshow, Stanley (2000). 
\emph{Applied Logistic Regression}, New York: Wiley, ISBN 0-471-61553-6

Lemeshow, S. and Hosmer, D.W.  (1982). 
A review of goodness of fit statistics for use in the development of logistic regression models. 
\emph{American Journal of Epidemiology}, 115(1), 92-106.

}
\author{
Michael Friendly
}
%\note{
%%%  ~~further notes~~
%}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
\code{\link[vcd]{rootogram}}, ~~~
}
\examples{

data(birthwt, package="MASS")
# how to do this without attach?
attach(birthwt)
	race = factor(race, labels = c("white", "black", "other"))
	ptd = factor(ptl > 0)
	ftv = factor(ftv)
	levels(ftv)[-(1:2)] = "2+"
	bwt <- data.frame(low = factor(low), age, lwt, race,
    	smoke = (smoke > 0), ptd, ht = (ht > 0), ui = (ui > 0), ftv)
detach(birthwt)
options(contrasts = c("contr.treatment", "contr.poly"))
BWmod <- glm(low ~ ., family=binomial, data=bwt)

(hlt <- HLtest(BWmod))
str(hlt)
summary(hlt)
plot(hlt)

# basic model
BWmod0 <- glm(low ~ age, family=binomial, data=bwt)
(hlt0 <- HLtest(BWmod0))
str(hlt0)
summary(hlt0)
plot(hlt0)


}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{htest}
%\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
