\name{PhdPubs}
\alias{PhdPubs}
\docType{data}
\title{
Publications of PhD Candidates
}
\description{
A data set giving the number of publications by doctoral candidates in biochemistry
in relation to various predictors, originally from Long (1997).

There is a large number of zero counts. Is there evidence for
a separate group of non-publishers?
}
\usage{data(PhdPubs)}
\format{
  A data frame with 915 observations on the following 6 variables.
  \describe{
    \item{\code{articles}}{number of articles published in the final three years of PhD studies}
    \item{\code{female}}{dummy variable for gender, coded \code{1} for female}
    \item{\code{married}}{dummy variable for marital status, coded \code{1} for married}
    \item{\code{kid5}}{number of young children, age 5 and under}
    \item{\code{phdprestige}}{prestige of the PhD department.  The higher the number the more prestigious the program. }
    \item{\code{mentor}}{number of publications by the mentor in the preceeding three years}
  }
}
\details{
In this version of the data set, \code{phdprestige} had been rounded to the nearest integer.
A Stata version with the continuous values was subsequently found at
\url{https://www.stata-press.com/data/lf2/couart2.dta}

}
\source{
Long, J. S. (1997). \emph{Regression Models for Categorical and Limited Dependent Variables},
Sage.

Long, J. S. & Freese, J. (2006). \emph{Regression Models for Categorical Dependent Variables Using Stata}, 2nd Ed.,
Stata Press.

}
%\references{
%%  ~~ possibly secondary sources and usages ~~
%}
\examples{
data(PhdPubs)
# very uninformative
hist(PhdPubs$articles, 
     breaks=0:19, col="pink", xlim=c(0,20),
     xlab="Number of Articles")

library(vcd)
rootogram(goodfit(PhdPubs$articles), xlab="Number of Articles")

# compare with negative binomial
rootogram(goodfit(PhdPubs$articles, type="nbinomial"), 
	xlab="Number of Articles", main="Negative binomial")


}
\keyword{datasets}
\concept{generalized linear models}
\concept{zero counts}
\concept{Poisson distributions}
