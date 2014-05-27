\name{Vote1980}
\alias{Vote1980}
\docType{data}
\title{
Race and Politics in the 1980 Presidential Vote
}
\description{
Data from the 1982 General Social Survey on votes in the 1980 U.S.
presidential election in relation to race and political 
conservatism.
}
\usage{data(Vote1980)}
\format{
  A frequency data frame representing a 2 x 7 x 2 table, with 28 observations on the following 4 variables.
  \describe{
    \item{\code{race}}{a factor with levels \code{NonWhite} \code{White}}
    \item{\code{conservatism}}{
      a factor with levels \code{1} \code{2} \code{3} \code{4} \code{5} \code{6} \code{7},
      \code{1}=most liberal, \code{7}=most conservative}
    \item{\code{votefor}}{a factor with levels \code{Carter} \code{Reagan}; \code{Carter} 
      represents Jimmy Carter or other.}
    \item{\code{Freq}}{a numeric vector}
  }
}
\details{
The data contains a number of sampling zeros in the frequencies of
NonWhites voting for Ronald Reagan.
}
\source{
Clogg, C. & Shockey, J. W. (1988).
In Nesselroade, J. R. & Cattell, R. B. (ed.) 
Multivariate Analysis of Discrete Data,
\emph{Handbook of Multivariate Experimental Psychology}, 
New York: Plenum Press.
}
\references{
Agresti, A. (1990)
\emph{Categorical Data Analysis}, Table 4.12
New York: Wiley-Interscience.

Friendly, M. (2000)
\emph{Visualizing Categorical Data}, Example 7.5
Cary, NC: SAS Institute.
}
\examples{
data(Vote1980)
fourfold(xtabs(Freq ~ race + votefor + conservatism, data=Vote1980), mfrow=c(2,4))

}
\keyword{datasets}
