\name{Heckman}
\Rdversion{1.1}
\alias{Heckman}
\docType{data}
\title{Labour Force Participation of Married Women 1967-1971}
\description{1583 married women were surveyed over the years 1967-1971, 
recording whether or not they were employed in the labor force.

The data, originally from Heckman & Willis (1977) provide an example
of modeling longitudinal categorical data, e.g., with markov chain
models for dependence over time.
}

\usage{
data(Heckman)
}
\format{
  A     5-dimensional array resulting from cross-tabulating    5 variables for  1583 observations. The variable names and their levels are:

  \tabular{rll}{
    No \tab Name \tab Levels \cr
    1\tab \code{e1971}\tab \code{"71Yes", "No"}\cr
    2\tab \code{e1970}\tab \code{"70Yes", "No"}\cr
    3\tab \code{e1969}\tab \code{"69Yes", "No"}\cr
    4\tab \code{e1968}\tab \code{"68Yes", "No"}\cr
    5\tab \code{e1967}\tab \code{"67Yes", "No"}\cr
  }
}

\details{
Lindsey (1993) fits an initial set of logistic regression models examining the dependence of
employment in 1971 (\code{e1971}) on successive subsets of the previous years,
 \code{e1970}, \code{e1969}, \dots \code{e1967}.

Alternatively, one can examine markov chain models of first-order (dependence on previous
year), second-order (dependence on previous two years), etc.
}
\source{


%  \cite{Lindsey:93 [p. 185]}
Lindsey, J. K. (1993). 
\emph{Models for Repeated Measurements}
Oxford, UK: Oxford University Press, p. 185.

}

\references{
%  \cite{HeckmanWillis:77}
Heckman, J.J. & Willis, R.J. (1977). 
"A beta-logistic model for the analysis of sequential labor force participation by married women." 
\emph{Journal of Political Economy}, 85: 27-58

}

%\seealso{ }
\examples{
data(Heckman)

# independence model
mosaic(Heckman, shade=TRUE)
# same, as a loglm()
require(MASS)
(heckman.mod0 <- loglm(~ e1971+e1970+e1969+e1968+e1967, data=Heckman))
mosaic(heckman.mod0, main="Independence model")

# first-order markov chain: bad fit
(heckman.mod1 <- loglm(~ e1971*e1970 + e1970*e1969 +e1969*e1968 + e1968*e1967, data=Heckman))
mosaic(heckman.mod1, main="1st order markov chain model")

# second-order markov chain: bad fit
(heckman.mod2 <- loglm(~ e1971*e1970*e1969 + e1970*e1969*e1968 +e1969*e1968*e1967, data=Heckman))
mosaic(heckman.mod2, main="2nd order markov chain model")

# third-order markov chain: fits OK
(heckman.mod3 <- loglm(~ e1971*e1970*e1969*e1968 + e1970*e1969*e1968*e1967, data=Heckman))
mosaic(heckman.mod2, main="3rd order markov chain model")

}

\keyword{datasets}
