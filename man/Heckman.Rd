\name{Heckman}
\alias{Heckman}
\docType{data}
\title{Labour Force Participation of Married Women 1967-1971}
\description{1583 married women were surveyed over the years 1967-1971, 
recording whether or not they were employed in the labor force.}

\usage{
data(Heckman)
}
\format{
  A     5-dimensional array resulting from cross-tabulating    5 variables for  1583 observations. The variable names and their levels are:

  \tabular{rll}{
    No \tab Name \tab Levels \cr
    1\tab \code{1971}\tab \code{"71Yes", "No"}\cr
    2\tab \code{1970}\tab \code{"70Yes", "No"}\cr
    3\tab \code{1969}\tab \code{"69Yes", "No"}\cr
    4\tab \code{1968}\tab \code{"68Yes", "No"}\cr
    5\tab \code{1967}\tab \code{"67Yes", "No"}\cr
  }
}

%\details{ }
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

# example goes here
}

\keyword{datasets}
