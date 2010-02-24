\name{Abortion}
\alias{Abortion}
\docType{data}
\title{Abortion Opinion Data}
\description{Opinions about abortion classified by gender and SES}

\usage{
data(Abortion)
}
\format{
  A     3-dimensional array resulting from cross-tabulating    3 variables for  1100 observations. The variable names and their levels are:

  \tabular{rll}{
    No \tab Name \tab Levels \cr
    1\tab \code{Sex}\tab \code{"Female", "Male"}\cr
    2\tab \code{Status}\tab \code{"Lo", "Hi"}\cr
    3\tab \code{Support_Abortion}\tab \code{"Yes", "No"}\cr
  }
}

\details{
The combinations of \code{Sex} and \code{Status} represent four independent samples, having
fixed \code{Sex}-\code{Status} marginal totals. 
Thus the \code{Sex:Status} association must be included in any loglinear model.
\code{Support_Abortion} is a natural response variable. }
\source{

%  \cite{Christensen:90 [p. 92]}
Christensen, R. (1990).
 \emph{Log-Linear Models},
 New York, NY: Springer-Verlag, p. 92, Example 3.5.2.

Christensen, R. (1997).
 \emph{Log-Linear Models and Logistic Regression},  
 New York, NY: Springer, p. 100, Example 3.5.2.

}

%\references{
%}

%\seealso { }
\examples{
data(Abortion)

# example goes here
ftable(Abortion)
mosaic(Abortion, shade=TRUE)
}

\keyword{datasets}
