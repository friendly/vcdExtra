\name{Caesar}
\alias{Caesar}
\docType{data}
\title{Risk Factors for Infection in Caesarian Births}
\description{Data from infection from birth by Caesarian section, classified by \code{Risk} (two levels), 
whether \code{Antibiotics} were used (two levels) and whether the Caesarian section was \code{Planned} or not.  
The outcome is \code{Infection} (three levels).}

\usage{
data(Caesar)
}
\format{
  A     4-dimensional array resulting from cross-tabulating    4 variables for   251 observations. The variable names and their levels are:

  \tabular{rll}{
    No \tab Name \tab Levels \cr
    1\tab \code{Infection}\tab \code{"Type 1", "Type 2", "None"}\cr
    2\tab \code{Risk}\tab \code{"Yes", "No"}\cr
    3\tab \code{Antibiotics}\tab \code{"Yes", "No"}\cr
    4\tab \code{Planned}\tab \code{"Yes", "No"}\cr
  }
}

%\details { }
\source{

%  \cite{Fahrmeir:94}
Fahrmeir, L. & Tutz, G. (1994).
Multivariate Statistical Modelling Based on Generalized Linear Models 
New York: Springer Verlag.

}

%\references{
%}

\seealso{\code{link[Fahrmeir]{caesar}} for the same data recorded as a frequency data frame
with other variables.}
\examples{
data(Caesar)

# example goes here
}

\keyword{datasets}
