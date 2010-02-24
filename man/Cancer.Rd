\name{Cancer}
\alias{Cancer}
\docType{data}
\title{Survival of Breast Cancer Patients}
\description{Three year survival of 474 breast cancer patients according to nuclear grade and diagnostic center.}

\usage{
data(Cancer)
}
\format{
  A     3-dimensional array resulting from cross-tabulating    3 variables for   474 observations. The variable names and their levels are:

  \tabular{rll}{
    No \tab Name \tab Levels \cr
    1\tab \code{Survival}\tab \code{"Died", "Surv"}\cr
    2\tab \code{Grade}\tab \code{"Malignant", "Benign"}\cr
    3\tab \code{Center}\tab \code{"Boston", "Glamorgan"}\cr
  }
}

%\details { }
\source{


%  \cite{Lindsey:95 [p38]}
%  \cite{Whittaker:90}
Lindsey, J. K. (1995). 
Analysis of Frequency and Count Data 
Oxford, UK: Oxford University Press. p. 38, Table 2.5.

Whittaker, J. (1990)
Graphical Models in Applied Multivariate Statistics
New York: John Wiley and Sons, p. 220.

}

%\references{
% \cite{Morrison etal}
%}

%\seealso { }
\examples{
data(Cancer)

# example goes here
}

\keyword{datasets}
