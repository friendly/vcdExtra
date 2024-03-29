\name{Heart}
\Rdversion{1.1}
\alias{Heart}
\docType{data}

\title{Sex, Occupation and Heart Disease}
\description{Classification of individuals by gender, occupational category and occurrence of heart disease}

\usage{data(Heart)}

\format{
  A 3-dimensional array resulting from cross-tabulating 3 variables for 21522 observations. The variable names and their levels are:

  \tabular{rll}{
    No \tab Name \tab Levels \cr
    1\tab \code{Disease}\tab \code{"Disease", "None"}\cr
    2\tab \code{Gender}\tab \code{"Male", "Female"}\cr
    3\tab \code{Occup}\tab \code{"Unempl", "WhiteCol", "BlueCol"}\cr
  }
}

%\details{ }
\source{

%  \cite{Karger, 1980}
Karger, (1980).
}

%\references{
%}

%\seealso{ }
\examples{
data(Heart)
str(Heart)

# Display the frequencies for occupational categories.
# Each row is a 2 x 2 table
vcd::structable(Disease + Gender ~ Occup, data=Heart)

# display as fourfold plots
vcd::cotabplot(~ Disease + Gender | Occup, data=Heart, panel = cotab_fourfold)
}

\keyword{datasets}
\concept{2 by 2 tables}
