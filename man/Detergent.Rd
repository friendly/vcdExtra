\name{Detergent}
\Rdversion{1.1}
\alias{Detergent}
\docType{data}
\title{Detergent preference data}
\description{Cross-classification of a sample of 1008 consumers according to 
(a) the softness of the laundry water used, (b) previous use of detergent Brand M, (c) the termperature of laundry water used and (d) expressed preference for Brand X or Brand M in a blind trial.}

\usage{
data(Detergent)
}
\format{
  A     4-dimensional array resulting from cross-tabulating    4 variables for  1008 observations. The variable names and their levels are:

  \tabular{rll}{
    No \tab Name \tab Levels \cr
    1\tab \code{Temperature}\tab \code{"High", "Low"}\cr
    2\tab \code{M_User}\tab \code{"Yes", "No"}\cr
    3\tab \code{Preference}\tab \code{"Brand X", "Brand M"}\cr
    4\tab \code{Water_softness}\tab \code{"Soft", "Medium", "Hard"}\cr
  }
}

%\details{ }
\source{

%  \cite{Fienberg:80 [p. 71]}
Fienberg, S. E. (1980).
\emph{The Analysis of Cross-Classified Categorical Data}
Cambridge, MA: MIT Press, p. 71.


}

\references{
%  \cite{RiesSmith:63}
Ries, P. N. & Smith, H. (1963).
The use of chi-square for preference testing in multidimensional problems. 
\emph{Chemical Engineering Progress}, 59, 39-43.

}

%\seealso{ }
\examples{
data(Detergent)

# example goes here
mosaic(Detergent, shade=TRUE)

require(MASS)
(det.mod0 <- loglm(~ Preference + Temperature + M_User + Water_softness, data=Detergent))
# examine addition of two-way terms
add1(det.mod0, ~ .^2, test="Chisq")

# model for Preference as a response
(det.mod1 <- loglm(~ Preference + (Temperature * M_User * Water_softness), data=Detergent))
mosaic(det.mod0)


}

\keyword{datasets}
