\name{Dyke}
\Rdversion{1.1}
\alias{Dyke}
\docType{data}
\title{Sources of knowledge of cancer}
\description{Observational data on a sample of 1729 individuals, cross-classified in a 2^5 table according to their sources of 
information (read newspapers, listen to the radio, do 'solid' reading, attend lectures) and whether they have
good or poor knowledge regarding cancer.  Knowledge of cancer is often treated as the response.}

\usage{
data(Dyke)
}
\format{
  A     5-dimensional array resulting from cross-tabulating    5 variables for  1729 observations. The variable names and their levels are:

  \tabular{rll}{
    No \tab Name \tab Levels \cr
    1\tab \code{Knowledge}\tab \code{"Good", "Poor"}\cr
    2\tab \code{Reading}\tab \code{"No", "Yes"}\cr
    3\tab \code{Radio}\tab \code{"No", "Yes"}\cr
    4\tab \code{Lectures}\tab \code{"No", "Yes"}\cr
    5\tab \code{Newspaper}\tab \code{"No", "Yes"}\cr
  }
}

%\details{ }
\source{

%  \cite{Fienberg:80 [Table 5-6]}
Fienberg, S. E. (1980).
\emph{The Analysis of Cross-Classified Categorical Data}
Cambridge, MA: MIT Press, p. 85, Table 5-6.

}

\references{
Dyke, G. V. and Patterson, H. D. (1952).
Analysis of factorial arrangements when the data are proportions.
\emph{Biometrics}, 8, 1-12.

Lindsey, J. K. (1993). 
\emph{Models for Repeated Measurements}
Oxford, UK: Oxford University Press, p. 57.

}

%\seealso{ }
\examples{
data(Dyke)

# independence model
mosaic(Dyke, shade=TRUE)

# baseline model, Knowledge as response
dyke.mod0 <- loglm(~ Knowledge + (Reading * Radio * Lectures * Newspaper), data=Dyke)
dyke.mod0

mosaic(dyke.mod0)

}

\keyword{datasets}
