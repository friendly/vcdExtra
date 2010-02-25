\name{Mobility}
\alias{Mobility}
\docType{data}
\title{Social Mobility data}
\description{Data on social mobility, recording the occupational category of fathers and their sons. }

\usage{
data(Mobility)
}
\format{
  A     2-dimensional array resulting from cross-tabulating    2 variables for 19912 observations. The variable names and their levels are:

  \tabular{rll}{
    No \tab Name \tab Levels \cr
    1\tab \code{Son's_Occupation}\tab \code{"UpNonMan", "LoNonMan", "UpManual", "LoManual", "Farm"}\cr
    2\tab \code{Father's_Occupation}\tab \code{"UpNonMan", "LoNonMan", "UpManual", "LoManual", "Farm"}\cr
  }
}

%\details{ }
\source{

Falguerolles, A. de and Mathieu, (1988). \emph{COMPSTAT 88}.

 % \cite{FeathermanHauser:78}

Featherman, D. L. and Hauser, R. M.
Occupations and social mobility in the United States. 
\emph{Sociological Microjournal}, 12,
Fiche 62. Copenhagen: Sociological Institute.
}

%\references{
%}

%\seealso{ }
\examples{
data(Mobility)

# example goes here
}

\keyword{datasets}
