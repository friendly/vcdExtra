\name{Bartlett}
\alias{Bartlett}
\docType{data}
\title{Bartlett data on plum root cuttings}
\description{In an experiment to investigate the effect of cuitting length (two levels) and planting time (two levels) 
on the survival of plum root cuttings, 240 cuttings were planted for each of the 2 x 2 combinations of these factors, 
and their survival was later recorded.  
Bartlett (1935) used these data to illustrate a method for testing for no three-way interaction in a contingency table.}

\usage{
data(Bartlett)
}
\format{
  A     3-dimensional array resulting from cross-tabulating    3 variables for   960 observations. The variable names and their levels are:

  \tabular{rll}{
    No \tab Name \tab Levels \cr
    1\tab \code{Alive}\tab \code{"Alive", "Dead"}\cr
    2\tab \code{Time}\tab \code{"Now", "Spring"}\cr
    3\tab \code{Length}\tab \code{"Long", "Short"}\cr
  }
}

%\details { }
\source{

%  \cite{Hand-etal:94 [p.15 #19]}
Hand, D. and Daly, F. and Lunn, A. D.and McConway, K. J. and Ostrowski, E. (1994).
\emph{A Handbook of Small Data Sets}.  London: Chapman & Hall, p. 15, # 19.

}

\references{
% \cite{Bartlett:35}
 Bartlett, M. S. (1935).
 Contingency Table Interactions 
 \emph{Journal of the Royal Statistical Society}, Supplement, 1935, 2, 248-252.
 
}

%\seealso { }
\examples{
data(Bartlett)

# example goes here
fourfold(Bartlett, mfrow=c(1,2))

mosaic(Bartlett, shade=TRUE)

}

\keyword{datasets}
