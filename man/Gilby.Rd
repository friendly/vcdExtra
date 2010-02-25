\name{Gilby}
\alias{Gilby}
\docType{data}
\title{Clothing and Intelligence Rating of Children}
\description{Schoolboys were classified according to their clothing and to their teachers rating of "dullness"
(lack of intelligence), in a 5 x 7 table originally from 
Gilby (1911).
Anscombe (1981) presents a slightly collapsed 4 x 6 table, used here, where 
the last two categories of clothing were pooled as were the first two categories of dullness due to small counts.

Both \code{Dullnes} and \code{Clothing} are ordered categories, so models and methods
that examine their association in terms of ordinal categories are profitable.
}

\usage{
data(Gilby)
}
\format{
  A     2-dimensional array resulting from cross-tabulating    2 variables for  1725 observations. The variable names and their levels are:

  \tabular{rll}{
    No \tab Name \tab Levels \cr
    1\tab \code{Dullness}\tab \code{"Ment. defective", "Slow", "Slow Intell", "Fairly Intell", "Capable", "V.Able"}\cr
    2\tab \code{Clothing}\tab \code{"V.Well clad", "Well clad", "Passable", "Insufficient"}\cr
  }
}

%\details{ }
\source{
Anscombe, F. J. (1981).
\emph{Computing in Statistical Science Through APL}.
New York: Springer-Verlag, p. 302

}

\references{
%  \cite{Gilby & Pearson 1911, from Anscombe 1981, p 302}
Gilby, W. H. (1911).
   On the significance of the teacher's appreciation of
   general intelligence.  \emph{Biometrika}, 8, 93-108 (esp. p. 94).
   [Quoted by Kendall (1943,..., 1953) Table 13.1, p 320.]
}

%\seealso{ }
\examples{
data(Gilby)

mosaic(Gilby, shade=TRUE)

# correspondence analysis to see relations among categories
if(require(ca)){
	ca(Gilby)
	plot(ca(Gilby))
	title(xlab="Dimension 1", ylab="Dimension 2")
}


}

\keyword{datasets}
