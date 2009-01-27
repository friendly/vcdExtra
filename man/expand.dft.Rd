\name{expand.dft}
\alias{expand.dft}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ Expand a frequency table to case form}
\description{
 Converts a frequency table, given either as a table object or a data frame in frequency form
 to a data frame representing individual observations in the table.
}
\usage{
expand.dft(x, var.names = NULL, freq = "Freq", ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{A table object, or a data frame in frequency form containing factors and one numeric variable
  representing the cell frequency for that combination of factors. }
  \item{var.names}{A list of variable names for the factors, if you wish to override those already in the table}
  \item{freq}{The name of the frequency variable in the table}
  \item{\dots}{Other arguments passed down to \code{type.convert}.  In particular, pay attention to 
  \code{na.strings} (default: \code{na.strings=NA} if there are missing cells) and \code{as.is}
  (default: \code{as.is=FALSE}, converting character vectors to factors).}
}
%\details{
%  ~~ If necessary, more details than the description above ~~
%}
\value{
A data frame containing the factors in the table and as many observations as are
represented by the total of the \code{freq} variable.
}
\references{Posted on R-Help, Jan 20, 2009.
\url{http://tolstoy.newcastle.edu.au/R/e6/help/09/01/1873.html}
}
\author{ Mark Schwarz }
%\note{ ~~further notes~~ 
% ~Make other sections like Warning with \section{Warning }{....} ~
%}
\seealso{ \code{\link[utils]{type.convert}}}
\examples{
library(vcd)
art <- xtabs(~Treatment + Improved, data = Arthritis)
art
artdf <- expand.dft(art)
str(artdf)
}

% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{manip}
\keyword{array}
% __ONLY ONE__ keyword per line
