\name{cutfac}
\alias{cutfac}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Cut a Numeric Variable to a Factor
}
\description{
\code{cutfac} acts like \code{\link[base]{cut}},
dividing the range of \code{x} into intervals and
coding the values in \code{x} according in which interval they fall.
However, it gives nicer labels for the factor levels and 
by default chooses convenient breaks among the values based on deciles.

It is particularly useful for plots in which one wants to make a
numeric variable discrete for the purpose of getting boxplots,
spinograms or mosaic plots.

}
\usage{
cutfac(x, breaks = NULL, q = 10)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{a numeric vector which is to be converted to a factor by cutting
}
  \item{breaks}{
either a numeric vector of two or more unique cut points or a single number (greater than or equal to 2) giving the number of intervals into which \code{x} is to be cut.
}
  \item{q}{
the number of quantile groups used to define \code{breaks}, if that has not been specified.
}
}
\details{
By default, \code{\link[base]{cut}} chooses breaks by equal lengths of the range of \code{x},
whereas \code{cutfac} uses \code{\link[stats]{quantile}} to choose breaks of roughly equal count.
}
\value{
A \code{\link[base]{factor}} corresponding to \code{x} is returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
}
\references{
Friendly, M. and Meyer, D. (2016). 
\emph{Discrete Data Analysis with R: Visualization and Modeling Techniques for Categorical and Count Data}. 
Boca Raton, FL: Chapman & Hall/CRC. \url{http://ddar.datavis.ca}.

}
\author{
Achim Zeileis
}
%\note{
%%%  ~~further notes~~
%}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
\code{\link[base]{cut}}, 
\code{\link[stats]{quantile}}
}
\examples{
if (require(AER)) {
data("NMES1988", package="AER")
nmes <- NMES1988[, c(1, 6:8, 13, 15, 18)]

plot(log(visits+1) ~ cutfac(chronic), data = nmes,
  ylab = "Physician office visits (log scale)", 
  xlab = "Number of chronic conditions", main = "chronic")

plot(log(visits+1) ~ cutfac(hospital, c(0:2, 8)), data = nmes,
  ylab = "Physician office visits (log scale)", 
  xlab = "Number of hospital stays", main = "hospital")
}

%\donttest{
%# countreg not yet on CRAN
%if (require(countreg)) {
%data("CrabSatellites", package = "countreg")
%
%# jittered scatterplot
%plot(jitter(satellites) ~ width, data=CrabSatellites,
%  ylab="Number of satellites (jittered)", xlab="Carapace width",
%  cex.lab=1.25)
%with(CrabSatellites, lines(lowess(width, satellites), col="red", lwd=2))
%
%# boxplot, using deciles
%plot(satellites ~ cutfac(width), data=CrabSatellites,
%     ylab="Number of satellites", xlab="Carapace width (deciles)")
%}


}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{manip}
%\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
