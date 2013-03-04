\name{print.Kappa}
\alias{print.Kappa}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ Print Kappa }
\description{
  This is a replacement for the \code{print.Kappa} method in \code{vcd},
  adding display of \code{z} values to the \code{vcd} version and
  optional confidence intervals.
}
\usage{
\method{print}{Kappa}(x, digits=max(getOption("digits") - 3, 3), CI=FALSE, level=0.95,  ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{ A Kappa object}
  \item{digits}{number of digits to print}
  \item{CI}{Include confidence intervals in the display?}
  \item{level}{confidence level}
  \item{\dots}{ Other arguments }
}
%\details{
%  ~~ If necessary, more details than the description above ~~
%}
\value{
  Returns the Kappa object, invisibly.
}
%\references{ ~put references to the literature/web site here ~ }
\author{ Michael Friendly}

\seealso{ 
\code{\link[vcd]{confint.Kappa}}
}
\examples{
data("SexualFun")
Kappa(SexualFun)
print(Kappa(SexualFun), CI=TRUE)

# stratified 3-way table
apply(MSPatients, 3, Kappa)
}

% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{htest}
\keyword{category}
