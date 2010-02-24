\name{print.Kappa}
\alias{print.Kappa}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ Print Kappa }
\description{
  This is a replacement for the \code{print.Kappa} method in \code{vcd},
  adding display of \code{z} values to the \code{vcd} version.
}
\usage{
\method{print}{Kappa}(x, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{ A Kappa object}
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

}

% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{htest}
\keyword{category}
