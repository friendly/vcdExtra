\name{mosaic.glm}
\alias{mosaic.glm}
\alias{mosaic.gnm}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Mosaic plots for fitted generalized linear and generalized nonlinear models }
\description{
Procduces mosaic plots for a log-linear model fitted with \code{\link[stats]{glm}}
or for a generalized nonlinear model fitted with \code{\link[gnm]{gnm}}.
These methods apply only to models with discrete factors.
}
\usage{
\method{mosaic}{glm}(x, residuals = NULL, ..., residuals_type = "deviance")

\method{mosaic}{gnm}(x, residuals = NULL, ..., residuals_type = "deviance")
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{ A \code{glm} or \code{gnm} object }
  \item{residuals}{ ~~Describe \code{residuals} here~~ }
  \item{\dots}{ Other arguments passed to \code{\link[vcd]{mosaic}} }
  \item{residuals_type}{ ~~Describe \code{residuals_type} here~~ }
}
\details{
  ~~ If necessary, more details than the description above ~~
}
\value{
The \code{structable} visualized by \code{\link[vcd]{mosaic}} is returned invisibly.

}
%\references{ ~put references to the literature/web site here ~ }
\author{Michael Friendly}
\note{ ~~further notes~~ 
}
\seealso{\code{\link[stats]{glm}}, \code{\link[gnm]{gnm}}, \code{\link[vcd]{plot.loglm}}, \code{\link[vcd]{mosaic}}}
\examples{
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{hplot}
\keyword{models}
\keyword{multivariate}
% __ONLY ONE__ keyword per line
