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
  \item{residuals}{ An optional array or vector of residuals corresponding to the cells in the
  data, for example, as calculated by \code{residuals.glm(x)}, \code{residuals.gnm(x)}.}
  \item{\dots}{ Other arguments passed to \code{\link[vcd]{mosaic}} }
  \item{residuals_type}{ The type of residuals, used as a label for the legend in the mosaic
  plot }
}
\details{
  ~~ If necessary, more details than the description above ~~
}
\value{
The \code{structable} visualized by \code{\link[vcd]{mosaic}} is returned invisibly.

}
%\references{ ~put references to the literature/web site here ~ }
\author{Michael Friendly}
\note{
The current version is BUGGED -- residuals are not shaded correctly.
The examples below are intended simply to demonstrate the bug. 
 
}
\seealso{\code{\link[stats]{glm}}, \code{\link[gnm]{gnm}}, \code{\link[vcd]{plot.loglm}}, \code{\link[vcd]{mosaic}}}
\examples{
GSStab <- xtabs(count ~ sex + party, data=GSS)
# using the data in table form
mod.glm1 <- glm(Freq ~ sex + party, family = poisson, data = GSStab)
res <- residuals(mod.glm1)    
std <- rstandard(mod.glm1)

# For mosaic.default(), need to re-shape residuals to conform to data
stdtab <- array(std, dim=dim(GSStab), dimnames=dimnames(GSStab))
mosaic(GSStab, gp=shading_Friendly, residuals=stdtab, residuals_type="Std\nresiduals", 
       labeling = labeling_residuals)

## BUGGED
mosaic.glm(mod.glm1, residuals=std, labeling = labeling_residuals, shade=TRUE)

}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{hplot}
\keyword{models}
\keyword{multivariate}
% __ONLY ONE__ keyword per line
