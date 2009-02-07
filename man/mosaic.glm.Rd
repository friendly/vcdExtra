\name{mosaic.glm}
\alias{mosaic.glm}
\alias{mosaic.gnm}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Mosaic plots for fitted generalized linear and generalized nonlinear models }
\description{
Procduces mosaic plots (and other plots in the \code{\link[vcd]{strucplot}} framework) 
for a log-linear model fitted with \code{\link[stats]{glm}}
or for a generalized nonlinear model fitted with \code{\link[gnm]{gnm}}.
These methods apply only to models with discrete factors.
}
\usage{
%\method{mosaic}{glm}(x, residuals = NULL, ..., residuals_type = "deviance")
\method{mosaic}{glm}(x, panel = mosaic, type = c("observed", "expected"), residuals = NULL, 
residuals_type = c("pearson", "deviance", "rstandard"), gp = shading_hcl, gp_args = list(), ...)

\method{mosaic}{gnm}(x, panel = mosaic, type = c("observed", "expected"), residuals = NULL, 
residuals_type = c("pearson", "deviance", "rstandard"), gp = shading_hcl, gp_args = list(), ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{ A \code{glm} or \code{gnm} object }
   \item{panel}{Panel function used to draw the plot for visualizing the observed values, residuals 
   and expected values. Currently, one of \code{"mosaic"}, \code{"assoc"}, or \code{"sieve"}
   in \code{vcd}.}
  \item{type}{A character string indicating whether the 
  \code{"observed"} or the \code{"expected"} values of the table should be visualized
  by the area of the tiles or bars.}
  \item{residuals}{ An optional array or vector of residuals corresponding to the cells in the
  data, for example, as calculated by \code{residuals.glm(x)}, \code{residuals.gnm(x)}.}
  \item{residuals_type}{If the \code{residuals} argument is \code{NULL}, residuals are calculated internally
  and used in the display.  In this case, \code{residual_type} can be \code{"pearson"}, 
  \code{"deviance"} or \code{"rstandard"}.  Otherwise (when \code{residuals} is supplied),
  \code{residuals_type} is used as a label for the legend in the 
  plot.  
   }
  \item{gp}{Object of class \code{"gpar"}, shading function or a corresponding generating function (see details and shadings). 
  Ignored if shade = FALSE.}
  \item{gp_args}{A list of arguments for the shading-generating function, if specified.}
  \item{\dots}{ Other arguments passed to the \code{panel} function e.g., \code{\link[vcd]{mosaic}} }
}
\details{
For both poisson family generalized linear models and loglinear models, standardized residuals
provided by \code{rstandard}
(sometimes called adjusted residuals) are often preferred because they have 
constant unit asymptotic variance
}
\value{
The \code{structable} visualized by \code{\link[vcd]{mosaic}} is returned invisibly.

}
%\references{ ~put references to the literature/web site here ~ }
\author{Michael Friendly, with help from Achim Zeileis}
%\note{
%}
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


# Using externally calculated residuals
mosaic.glm(mod.glm1, residuals=std, labeling = labeling_residuals, shade=TRUE)

# Using residuals_type
mosaic.glm(mod.glm1, residuals_type="rstandard", labeling = labeling_residuals, shade=TRUE)

}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{hplot}
\keyword{models}
\keyword{multivariate}
% __ONLY ONE__ keyword per line
