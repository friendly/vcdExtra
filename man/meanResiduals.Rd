\name{meanResiduals}
\alias{meanResiduals}
\title{Average Residuals within Factor Levels}
\description{
  Computes the mean working residual from a model fitted using
  Iterative Weighted Least Squares for each level of a factor or
  interaction of factors.
}
\usage{meanResiduals(object, by, standardized=TRUE, as.table=TRUE, ...)}
\arguments{
  \item{object}{model object for which \code{object$residuals} gives the
    working residuals and \code{object$weights} gives the working weights.}
  \item{by}{a list of factors.}
  \item{standardized}{logical: if \code{TRUE}, the mean residuals are
    standardized to be approximately standard normal.}
  \item{as.table}{logical: if \code{TRUE} the result is returned as a table
    cross-classified by the factors passed to \code{by}.}
  \item{...}{currently ignored}
}
\author{Heather Turner}
\value{If \code{as.table == TRUE}, the mean residuals cross-classified
  by the factors passed to \code{by}, otherwise a vector of mean residuals.
  In either case the returned object has a single attribute,
  \code{"weights"} which gives the weight associated with each
  grouped residual.}
\examples{data(yaish)
## Fit a conditional independence model, leaving out
## the uninformative subtable for dest == 7:
CImodel <- gnm(Freq ~ educ*orig + educ*dest, family = poisson,
               data = yaish, subset = (dest != 7))

## compute mean residuals over origin and destination
meanResiduals(CImodel, model.frame(CImodel)[c("orig", "dest")])

## display mean residuals for origin and destination
mosaic(CImodel, ~orig+dest)

## non-aggregated residuals
res1 <- meanResiduals(CImodel,
                      model.frame(CImodel)[c("educ", "orig", "dest")])

res2 <- residuals(CImodel, type = "pearson")

all.equal(res1[,,], res2[,,])}
\keyword{ models }
\keyword{ regression }
\keyword{ nonlinear }
