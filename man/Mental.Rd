\name{Mental}
\alias{Mental}
\docType{data}
\title{ Mental impariment and parents SES}
\description{
A 6 x 4 contingency table representing the cross-classification of
mental health status (\code{mental}) of 1660 young New York residents by
their parents' socioeconomic status (\code{ses}).
}
\usage{data(Mental)}
\format{
  A data frame frequency table with 24 observations on the following 3 variables.
  \describe{
    \item{\code{ses}}{an ordered factor with levels \code{1} < \code{2} < \code{3} < \code{4} < \code{5} < \code{6}}
    \item{\code{mental}}{an ordered factor with levels \code{Well} < \code{Mild} < \code{Moderate} < \code{Impaired}}
    \item{\code{Freq}}{cell frequency: a numeric vector}
  }
}
\details{
Both \code{ses} and \code{mental} can be treated as ordered factors or
integer scores.  For \code{ses}, 1="High" and 6="Low".
}
\source{
Haberman, S. J. 
\emph{The Analysis of Qualitative Data: New Developments}, Academic Press, 1979, Vol. II,
p. 375.

Srole, L.; Langner, T. S.; Michael, S. T.; Kirkpatrick, P.; Opler, M. K. & Rennie, T. A. C. 
\emph{Mental Health in the Metropolis: The Midtown Manhattan Study},
NYU Press, 1978, p. 289
}
\references{
Friendly, M. \emph{Visualizing Categorical Data}, Cary, NC: SAS Institute, 2000, Appendix B.7.
}
\examples{
data(Mental)
str(Mental)
(Mental.tab <- xtabs(Freq ~ ses+mental, data=Mental))

# mosaic and sieve plots
mosaic(Mental.tab, gp=shading_Friendly)
sieve(Mental.tab, gp=shading_Friendly)

library(ca)
plot(ca(Mental.tab), main="Mental impairment & SES")
title(xlab="Dim 1", ylab="Dim 2")

}
\keyword{datasets}
