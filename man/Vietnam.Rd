\name{Vietnam}
\alias{Vietnam}
\docType{data}
\title{Student opinion about the war in Vietnam}
\description{
A survey of student opinion on the Vietnam War was taken at the
University of North Carolina at Chapel Hill in May 1967 and published
in the student newspaper. Students were asked to fill in ballot
papers stating which policy out of A,B,C or D they supported.
Responses were cross-classified by gender/year.

The result is a 2 x 5 x 4 contingency table, with factors
\code{sex}, \code{yr} and \code{resp}.

}
\usage{data(Vietnam)}
\format{
  A data frame in frequency form with 40 observations on the following 4 variables.
  \describe{
    \item{\code{sex}}{a factor with levels \code{F} \code{M}}
    \item{\code{yr}}{an ordered factor with levels \code{Fresh} < \code{Soph} < \code{Jr} < \code{Sr} < \code{Grad}}
    \item{\code{resp}}{a factor with levels \code{A} \code{B} \code{C} \code{D}}
    \item{\code{Freq}}{a numeric vector}
  }
}
\details{
The response categories can be considered ordered in terms of attitude about the war
in Vietnam:
 \describe{
  \item{A}{defeat North Vietnam by widespread bombing and land invasion}
  \item{B}{follow the present policy}
  \item{C}{withdraw troops to strong points and open negotiations on elections involving the Viet Cong}
  \item{D}{immediate withdrawal of all U.S. troops}
  }
}
\source{
Aitkin, M. A., Anderson, D., Francis, B., & Hinde, J. 
\emph{Statistical Modelling in GLIM},
 Clarendon Press, 1989

}
\references{
Friendly, M. \emph{Visualizing Categorical Data}, Cary, NC: SAS Institute, 2000, Appendix B.12.

}
\examples{
data(Vietnam)
str(Vietnam) 
# make it into a table
Vietnam.table <- xtabs(Freq ~ sex+yr+resp, data=Vietnam)
structable(Vietnam.table)
}
\keyword{datasets}
