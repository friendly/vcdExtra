\name{DaytonSurvey}
\alias{DaytonSurvey}
\docType{data}
\title{
Dayton Student Survey on Substance Use
}
\description{
This data, from Agresti (2002), Table 9.1, gives the result of a 1992
survey in Dayton Ohio of  2276 high school seniors on
whether they had ever used 
alcohol, cigarettes and marijuana.

}
\usage{data(DaytonSurvey)}
\format{
  A frequency data frame with 32 observations on the following 6 variables.
  \describe{
    \item{\code{cigarette}}{a factor with levels \code{Yes} \code{No}}
    \item{\code{alcohol}}{a factor with levels \code{Yes} \code{No}}
    \item{\code{marijuana}}{a factor with levels \code{Yes} \code{No}}
    \item{\code{sex}}{a factor with levels \code{female} \code{male}}
    \item{\code{race}}{a factor with levels \code{white} \code{other}}
    \item{\code{Freq}}{a numeric vector}
  }
}
\details{
Agresti uses the letters G (\code{sex}), R (\code{race}),
A (\code{alcohol}), C (\code{cigarette}), M (\code{marijuana}) to refer to the table variables,
and this usage is followed in the examples below.

Background variables include \code{sex} and \code{race} of the
respondent (GR), typically treated as explanatory, so that any
model for the full table should include the term \code{sex:race}.
Models for the reduced table, collapsed over \code{sex} and \code{race}
are not entirely unreasonable, but don't permit the estimation
of the effects of these variables on the responses.

The full 5-way table contains a number of cells with counts of 0 or 1,
as well as many cells with large counts, and even the ACM table
collapsed over GR has some small cell counts.  Consequently, 
residuals for these models in mosaic displays are best represented
as standardized (adjusted) residuals.


}
\source{
Agresti, A. (2002). \emph{Categorical Data Analysis}, 2nd Ed.,
New York: Wiley-Interscience, Table 9.1, p. 362.
}
\references{
Thompson, L. (2009). \emph{R (and S-PLUS) Manual to Accompany Agresti's Categorical Data},
\url{https://home.comcast.net/~lthompson221/Splusdiscrete2.pdf} 
}
\examples{
data(DaytonSurvey)

mod.GR <- glm(Freq ~ . + sex*race, data=DaytonSurvey, family=poisson)  # mutual independence + GR
mod.homog.assoc <- glm(Freq ~ .^2, data=DaytonSurvey, family=poisson)  # homogeneous association

# collapse over sex and race
Dayton.ACM <- aggregate(Freq ~ cigarette+alcohol+marijuana, data=DaytonSurvey, FUN=sum)

}
\keyword{datasets}
