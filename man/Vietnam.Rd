\name{Vietnam}
\alias{Vietnam}
\docType{data}
\title{
Student Opinion about the Vietnam War
}
\description{
A survey of student opinion on the Vietnam War was taken at the
University of North Carolina at Chapel Hill in May 1967 and published
in the student newspaper. Students were asked to fill in ballot
papers stating which policy out of A,B,C or D they supported.
Responses were cross-classified by gender/year.

The response categories were:
\describe{
 \item{\code{A}}{Defeat North Vietnam by widespread bombing and land invasion}
 \item{\code{B}}{Maintain the present policy}
 \item{\code{C}}{De-escalate military activity, stop bombing and begin negotiations}
 \item{\code{D}}{Withdraw military forces Immediately}
}

}
\usage{data(Vietnam)}
\format{
  A frequency data frame with 40 observations representing a 2 x 5 x 4 contingency table
   on the following 4 variables.
  \describe{
    \item{\code{sex}}{a factor with levels \code{Female} \code{Male}}
    \item{\code{year}}{year of study, an ordered factor with levels
       \code{Freshmen}, \code{Sophomore}, \code{Junior}, \code{Senior}, \code{Grad student}}
    \item{\code{response}}{a factor with levels \code{A} \code{B} \code{C} \code{D}}
    \item{\code{Freq}}{cell frequency, a numeric vector}
  }
}
\details{
For some analyses, it is useful to treat \code{year} as numeric,
and possibly assign grad students a value \code{year=7}.
}
\source{
Aitken, M. etal, 1989,  \emph{Statistical Modelling in GLIM}
}
\references{
Friendly, M.  (2000), \emph{Visualizing Categorical Data}, SAS Institute, Cary, NC,
Example 7.9.

}
\examples{
data(Vietnam)
## maybe str(Vietnam) ; plot(Vietnam) ...
}
\keyword{datasets}
