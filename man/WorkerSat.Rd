\name{WorkerSat}
\alias{WorkerSat}
\docType{data}
\title{
Worker Satisfaction Data
}
\description{
Blue collar workers job satisfaction from large scale investigation in Denmark in 1968 (Andersen, 1991). 
}
\usage{data("WorkerSat")}
\format{
  A frequency data frame with 8 observations on the following 4 variables, representing the 2 x 2 x 2 classification of 715 cases.
  \describe{
    \item{\code{Manage}}{Quality of management, an ordered factor with levels \code{bad} < \code{good}}
    \item{\code{Super}}{Supervisor satisfaction, an ordered factor with levels \code{low} < \code{high}}
    \item{\code{Worker}}{Worker job satisfaction, an ordered factor with levels \code{low} < \code{high}}
    \item{\code{Freq}}{a numeric vector}
  }
}
%\details{
%%%  ~~ If necessary, more details than the __description__ above ~~
%}
\source{
\url{https://onlinecourses.science.psu.edu/stat504/node/131}
}
\references{
Andersen, E. B. (1991)
Statistical Analysis of Categorical Data, 2nd Ed., Springer-Verlag.

}
\examples{
data(WorkerSat)

worker.tab <- xtabs(Freq ~ Worker + Super + Manage, data=WorkerSat)
fourfold(worker.tab)
mosaic(worker.tab, shade=TRUE)

}
\keyword{datasets}
