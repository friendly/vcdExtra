\name{Vision}
\alias{Vision}
\docType{data}
\title{Visual acuity in left and right eyes for men and women}
\description{
A 2 x 4 x 4 table in frequency form, for a large sample of men and women,
aged 30--39 who worked in the U.K. Royal Ordnance Factory during World War II. 
For each person, unaided distance visual acuity was tested for their
right and left eyes, and categorized into four ordered grades, from
1 (Low) to 4 (High).
}
\usage{data(Vision)}
\format{
  A data frame, representing a 2 x 4 x 4 table in frequency form, with 32 observations on the following 4 variables.
  \describe{
    \item{\code{gender}}{a factor with levels \code{F} \code{M}}
    \item{\code{right}}{Right eye grade: an ordered factor with levels \code{1} < \code{2} < \code{3} < \code{4}}
    \item{\code{left}}{Right eye grade: an ordered factor with levels \code{1} < \code{2} < \code{3} < \code{4}}
    \item{\code{Freq}}{Cell frequency: a numeric vector}
  }
}
\details{
  ~~ If necessary, more details than the description above ~~
}
\source{
Kendall, M. G. & Stuart, A. \emph{The Advanced Theory of Statistics}, Griffin, 1961, vol. 2,
Tables 33.2 and 33.5.

}
\references{
Bishop, Y. M. M. Fienberg, S. E. & Holland, P. W. Discrete Multivariate Analysis: Theory and Practice MIT Press, 1975,
p. 284.

Friendly, M. \emph{Visualizing Categorical Data}, Cary, NC: SAS Institute, 2000, Appendix B.13.

}
\examples{
data(Vision)
## maybe str(Vision) ; plot(Vision) ...
}
\keyword{datasets}
