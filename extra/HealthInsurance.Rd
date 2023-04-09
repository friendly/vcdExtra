\name{HealthInsurance}
\alias{HealthInsurance}
\docType{data}
\title{
Choice of Health Insurance Product
}
\description{
A company recently introduced a new health insurance provider for its employees. At the beginning of the year the employees had to choose one of three (or four)
different health plan products from this provider to best suit their needs.

This dataset was modified for the present purposes by adding a 4th choice
}
\usage{data("HealthInsurance")}
\format{
  A data frame with 1448 observations on the following 7 variables.
  \describe{
    \item{\code{product}}{Choice among three products, a factor with levels \code{A} \code{B} \code{C}}
    \item{\code{product4}}{Choice among four products, a factor with levels \code{A} \code{B} \code{C} \code{D}}
    \item{\code{age}}{The age of the individual, a numeric vector}
    \item{\code{household}}{The number of people living with the individual in the same household, a numeric vector}
    \item{\code{position_level}}{a numeric vector}
    \item{\code{gender}}{The gender of the individual, a factor with levels \code{Female} \code{Male}}
    \item{\code{absent}}{The number of days the individual was absent from work in the year prior to the choice, a numeric vector}
  }
}
\details{
%%  ~~ If necessary, more details than the __description__ above ~~
}
\source{
Originally taken from \emph{Handbook of Regression Modeling in People Analytics},
\url{https://peopleanalytics-regression-book.org/data/health_insurance.csv}.
}
\references{
%%  ~~ possibly secondary sources and usages ~~
}
\examples{
data(HealthInsurance)
## maybe str(HealthInsurance) ; plot(HealthInsurance) ...
}
\keyword{datasets}
