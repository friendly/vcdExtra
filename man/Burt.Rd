\name{Burt}
\alias{Burt}
\docType{data}
\title{
Burt (1950) Data on Hair, Eyes, Head and Stature
}
\description{
Cyril Burt (1950) gave these data, on a sample of 100 people from Liverpool,
to illustrate the application of a method of factor analysis (later
called multiple correspondence analysis) applied to 
categorical data.

He presented these data initially in the form that has come to be called a "Burt table",
giving the univariate and bivariate frequencies for an n-way frequency table.

}
\usage{data("Burt")}
\format{
  A frequency data frame (representing a 3 x 3 x 2 x 2 frequency table) with 36 observations on the following 5 variables.
  \describe{
    \item{\code{Hair}}{hair color, a factor with levels \code{Fair} \code{Red} \code{Dark}}
    \item{\code{Eyes}}{eye color, a factor with levels \code{Light} \code{Mixed} \code{Dark}}
    \item{\code{Head}}{head shape, a factor with levels \code{Narrow} \code{Wide}}
    \item{\code{Stature}}{height, a factor with levels \code{Tall} \code{Short}}
    \item{\code{Freq}}{a numeric vector}
  }
}
\details{
Burt says: "In all, 217 individuals were examined, about two-thirds of them males.
But, partly to simplify the calculations and partly because the later observations were rather more trustworthy, 
I shall here restrict my analysis to the data obtained from the last hundred males in the series."

\code{Head} and \code{Stature} reflect a binary coding where people are classified
according to whether they are below or above the average for the population.
}
\source{
Burt, C. (1950). The factorial analysis of qualitative data, 
\emph{British Journal of Statistical Psychology}, 3(3), 166-185.
Table IX.
}
\references{
%%  ~~ possibly secondary sources and usages ~~
}
\examples{
data(Burt)
mosaic(Freq ~ Hair + Eyes + Head + Stature, data=Burt, shade=TRUE)

#or
burt.tab <- xtabs(Freq ~ Hair + Eyes + Head + Stature, data=Burt)
mosaic(burt.tab, shade=TRUE)
}
\keyword{datasets}
