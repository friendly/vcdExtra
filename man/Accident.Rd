\name{Accident}
\alias{Accident}
\docType{data}
\title{
Traffic Accident Victims in France in 1958
}
\description{
Bertin (1983) used these data to illustrate the cross-classification of 
data by numerous variables, each of which could have various types
and could be assigned to various visual attributes.
}
\usage{data(Accident)}
\format{
  A data frame in frequency form (comprising a 5 x 2 x 4 x 2 table)
   with 80 observations on the following 5 variables.
  \describe{
    \item{\code{age}}{an ordered factor with levels \code{0-9} < \code{10-19} < \code{20-29} < \code{30-49} < \code{50+}}
    \item{\code{result}}{a factor with levels \code{Died} \code{Injured}}
    \item{\code{mode}}{mode of transportation,
      a factor with levels \code{4-Wheeled} \code{Bicycle} \code{Motorcycle} \code{Pedestrian}}
    \item{\code{gender}}{a factor with levels \code{Female} \code{Male}}
    \item{\code{Freq}}{a numeric vector}
  }
}
%\details{
%%%  ~~ If necessary, more details than the __description__ above ~~
%}
\source{
Bertin (1983), p. 30; original data from the Ministere des Travaux Publics
}
\references{
Bertin, J. (1983),  \emph{Semiology of Graphics}, University of Wisconsin Press.
}
\examples{
data(Accident)
## maybe str(Accident) ; plot(Accident) ...

# mutual independence
acc.mod0 <- glm(Freq ~ age+result+mode+gender, data=Accident, family=poisson)
mosaic(acc.mod0)

# result as a response
acc.mod1 <- glm(Freq ~ result+ age*mode*gender, data=Accident, family=poisson)
mosaic(acc.mod1)
}
\keyword{datasets}
