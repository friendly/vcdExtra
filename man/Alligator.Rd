\name{Alligator}
\alias{Alligator}
\docType{data}
\title{
Alligator Food Choice
}
\description{
The Alligator data, from Agresti (2002), comes from a study of the primary food choices of alligators 
in four Florida lakes. Researchers classified the stomach contents of 219 captured alligators into five categories: 
Fish (the most common primary food choice), Invertebrate (snails, insects, crayfish, etc.), 
Reptile (turtles, alligators), Bird, and Other (amphibians, plants, household pets, stones, and other debris).
}
\usage{data(Alligator)}
\format{
  A frequency data frame with 80 observations on the following 5 variables.
  \describe{
    \item{\code{lake}}{a factor with levels \code{George} \code{Hancock} \code{Oklawaha} \code{Trafford}}
    \item{\code{sex}}{a factor with levels \code{female} \code{male}}
    \item{\code{size}}{alligator size, a factor with levels \code{large} (>2.3m) \code{small} (<=2.3m)}
    \item{\code{food}}{primary food choice, a factor with levels \code{bird} \code{fish} \code{invert} \code{other} \code{reptile}}
    \item{\code{count}}{cell frequency, a numeric vector}
  }
}
\details{
The table contains a fair number of 0 counts.

\code{food} is the response variable.  \code{fish} is the most frequent choice, and often
taken as a baseline category in multinomial response models.
}
\source{
Agresti, A. (2002). \emph{Categorical Data Analysis}, New York: Wiley, 2nd Ed., Table 7.1
}
%\references{
%%%  ~~ possibly secondary sources and usages ~~
%}
\examples{
data(Alligator)

# change from frequency data.frame to table
allitable <- xtabs(count~lake+sex+size+food, data=Alligator)
# Agresti's Table 7.1
structable(food~lake+sex+size, allitable)


plot(allitable, shade=TRUE)
# mutual independence model
mosaic(~food+lake+size, allitable, shade=TRUE)
# food jointly independent of lake and size
mosaic(~food+lake+size, allitable, shade=TRUE, expected=~lake:size+food)

if (require(nnet)) {
	# multinomial logit model
	mod1 <- multinom(food ~ lake+size+sex, data=Alligator, weights=count)
}

}
\keyword{datasets}
