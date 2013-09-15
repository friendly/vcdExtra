\name{Titanicp}
\alias{Titanicp}
\docType{data}
\title{
Passengers on the Titanic
}
\description{
Data on passengers on the RMS Titanic, excluding the Crew and some
individual identifier variables. 
}
\usage{data(Titanicp)}
\format{
  A data frame with 1309 observations on the following 6 variables.
  \describe{
    \item{\code{pclass}}{a factor with levels \code{1st} \code{2nd} \code{3rd}}
    \item{\code{survived}}{a factor with levels \code{died} \code{survived}}
    \item{\code{sex}}{a factor with levels \code{female} \code{male}}
    \item{\code{age}}{passenger age in years (or fractions of a year, for children), a numeric vector}
    \item{\code{sibsp}}{number of siblings or spouses aboard, integer: \code{0:8}}
    \item{\code{parch}}{number of parents or children aboard, integer: \code{0:6}}
  }
}

\details{
There are a number of related versions of the Titanic data, in various formats.
This version was derived from \code{ptitanic} in the \pkg{rpart.plot} package,
modifying it to remove the \code{Class 'labelled'} attributes for some variables
(inherited from Frank Harrell's \code{titanic3} version)
which caused problems with some applications, notably \code{ggplot2}.

Other versions:

\code{\link[datasets]{Titanic}} is the 4-way frequency table of all 2201 people
aboard the Titanic, including passengers and crew.
 


}
\source{

The original R source for this
dataset was compiled by Frank Harrell and Robert Dawson:
\url{http://biostat.mc.vanderbilt.edu/twiki/pub/Main/DataSets/titanic.html},
described in more detail in
\url{http://biostat.mc.vanderbilt.edu/twiki/pub/Main/DataSets/titanic3info.txt}

For this version of the Titanic data, passenger details were deleted, 
survived was cast as a factor, and the name changed to \code{Titanicp} to minimize confusion 
with other versions.

}
%\references{
%%%  ~~ possibly secondary sources and usages ~~
%}
\examples{
data(Titanicp)
## maybe str(Titanicp) ; plot(Titanicp) ...
}
\keyword{datasets}
