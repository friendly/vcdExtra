\name{AirCrash}
\alias{AirCrash}
\docType{data}
\title{
Air Crash Data
}
\description{
Data on all fatal commercial airplane crashes from 1993--2015.
Excludes small planes (less than 6 passengers) and non-commercial (cargo, military, private) aircraft.
}
\usage{data("AirCrash")}
\format{
  A data frame with 439 observations on the following 5 variables.
  \describe{
    \item{\code{Phase}}{phase of the flight, a factor with levels \code{en route} \code{landing} \code{standing} \code{take-off} \code{unknown}}
    \item{\code{Cause}}{a factor with levels \code{criminal} \code{human error} \code{mechanical} \code{unknown} \code{weather}}
    \item{\code{date}}{date of crash, a Date}
    \item{\code{Fatalities}}{number of fatalities, a numeric vector}
    \item{\code{Year}}{year, a numeric vector}
  }
}
\details{
\code{Phase} of the flight was cleaned by combining related variants, spelling, etc.
}
\source{
Originally from David McCandless,
\url{http://www.informationisbeautiful.net/visualizations/plane-truth-every-single-commercial-plane-crash-visualized/},
with the data at
\url{https://docs.google.com/spreadsheet/ccc?key=0AjOUPqcIwvnjdEx2akx5ZjJXSk9oM1E3dWpqZFJ6Nmc&usp=drive_web#gid=1},
downloaded April 14, 2015.
}
\references{
Rick Wicklin,
\url{http://blogs.sas.com/content/iml/2015/03/30/visualizing-airline-crashes.html}
}
\examples{
data(AirCrash)
aircrash.tab <- xtabs(~Phase + Cause, data=AirCrash)
mosaic(aircrash.tab, shade=TRUE)

# fix label overlap
mosaic(aircrash.tab, shade=TRUE, 
       labeling_args=list(rot_labels=c(30, 30, 30, 30)))

# reorder by Phase
phase.ord <- rev(c(3,4,1,2,5))
mosaic(aircrash.tab[phase.ord,], shade=TRUE, 
       labeling_args=list(rot_labels=c(30, 30, 30, 30)), offset_varnames=0.5)

# reorder by frequency
phase.ord <- order(rowSums(aircrash.tab), decreasing=TRUE)
cause.ord <- order(colSums(aircrash.tab), decreasing=TRUE)
mosaic(aircrash.tab[phase.ord,cause.ord], shade=TRUE, 
       labeling_args=list(rot_labels=c(30, 30, 30, 30)))


library(ca)
aircrash.ca <- ca(aircrash.tab)
plot(aircrash.ca)
}
\keyword{datasets}
