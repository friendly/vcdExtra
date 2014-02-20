\name{CyclingDeaths}
\alias{CyclingDeaths}
\docType{data}
\title{
London Cycling Deaths
}
\description{
A vector of the number of deaths of cyclists in London from 2005 through
2012 in each fortnightly period.  Aberdein & Spiegelhalter (2013)
discuss these data in relation to the observation that six cyclists
died in London between Nov. 5 and Nov. 13, 2013. 

}
\usage{data(CyclingDeaths)}
\format{
  The format is:
 Named int [1:208] 1 0 0 0 1 1 1 0 2 0 ...
 - attr(*, "names")= chr [1:208] "2005-01-01" "2005-01-15" "2005-01-29" "2005-02-12" ...
}
%\details{
%%%  ~~ If necessary, more details than the __description__ above ~~
%}
\source{
\url{http://data.gov.uk/dataset/road-accidents-safety-data},
STATS 19 data, 2005-2012.
}
\references{
Aberdein, Jody and Spiegelhalter, David (2013).
Have London's roads become more dangerous for cyclists?
\emph{Significance}, 10(6), 46--48.

}
\examples{
data(CyclingDeaths)


# make a one-way frequency table
CyclingDeaths.tab <- table(CyclingDeaths)

gf <- goodfit(CyclingDeaths.tab)
gf
summary(gf)

rootogram(gf, xlab="Number of Deaths")
distplot(CyclingDeaths.tab)

# prob of 6 or more deaths in one fortnight
lambda <- gf$par$lambda
ppois(5, lambda, lower.tail=FALSE)
}
\keyword{datasets}
