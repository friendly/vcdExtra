\name{Mice}
\alias{Mice}
\docType{data}
\title{
Mice Depletion Data
}
\description{
Data from Kastenbaum and Lamphiear (1959).
The table gives the number of depletions (deaths) in 657 litters of mice, classified by
litter size and treatment.  This data set has become a classic in the analysis of
contingency tables, yet unfortunately little information on the details of the
experiment has been published.

}
\usage{data("Mice")}
\format{
  A freqency data frame with 30 observations on the following 4 variables, representing a 5 x 2 x 3 contingency table.
  \describe{
    \item{\code{litter}}{litter size, a numeric vector}
    \item{\code{treatment}}{treatment, a factor with levels \code{A} \code{B}}
    \item{\code{deaths}}{number of depletions, a factor with levels \code{0} \code{1} \code{2+}}
    \item{\code{Freq}}{cell frequency, a numeric vector}
  }
}
%\details{
%%%  ~~ If necessary, more details than the __description__ above ~~
%}
\source{
Goodman, L. A. (1983)
The analysis of dependence in cross-classifications having ordered categories, using log-linear models for frequencies and log-linear models for odds.
\emph{Biometrics}, 39, 149-160.

}
\references{
Kastenbaum, M. A. & Lamphiear, D. E. (1959)
Calculation of chi-square to calculate the no three-factor interaction hypothesis. \emph{Biometrics}, 15, 107-115.
}
\examples{ 
data(Mice)
# make a table
ftable(mice.tab <- xtabs(Freq ~ litter + treatment + deaths, data=Mice))

library(vcd)
mosaic(mice.tab, shade=TRUE)

}
\keyword{datasets}
