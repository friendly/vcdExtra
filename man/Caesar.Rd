\name{Caesar}
\alias{Caesar}
\docType{data}
\title{Risk Factors for Infection in Caesarian Births}
\description{Data from infection from birth by Caesarian section, classified by \code{Risk} (two levels), 
whether \code{Antibiotics} were used (two levels) and whether the Caesarian section was \code{Planned} or not.  
The outcome is \code{Infection} (three levels).}

\usage{
data(Caesar)
}
\format{
  A     4-dimensional array resulting from cross-tabulating    4 variables for   251 observations. The variable names and their levels are:

  \tabular{rll}{
    No \tab Name \tab Levels \cr
    1\tab \code{Infection}\tab \code{"Type 1", "Type 2", "None"}\cr
    2\tab \code{Risk}\tab \code{"Yes", "No"}\cr
    3\tab \code{Antibiotics}\tab \code{"Yes", "No"}\cr
    4\tab \code{Planned}\tab \code{"Yes", "No"}\cr
  }
}

\details{
\code{Infection} is regarded as the response variable here. 
There are quite a few 0 cells here, particularly when  \code{Risk}
is absent and the Caesarian section was unplanned.
Should these be treated as structural or sampling zeros?
}
\source{

%  \cite{Fahrmeir:94}
Fahrmeir, L. & Tutz, G. (1994).
Multivariate Statistical Modelling Based on Generalized Linear Models 
New York: Springer Verlag.

}

%\references{
%}

\seealso{\code{\link[Fahrmeir]{caesar}} for the same data recorded as a frequency data frame
with other variables.}
\examples{
data(Caesar)
#display table;  note that there are quite a few 0 cells
structable(Caesar)

# baseline model, Infection as response
Caesar.mod0 <- loglm(~Infection + (Risk*Antibiotics*Planned), data=Caesar)
# NB: Pearson chisq cannot be computed due to the 0 cells
Caesar.mod0

mosaic(Caesar.mod0, main="Baseline model")

# Illustrate handling structural zeros
zeros <- 0+ (Caesar >0)
zeros[1,,1,1] <- 1
structable(zeros)

# fit model excluding possible structural zeros
Caesar.mod0s <- loglm(~Infection + (Risk*Antibiotics*Planned), data=Caesar, 
	start=zeros)
Caesar.mod0s

anova(Caesar.mod0, Caesar.mod0s, test="Chisq")

mosaic (Caesar.mod0s)

# what terms to add?
add1(Caesar.mod0, ~.^2, test="Chisq")

# add Association of Infection:Antibiotics
Caesar.mod1 <- update(Caesar.mod0, ~.+Infection:Antibiotics)
anova(Caesar.mod0, Caesar.mod1, test="Chisq")

mosaic(Caesar.mod1, gp=shading_Friendly, main="Adding Infection:Antibiotics")



}

\keyword{datasets}
