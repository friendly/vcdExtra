\name{GSS}
\alias{GSS}
\docType{data}
\title{General Social Survey-- Sex and Party affiliation}
\description{
Data from the General Social Survey, 1991, on the relation between 
sex and party affiliation.
}
\usage{data(GSS)}
\format{
  A data frame in frequency form with 6 observations on the following 3 variables.
  \describe{
    \item{\code{sex}}{a factor with levels \code{female} \code{male}}
    \item{\code{party}}{a factor with levels \code{dem} \code{indep} \code{rep}}
    \item{\code{count}}{a numeric vector}
  }
}
%\details{
%  ~~ If necessary, more details than the __description__ above ~~
%}
\source{
Agresti, A. \emph{Categorical Data Analysis}, 2nd E., John Wiley & Sons, 2002,
Table 3.11, p. 106.
}
%\references{
%  ~~ possibly secondary sources and usages ~~
%}
\examples{
data(GSS)
str(GSS)

# use xtabs to show the table in a compact form
(GSStab <- xtabs(count ~ sex + party, data=GSS))

# fit the independence model
(mod.glm <- glm(count ~ sex + party, family = poisson, data = GSS))

# display all the residuals in a mosaic plot
mosaic(mod.glm, 
  formula = ~ sex + party, 
  labeling = labeling_residuals, 
  suppress=0)
}
\keyword{datasets}
