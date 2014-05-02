\name{Toxaemia}
\alias{Toxaemia}
\docType{data}
\title{
Toxaemia Symptoms in Pregnancy
}
\description{
Brown et al (1983) gave these data on two signs of toxaemia,
an abnormal condition during pregnancy characterized by
high blood pressure (hypertension) and high levels of protein
in the urine.  If untreated, both the mother and baby are
at risk of complications or death.

The data frame \code{Toxaemia} represents 13384 expectant
mothers in Bradford, England in their first pregnancy, who
were also classified according to social class and the number
of cigarettes smoked per day.
}
\usage{data(Toxaemia)}
\format{
  A data frame in frequency form representing a 5 x 3 x 2 x 2 contingency table,
  with 60 observations on the following 5 variables.
  \describe{
    \item{\code{class}}{Social class of mother, a factor with levels \code{1} \code{2} \code{3} \code{4} \code{5}}
    \item{\code{smoke}}{Cigarettes smoked per day during pregnancy, a factor with levels \code{0} \code{1-19} \code{20+}}
    \item{\code{hyper}}{Hypertension level, a factor with levels \code{Low} \code{High}}
    \item{\code{urea}}{Protein urea level, a factor with levels \code{Low} \code{High}}
    \item{\code{Freq}}{frequency in each cell, a numeric vector}
  }
}
%\details{
%%%  ~~ If necessary, more details than the __description__ above ~~
%}
\source{
Brown, P. J., Stone, J. and Ord-Smith, C. (1983), 
Toxaemic signs during pregnancy.
\emph{JRSS, Series C, Applied Statistics}, 32, 69-72
}
\references{
Friendly, M.  (2000), \emph{Visualizing Categorical Data}, SAS Institute, Cary, NC,
Example 7.15.
}
\examples{
data(Toxaemia)

tox.tab <- xtabs(Freq~class+smoke+hyper+urea,Toxaemia)
ftable(tox.tab, row.vars=1)


# symptoms by smoking
mosaic(~smoke+hyper+urea, data=tox.tab, shade=TRUE)

# symptoms by social class
mosaic(~class+hyper+urea, data=tox.tab, shade=TRUE)

# predictors
mosaic(~smoke+class, data=tox.tab, shade=TRUE)

# responses
mosaic(~hyper+urea, data=tox.tab, shade=TRUE)

# log odds ratios for urea and hypertension, by class and smoke
LOR <-loddsratio(aperm(tox.tab))
LOR

}
\keyword{datasets}
