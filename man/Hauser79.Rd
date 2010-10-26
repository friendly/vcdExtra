\name{Hauser79}
\alias{Hauser79}
\docType{data}
\title{
Hauser (1979) Data on Social Mobility
}
\description{
Hauser (1979) presented this two-way frequency table,
cross-classifying occupational categories of sons and fathers
in the United States.
}
\usage{data(Hauser79)}
\format{
  A frequency data frame with 25 observations on the following 3 variables, representing the
  cross-classification of 19912 individuals by father's occupation and son's first occupation. 
  \describe{
    \item{\code{Son}}{a factor with levels \code{UpNM} \code{LoNM} \code{UpM} \code{LoM} \code{Farm}}
    \item{\code{Father}}{a factor with levels \code{UpNM} \code{LoNM} \code{UpM} \code{LoM} \code{Farm}}
    \item{\code{Freq}}{a numeric vector}
  }
}
%\details{
%%%  ~~ If necessary, more details than the __description__ above ~~
%}
\source{
R.M. Hauser (1979), Some exploratory methods for modeling mobility tables and other cross-classified data. 
In: K.F. Schuessler (Ed.), \emph{Sociological Methodology}, 1980, Jossey-Bass, San Francisco, pp. 413-458.
}
%\references{
%%%  ~~ possibly secondary sources and usages ~~
%}
\examples{
data(Hauser79)
str(Hauser79)
mosaic(Freq ~ Father + Son, data=Hauser79, shade=TRUE)
}
\keyword{datasets}
