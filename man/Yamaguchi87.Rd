\name{Yamaguchi87}
\alias{Yamaguchi87}
\docType{data}
\title{
Occupational Mobility in Three Countries
}
\description{
Yamaguchi (1987) presented this three-way frequency table,
cross-classifying occupational categories of sons and fathers
in the United States, United Kingdom and Japan.

The US data were derived from the 1973 OCG-II survey; those
for the UK from the 1972 Oxford Social Mobility Survey; those
for Japan came from the 1975 Social Stratification and Mobility
survey. They pertain to men aged 20-64.
}
\usage{data(Yamaguchi87)}
\format{
  A frequency data frame with 75 observations on the following 4 variables.
  \describe{
    \item{\code{Son}}{a factor with levels \code{UpNM} \code{LoNM} \code{UpM} \code{LoM} \code{Farm}}
    \item{\code{Father}}{a factor with levels \code{UpNM} \code{LoNM} \code{UpM} \code{LoM} \code{Farm}}
    \item{\code{Country}}{a factor with levels \code{US} \code{UK} \code{Japan}}
    \item{\code{Freq}}{a numeric vector}
  }
}
%\details{
%%%  ~~ If necessary, more details than the __description__ above ~~
%}
\source{
Yamaguchi, K. (1987). 
Models for comparing mobility tables: toward parsimony and substance, 
\emph{American Sociological Review}, vol. 52 (Aug.), 482-494, Table 1
}
\references{
Goodman, L. A. and Hout, M. (1998).
Statistical Methods and Graphical Displays for Analyzing How the Association Between Two Qualitative Variables Differs Among Countries, Among Groups, Or Over Time: 
A Modified Regression-Type Approach.
\emph{Sociological Methodology}, 28 (1), 175-230.
}
\examples{
data(Yamaguchi87)
# reproduce Table 1
structable(~ Father + Son + Country, Yamaguchi87)
}
\keyword{datasets}
