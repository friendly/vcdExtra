\name{Fungicide}
\alias{Fungicide}
\docType{data}
\title{
Carcinogenic Effects of a Fungicide
}
\description{
Data from Gart (1971) on the carcinogenic effects of a certain fungicide
in two strains of mice.
Of interest is how the association between \code{group} (Control, Treated)
and \code{outcome} (Tumor, No Tumor) varies with \code{sex} and
\code{strain} of the mice.

Breslow (1976) used this data to illustrate the application of linear models
to log odds ratios.
}
\usage{data(Fungicide)}
\format{
	The data comprise a set of four 2 x 2 tables classifying 403 mice, either
	Control or Treated and whether or not a tumor was later observed.  
	The four groups represent the combinations of sex and strain of mice.
  The format is:
 num [1:2, 1:2, 1:2, 1:2] 5 4 74 12 3 2 84 14 10 4 ...
 - attr(*, "dimnames")=List of 4
  ..$ group  : chr [1:2] "Control" "Treated"
  ..$ outcome: chr [1:2] "Tumor" "NoTumor"
  ..$ sex    : chr [1:2] "M" "F"
  ..$ strain : chr [1:2] "1" "2"
}
\details{
All tables have some small cells, so a continuity correction is recommended.
}
\source{
Gart, J. J. (1971). The comparison of proportions: a review of significance tests,
   confidence intervals and adjustments for stratification. 
   \emph{International Statistical Review},
   39, 148-169.
}
\references{
Brewlow, N. (1976), Regression analysis of the log odds ratio:
   A method for retrospective studies, \emph{Biometrics}, 32(3), 409-416.
}
\examples{
data(Fungicide)
lor.fung <- loddsratio(Fungicide, correct=TRUE)
confint(lor.fung)
}
\keyword{datasets}
