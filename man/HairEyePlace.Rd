\name{HairEyePlace}
\alias{HairEyePlace}
\docType{data}
\title{
Cross-Classifications of Eye Color by Hair Color in Caithness and Aberdeen
}
\description{
A three-way frequency table crossing eye color and hair color in two places, Caithness and Aberdeen,
Scotland.
These data were of interest to Fisher (1940) and others because
there are mixtures of people of Nordic, Celtic and Anglo-Saxon origin.
One or both tables have been widely analyzed in conjunction with
RC and canonical correlation models for categorical data, e.g.,
Becker and Clogg (1989).
}
\usage{data(HairEyePlace)}
\format{
  The format is:
 num [1:4, 1:5, 1:2] 326 688 343 98 38 116 84 48 241 584 ...
 - attr(*, "dimnames")=List of 3
  ..$ Eye  : chr [1:4] "Blue" "Light" "Medium" "Dark"
  ..$ Hair : chr [1:5] "Fair" "Red" "Medium" "Dark" ...
  ..$ Place: chr [1:2] "Caithness" "Aberdeen"
}
%\details{
%%%  ~~ If necessary, more details than the __description__ above ~~
%}
\source{
This data was taken from the \code{colors} data in \pkg{logmult}.
}
\references{
Becker, M. P., and Clogg, C. C. (1989). 
Analysis of Sets of Two-Way Contingency Tables Using Association Models. 
\emph{Journal of the American Statistical Association}, 84(405), 142-151. 

Fisher, R.A. (1940) The precision of discriminant functions. 
\emph{Annals of Eugenics}, 10, 422-429.
}
\examples{
data(HairEyePlace)
## maybe str(HairEyePlace) ; plot(HairEyePlace) ...
}
\keyword{datasets}
