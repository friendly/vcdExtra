\name{Hoyt}
\alias{Hoyt}
\docType{data}
\title{Minnesota High School Graduates}
\description{Minnesota high school graduates of June 1930 were classified with respect to 
(a) position by thirds in their graduating class, (b) post-high school status in April 1939 (4 levels), (c) sex, 
(d) father's occupational status (7 levels, from 1=High to 7=Low).
}

\usage{
data(Hoyt)
}
\format{
  A     4-dimensional array resulting from cross-tabulating    4 variables for 13968 observations. The variable names and their levels are:

  \tabular{rll}{
    No \tab Name \tab Levels \cr
    1\tab \code{Status}\tab \code{"College", "School", "Job", "Other"}\cr
    2\tab \code{Rank}\tab \code{"Low", "Middle", "High"}\cr
    3\tab \code{Occupation}\tab \code{"1", "2", "3", "4", "5", "6", "7"}\cr
    4\tab \code{Sex}\tab \code{"Male", "Female"}\cr
  }
}

\details{Post high-school \code{Status} is natural to consider as the response.
\code{Rank} and father's \code{Occupation} are ordinal variables.}
\source{

%  \cite{Hoyt-etal:59}
%  \cite{Fienberg:80 [pp.91-92]}
%  \cite{Plackett:74}
%  \cite{minn38{MASS}}

Fienberg, S. E. (1980).
\emph{The Analysis of Cross-Classified Categorical Data}.
Cambridge, MA: MIT Press, p. 91-92.

R. L. Plackett, (1974).
\emph{The Analysis of Categorical Data}. London: Griffin.


}

\references{
Hoyt, C. J., Krishnaiah, P. R. and Torrance, E. P. (1959) 
Analysis of complex contingency tables, 
\emph{Journal of Experimental Education} 27, 187-194. 

}

\seealso{
\code{\link[MASS]{minn38}} provides the same data as a data frame.
}
\examples{
data(Hoyt)

# display the table
structable(Status+Sex ~ Rank+Occupation, data=Hoyt)

# mosaic for independence model
plot(Hoyt, shade=TRUE)

# examine all pairwise mosaics
pairs(Hoyt, shade=TRUE)

# collapse Status to College vs. Non-College
Hoyt1 <- collapse.table(Hoyt, Status=c("College", rep("Non-College",3)))
plot(Hoyt1, shade=TRUE)

}

\keyword{datasets}
