\name{HospVisits}
\alias{HospVisits}
\docType{data}
\title{
Hospital Visits Data
}
\description{
Length of stay in hospital for 132 schizophrenic patients, classified by
visiting patterns, originally from Wing (1962).

}
\usage{data("HospVisits")}
\format{
  A 3 by 3 frequency table, with format:
 table [1:3, 1:3] 43 6 9 16 11 18 3 10 16
 - attr(*, "dimnames")=List of 2
  ..$ visit: chr [1:3] "Regular" "Infrequent" "Never"
  ..$ stay : chr [1:3] "2-9" "10-19" "20+"
}

\details{
Both table variables can be considered ordinal.
The variable \code{visit} refers to visiting patterns recorded hospital. 
The category labels are abbreviations of those given by Goodman (1983);
e.g., \code{"Regular"} is short for \dQuote{received visitors regularly or patient went home}.
The variable \code{stay} refers to length of stay in hospital, in year groups.

}
\source{
Goodman, L. A. (1983) The analysis of dependence in cross-classifications having ordered categories, using log-linear models for frequencies and log-linear models for odds. 
\emph{Biometrics}, 39, 149-160.
}
\references{
Wing, J. K. (1962). Institutionalism in Mental Hospitals,
\emph{British Journal of Social and Clinical Psychology}, 1 (1), 38-51.
}
\seealso{\code{\link[ca]{ca}}}

\examples{
data(HospVisits)
mosaic(HospVisits, gp=shading_Friendly)

if(require(ca)){
  ca(HospVisits)
  # surprisingly 1D !
  plot(ca(HospVisits))
  }
}
\keyword{datasets}
\concept{correspondence analysis}
\concept{ordinal variables}
