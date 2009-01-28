\name{collapse.table}
\alias{collapse.table}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Collapse levels of a table}
\description{
Collapse a contingency table or ftable by re-assigning levels of table variables
}
\usage{
collapse.table(table, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{table}{A table or ftable object}
  \item{\dots}{ A collection of one or more assignments of factors of the table to
  a list of levels }
}
\details{
  ~~ If necessary, more details than the description above ~~
}
\value{
  ~Describe the value returned
  If it is a LIST, use
  \item{comp1 }{Description of 'comp1'}
  \item{comp2 }{Description of 'comp2'}
  ...
}
%\references{ ~put references to the literature/web site here ~ }
\author{Michael Friendly}
%\note{ ~~further notes~~ 
%
% ~Make other sections like Warning with \section{Warning }{....} ~
%}
\seealso{ \code{\link{expand.dft}}, ~~~ }
\examples{
# create some sample data in table form
sex <- c("Male", "Female")
age <- letters[1:6]
education <- c("low", 'med', 'high')
data <- expand.grid(sex=sex, age=age, education=education)
counts <- rpois(36, 100) 
data <- cbind(data, counts)
t1 <- xtabs(counts ~ sex + age + education, data=data)

# collapse age to 3 levels
t2 <- collapse.table(t1, age=c("A", "A", "B", "B", "C", "C"))

# collapse age to 3 levels and pool education: "low" and "med" to "low"
t3 <- collapse.table(t1, age=c("A", "A", "B", "B", "C", "C"), 
    education=c("low", "low", "high"))
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{manip}
\keyword{attribute}% __ONLY ONE__ keyword per line
