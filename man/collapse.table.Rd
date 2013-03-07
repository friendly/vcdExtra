\name{collapse.table}
\alias{collapse.table}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Collapse Levels of a Table}
\description{
Collapse (or re-label) variables in a
a contingency table or ftable by re-assigning levels of the table variables
}
\usage{
collapse.table(table, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{table}{A \code{table} or \code{ftable} object}
  \item{\dots}{ A collection of one or more assignments of factors of the table to
  a list of levels }
}
\details{
Each of the \code{\dots} arguments must be of the form 
\code{variable = levels}, where \code{variable} is the name of one of the table
dimensions, and \code{levels} is a character or numeric vector of length equal
to the corresponding dimension of the table.
}
\value{
A \code{xtabs} and \code{table} objects, representing the original table with
one or more of its factors collapsed or rearranged into other levels.
}
%\references{ ~put references to the literature/web site here ~ }
\author{Michael Friendly}
%\note{ ~~further notes~~ 
%
% ~Make other sections like Warning with \section{Warning }{....} ~
%}
\seealso{ 
\code{\link{expand.dft}}

\code{\link[base]{margin.table}} "collapses" a table in a different way, by
summing over table dimensions.
}

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

# change labels for levels of education to 1:3
t4 <- collapse.table(t1,  education=1:3)

}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{manip}
\keyword{attribute}% __ONLY ONE__ keyword per line
