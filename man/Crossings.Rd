\name{Crossings}
\alias{Crossings}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Crossings Interaction of Factors
}
\description{
Given two ordered factors in a square, n x n frequency table, \code{Crossings}
creates an n-1 column matrix corresponding to different degrees of difficulty
in crossing from one level to the next, as described by Goodman (1972).

}
\usage{
Crossings(...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{\dots}{
  Two factors
}
}
%\details{
%%  ~~ If necessary, more details than the description above ~~
%}
\value{
For two factors of \code{n} levels, returns a binary indicator matrix
of \code{n*n} rows and \code{n-1} columns.
}
\references{
Goodman, L. (1972).  Some multiplicative models for the analysis of cross-classified data.
In: \emph{Proceedings of the Sixth Berkeley Symposium on Mathematical Statistics and Probability},
Berkeley, CA: University of California Press, pp. 649-696.

}
\author{
Michael Friendly and Heather Turner
}
%\note{
%%  ~~further notes~~
%}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
\code{\link[stats]{glm}}, 
\code{\link[gnm]{gnm}} for model fitting functions for frequency tables

\code{\link[gnm]{Diag}}, 
\code{\link[gnm]{Mult}}, 
\code{\link[gnm]{Symm}}, 
\code{\link[gnm]{Topo}} 
for similar extensions to terms in model formulas.
}
\examples{
data(Hauser79)
# display table
structable(~Father+Son, data=Hauser79)

hauser.indep <- gnm(Freq ~ Father + Son, data=Hauser79, family=poisson)
hauser.CR <- update(hauser.indep, ~ . + Crossings(Father,Son))
summarize(hauser.CR)

hauser.CRdiag <- update(hauser.indep, ~ . + Crossings(Father,Son) + Diag(Father,Son))
summarize(hauser.CRdiag)

}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{models}
\keyword{manip}% __ONLY ONE__ keyword per line
