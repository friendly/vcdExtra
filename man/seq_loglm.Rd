\name{seq_loglm}
\alias{seq_loglm}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Sequential Loglinear Models for an N-way Table
}
\description{
This function takes an n-way contingency table and fits a series of sequential
models to the 1-, 2-, ... n-way marginal tables, corresponding to a variety of
types of loglinear models.

}
\usage{
seq_loglm(x, 
   type = c("joint", "conditional", "mutual", "markov", "saturated"), 
   marginals = 1:nf, vorder = 1:nf, 
   k = NULL, prefix = "model", fitted = TRUE, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{
a contingency table in array form, with optional category labels specified in the 
dimnames(x) attribute,
or else a data.frame in frequency form, with the frequency variable named \code{"Freq"}.

}
  \item{type}{
type of sequential model to fit, a character string.
One of \code{"joint"}, \code{"conditional"}, \code{"mutual"}, \code{"markov"},
or  \code{"saturated"}.
}
  \item{marginals}{
which marginal sub-tables to fit? A vector of a (sub)set of the integers, \code{1:nf}
where \code{nf} is the number of factors in the full n-way table.
}
  \item{vorder}{
order of variables, a permutation of the integers \code{1:nf},
used to reorder the variables in the original table for the purpose
of fitting sequential marginal models. 
}
  \item{k}{
conditioning variable(s) for \code{type} = \code{"joint"}, \code{"conditional"} 
or Markov chain order for \code{type} = \code{"markov"}
}
  \item{prefix}{
prefix used to give names to the sequential models
}
  \item{fitted}{
argument passed to \code{loglm} to store the fitted values in the 
model objects
}
  \item{\dots}{
other arguments, passed down
}
}
\details{
Sequential marginal models for an n-way tables begin with the
model of equal-probability for the one-way margin
(equivalent to a \code{\link[stats]{chisq.test}}) and add
successive variables one at a time in the order specified by
\code{vorder}.

All model types give the same result for the two-way margin,
namely the test of independence for the first two factors.

Sequential models of \emph{joint independence} (\code{type="joint"})
have a particularly simple interpretation, because they
decompose the likelihood ratio test for the model of 
mutual independence in the full n-way table, and hence
account for "total" association in terms of portions attributable
to the conditional probabilities of each new variable,
given all prior variables.
}
\value{
An object of class \code{"loglmlist"}, each of which is a class \code{"loglm"} object
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
}
\references{
These functions were inspired by the original SAS implementation of
mosaic displays, described in the \emph{User's Guide},
\url{http://www.datavis.ca/mosaics/mosaics.pdf}
}
\author{
Michael Friendly
}
\note{
One-way marginal tables are a bit of a problem here, because they
cannot be fit directly using \code{\link[MASS]{loglm}}.
The present version uses \code{\link[stats]{loglin}},
and repairs the result to look like a \code{loglm} object (sort of).
 
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
\code{\link{loglin-utilities}} for descriptions of sequential models,
\code{\link{conditional}},
\code{\link{joint}},
\code{\link{mutual}}, \dots


\code{\link{loglmlist}}, 
}
\examples{
data(Titanic, package="datasets")
# variables are in the order Class, Sex, Age, Survived
tt <- seq_loglm(Titanic)


}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{models}
%\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
