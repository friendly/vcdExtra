\name{seq_mosaic}
\alias{seq_mosaic}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Sequential Mosaics and Strucplots for an N-way Table
}
\description{
This function takes an n-way contingency table and plots mosaics for series of sequential
models to the 1-, 2-, ... n-way marginal tables, corresponding to a variety of
types of loglinear models.
}
\usage{
seq_mosaic(x, panel = mosaic, 
   type = c("joint", "conditional", "mutual", "markov", "saturated"), 
   plots = 1:nf, vorder = 1:nf, 
   k = NULL, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{
a contingency table in array form, with optional category labels specified in the 
dimnames(x) attribute,
or else a data.frame in frequency form, with the frequency variable named \code{"Freq"}.
}

  \item{panel}{
a \code{\link[vcd]{strucplot}} panel function, 
typicially \code{\link[vcd]{mosaic}} or \code{\link[vcd]{sieve}. NOT yet implemented.}
}

  \item{type}{
type of sequential model to fit, a character string.
One of \code{"joint"}, \code{"conditional"}, \code{"mutual"}, \code{"markov"},
or  \code{"saturated"}.
}
  \item{plots}{
which marginal sub-tables to plot? A vector of a (sub)set of the integers, \code{1:nf}
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
  \item{\dots}{
other arguments passed to \code{\link[vcd]{mosaic}}. 
}

}

\details{
This function produces 
similar plots to the use of \code{\link{mosaic.loglmlist}},
called with the result of \code{\link{seq_loglm}}.
}
\value{
None. Used for its side-effect of producing plots
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
%\note{
%%%  ~~further notes~~
%}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
\code{\link{loglin-utilities}} for descriptions of sequential models,
\code{\link{conditional}},
\code{\link{joint}},
\code{\link{mutual}}, \dots

\code{\link{loglmlist}},  \code{\link{mosaic.loglmlist}},
\code{\link{seq_loglm}}

\code{\link{mosaic.glm}}, \code{\link[vcd]{mosaic}},
\code{\link[vcd]{strucplot}}, for the many parameters that control the details of mosaic plots.
}
\examples{

data(Titanic, package="datasets")

seq_mosaic(Titanic)  # models of joint independence, Survived last
seq_mosaic(Titanic, type="condit")
seq_mosaic(Titanic, type="mutual")

# other panel functions and options: presently BUGGED
\dontrun{
seq_mosaic(Titanic, type="mutual", panel=sieve, 
   gp=shading_Friendly, labeling=labeling_values)
}

}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{hplots}
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
