\name{loglin-utilities}
\alias{conditional}
\alias{joint}
\alias{loglin2formula}
\alias{loglin2string}
\alias{markov}
\alias{mutual}
\alias{saturated}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Loglinear Model Utilities
}
\description{
These functions generate lists of terms to specify a loglinear model
in a form compatible with \code{\link[stats]{loglin}} and also provide for conversion to an
equivalent \code{\link[MASS]{loglm}} specification or a shorthand character
string representation.
  
They allow for a more conceptual
way to specify such models by a function for their type, as opposed
to just an uninterpreted list of model terms
and also allow
easy specification of marginal models for a given contingency table.

They are intended to be used as tools in higher-level modeling and 
graphics functions, but can also be used directly.
}
\usage{

conditional(nf, table = NULL, factors = 1:nf, with = nf)

joint(nf, table = NULL, factors = 1:nf, with = nf)

markov(nf, factors = 1:nf, order = 1)

mutual(nf, table = NULL, factors = 1:nf)

saturated(nf, table = NULL, factors = 1:nf)

loglin2formula(x, env = parent.frame())

loglin2string(x, brackets = c("[", "]"), sep = ",", collapse = " ", abbrev)

}
%- maybe also 'usage' for other objects documented here.
\arguments{

  \item{nf}{
number of factors for which to generate the model
}
  \item{table}{
a contingency table used only for factor names in the model, typically the output from \code{\link[base]{table}}
and possibly permuted with \code{aperm}
}
  \item{factors}{
names of factors used in the model formula when \code{table} is not specified
}
  \item{with}{
For \code{joint} and \code{conditional} models, \code{with} gives the
indices of the factors against which all others are considered jointly 
or conditionally independent
}

  \item{order}{
For \code{markov}, this gives the order of the Markov chain model for the
factors.  An \code{order=1} Markov chain allows associations among
sequential pairs of factors, e.g., \code{[A,B], [B,C], [C,D]} \dots.
An \code{order=2} Markov chain allows associations among
sequential triples.
}

  \item{x}{
a list of terms in a loglinear model, such as returned by \code{conditional}, \code{joint}, 
 \dots
}

  \item{env}{
For \code{loglin2formula}, environment in which to evaluate the formula
  }
  \item{brackets}{
For \code{loglin2string}, 
characters to use to surround model terms.  
Either a single character string containing two characters (e.g., \code{'[]'}
or a character vector of length two.
}
  \item{sep}{
For \code{loglin2string},
the separator character string used for factor names within a given model term
}
  \item{collapse}{
For \code{loglin2string},
the character string used between terms in the the model string
}
  \item{abbrev}{
For \code{loglin2string},
whether and how to abbreviate the terms in the string representation.
This has not yet been implemented.
}

}
\details{
  The main model specification functions, \code{conditional}, \code{joint}, 
  \code{markov}, \dots, \code{saturated},
  return a list of vectors indicating the marginal totals to be fit,
  via the \code{margin} argument to \code{\link[stats]{loglin}}.
  Each element of this list corresponds to a high-order
  term in a hierarchical loglinear model, where, e.g., a term
  like \code{c("A", "B")} is equivalent to the \code{\link[MASS]{loglm}}
  term \code{"A:B"} and hence automatically includes all low-order terms.
  
  Note that these can be used to supply the \code{expected} argument for
  the default \code{\link[vcd]{mosaic}} function, when the data is supplied
  as a contingency table.
  
  \code{loglin2formula} converts the output of one of these to a model formula
  suitable as the \code{formula} for of \code{\link[MASS]{loglm}}.
  
  \code{loglin2string} converts the output of one of these to a string
  describing the loglinear model in the shorthand bracket notation,
  e.g., \code{"[A,B] [A,C]"}.  
  
  
}
\value{
  For the main model specification functions, \code{conditional}, \code{joint}, 
  \code{markov}, \dots, the result is
   a list of vectors (terms), where the elements in each vector are the
   names of the factors. The elements of the list are given names
   \code{term1, term2, \dots}.

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
\code{\link[stats]{loglin}},
\code{\link[MASS]{loglm}}
}
\examples{
joint(3, table=HairEyeColor)
# as a formula or string
loglin2formula(joint(3, table=HairEyeColor))
loglin2string(joint(3, table=HairEyeColor))

joint(2, HairEyeColor)  # marginal model for [Hair] [Eye]

# other possibilities
joint(4, factors=letters, with=1)
joint(5, factors=LETTERS)
joint(5, factors=LETTERS, with=4:5)

conditional(4)
conditional(4, with=3:4)

# use in mosaic displays or other strucplots
mosaic(HairEyeColor, expected=joint(3))
mosaic(HairEyeColor, expected=conditional(3))

# use with loglm
cond3 <- loglin2formula(conditional(3, table=HairEyeColor))
cond3 <- loglin2formula(conditional(3))  # same, with factors 1,2,3
loglm(cond3, data=HairEyeColor)

saturated(3, HairEyeColor)
loglin2formula(saturated(3, HairEyeColor))
loglin2string(saturated(3, HairEyeColor))
loglin2string(saturated(3, HairEyeColor), brackets='{}', sep=', ')

}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{models}
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
