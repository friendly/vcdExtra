\name{aperm}
\alias{aperm}
\alias{aperm.default}
\alias{aperm.table}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Array Transposition
}
\description{
Transpose an array or table by permuting its dimensions and optionally resizing it.

These functions extend \code{\link[base]{aperm}} to handle \code{\link[base]{table}} objects,
and will be withdrawn when the base functions are similarly extended.
}
\usage{

aperm(a, ...)

\method{aperm}{default}(a, perm, resize = TRUE, ...)

\method{aperm}{table}(a, perm, resize = TRUE, keep.class = TRUE, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{

  \item{a}{
	the array or table object to be transposed
}

  \item{perm}{
the subscript permutation vector, which must be a permutation of the integers 1:n, where n is the number of dimensions of a. 
The default is to reverse the order of the dimensions.
}
  \item{resize}{
a flag indicating whether the vector 
should be resized as well as having its elements reordered (default \code{TRUE}).
}

  \item{keep.class}{
a flag indicating whether the result should be assigned the same class as \code{a} (default \code{TRUE}).
}
  \item{\dots}{
  arguments passed to methods
}
}
%\details{
%}
\value{
A transposed version of array \code{a}, with subscripts permuted as indicated by the vector \code{perm}. 
If \code{resize} is \code{TRUE}, the array is reshaped as well as having its elements permuted, 
the dimnames are also permuted; 
if \code{resize = FALSE} then the returned object has the same dimensions as \code{a}, 
and the dimnames are dropped. In each case other attributes are copied from \code{a}. }
	
%\references{
%% ~put references to the literature/web site here ~
%}
\author{
Jonathan Rougier, \email{J.C.Rougier@durham.ac.uk} did the faster C implementation. 
Michael Friendly extended \code{aperm} to table objects.
}
\note{
The function \code{\link[base]{t}} provides a faster and more convenient way of transposing matrices. 
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
\code{\link[base]{t}}, to transpose matrices. 

\code{\link[base]{aperm}}, \code{\link[base]{table}}
}
\examples{
# interchange the first two subscripts on a 3-way array x
x  <- array(1:24, 2:4)
xt <- aperm(x, c(2,1,3))
stopifnot(t(xt[,,2]) == x[,,2],
          t(xt[,,3]) == x[,,3],
          t(xt[,,4]) == x[,,4])

# table objects
str(UCBAdmissions)
UCB <- aperm(UCBAdmissions, c(2, 1, 3))
str(UCB)
}

% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{array}
\keyword{manip}% __ONLY ONE__ keyword per line
