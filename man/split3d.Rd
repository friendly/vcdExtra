\name{split3d}
\Rdversion{1.1}
\alias{split3d}
\alias{split3d.shape3d}
\alias{split3d.list}
\alias{range3d}
\alias{center3d}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Subdivide a 3D Object
}
\description{
Subdivides a \code{shape3d} object or a list of \code{shape3d} objects
into objects of the same shape along a given dimension
according to the proportions or frequencies specified in vector(s).

\code{split3d} is the basic workhorse used in \code{\link{mosaic3d}},
	but may be useful in other contexts.

\code{range3d} and \code{center3d} are utility functions, also useful in other contexts.
}
\usage{

split3d(obj, ...)

\method{split3d}{shape3d}(obj, p, dim, space = 0.1, ...)

\method{split3d}{list}(obj, p, dim, space = 0.1, ...)

range3d(obj)

center3d(obj)
}
%- maybe also 'usage' for other objects documented here.
\arguments{

  \item{obj}{
A \code{shape3d} object, or a list composed of them
}
  \item{\dots}{
Other arguments for split3d methods
}

  \item{p}{
For a single \code{shade3d} object, a vector of proportions
(or a vector of non-negative numbers which will be normed to proportions)
indicating the number of subdivisions and their scaling along dimension \code{dim}.
For a list of \code{shade3d} objects, a matrix whose columns indicate the
subdivisions of each object.  
}
  \item{dim}{
The dimension along which the object is to be subdivided.
Either an integer: 1, 2, or 3, or a character: "x", "y", or "z".
}
  \item{space}{
The total space used to separate the copies of the object along
dimension \code{dim}.
The unit inter-object space is therefore \code{space/(length(p)-1)}.
}


}
\details{
The resulting list of \code{shape3d} objects is actually composed of \emph{copies}
of the input object(s), scaled according to the proportions in \code{p}
and then translated to make their range along the splitting dimension
equal to that of the input object(s).  
}
\value{
\code{split3d} returns a list of \code{shape3d} objects.

\code{range3d} returns a 2 x 3 matrix, whose first row contains
the minima on dimensions x, y, z, and whose second row contains
the maxima.

\code{center3d} returns a numeric vector containing
the means of the minima and maxima on dimensions x, y, z.
}
\author{
Duncan Murdoch, with refinements by Michael Friendly
}
%\note{
%%%  ~~further notes~~
%}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
\code{\link{mosaic3d}}

\code{\link[rgl]{shapelist3d}} for the plotting of lists of \code{shape3d} objects.

}
\examples{
if (require(rgl)) {
  open3d()
  cube <- cube3d(alpha=0.4)
  sl1 <- split3d(cube, c(.2, .3, .5), 1)
  col <- c("#FF000080", "#E5E5E580", "#0000FF80")
  shapelist3d(sl1, col=col)
	
  open3d()
  p <- matrix(c(.6, .4, .5, .5, .2, .8), nrow=2)
  sl2 <- split3d(sl1, p, 2)
  shapelist3d(sl2, col=col)	
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{dplot}
%\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
