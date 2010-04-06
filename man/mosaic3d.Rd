\name{mosaic3d}
\Rdversion{1.1}
\alias{mosaic3d}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
3D Mosaic Plots
}
\description{
Produces a 3D mosaic plot for a contingency table using the \code{\link[rgl]{rgl-package}}.
 
Generalizing the 2D mosaic plot, this begins
with a unit cube, and successively sub-divides it along the X, Y, Z dimensions
according to the table margins, generating a nested set of 3D tiles.
Residuals from a given loglinear model are then used to color or shade each of the tiles.


This is an initial, experimental implementation.  The arguments and details are subject to change.

}
\usage{
mosaic3d(observed, expected = NULL, residuals = NULL, margin = NULL, 
         spacing = 0.1, split_dir = 1:3, 
         shading = shading_basic, alpha = 0.5, labeling_args = list(), ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{observed}{
A multidimensional \code{array} or \code{table} of frequencies in a contingency table. 
In the present implementation, the dimensions are taken in sequential order.
Use \code{link{aperm}} to change this.
}
  \item{expected, residuals, margin}{
The given loglinear model fit to the n-way table can be specified through one of these
arguments.  If \code{residuals} is specified, these are used directly to color or shade
the tiles. If \code{expected} is specified, residuals are calculated as Pearson
residuals. If only \code{margin}, it must be a list of 
vectors with the marginal totals to be fit in the loglinear model,
in a form suitable to be passed to \code{\link[stats]{loglin}}
If none of these arguments are specified, the function fits the
mutual independence model, with \code{margin=list(1:ndim)}.
}
%  \item{residuals}{
%%%     ~~Describe \code{residuals} here~~
%}
%  \item{margin}{
%%%     ~~Describe \code{margin} here~~
%}
  \item{spacing}{
A number or vector giving the total amount of space used to separate the
3D tiles along each of the dimensions of the table. The values specified
are re-cycled to the number of table dimensions.
}
  \item{split_dir}{
A numeric vector composed of the integers \code{1:3} or a character vector
composed of \code{c("x", "y", "z")}, where \code{split_dir[i]} specifies the
axis along which the tiles should be split for dimension \code{i} of the
table. The values specified are re-cycled to the number of table dimensions.
}
  \item{shading}{
A function, taking an array or vector of residuals for the given model,
returning a vector of colors.  At present, only the default
\code{shading=shading_basic} is provided.  This is roughly equivalent
to the use of the \code{shade} argument in \code{\link[graphics]{mosaicplot}}
or to the use of \code{gp=shading_Friendly} in \code{\link[vcd]{mosaic}}.
}
  \item{alpha}{
Specifies the transparency of the 3D tiles used to compose the 3D mosaic.
}
  \item{labeling_args}{
This argument is intended to be used to specify details of the rendering
of labels for the table dimensions, but at present has no effect.
}
  \item{\dots}{
Other arguments passed down to 3D functions.
}
}
\details{
Friendly (1995), Friendly [Sect. 4.5](2000) and Theus and Lauer (1999)
have all used the idea of 3D mosaic displays to explain various aspects
of loglinear models (the iterative proportional fitting algorithm,
the structure of various models for 3-way and n-way tables, etc.),
but no implementation of 3D mosaics was previously available.	
}
\value{
No returned value so far; used for the side-effect of drawing on an
rgl window.
}
\references{
Friendly, M. (1995).
Conceptual and Visual Models for Categorical Data, 
\emph{The American Statistician}, \bold{49}, 153-160.

Friendly, M. \emph{Visualizing Categorical Data}, Cary NC: SAS Insitute, 2000.
Web materials: \url{http://www.math.yorku.ca/SCS/vcd/}.

Theus, M. & Lauer, S. R. W. (1999)
Visualizing Loglinear Models, 
\emph{Journal of Computational and Graphical Statistics}, \bold{8}, 396-412.


}
\author{
Michael Friendly, with the help of Duncan Murdoch and Achim Zeileis
}
%\note{
%%%  ~~further notes~~
%}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
\code{\link[vcd]{mosaic}}, 
\code{\link[graphics]{mosaicplot}},
\code{\link[stats]{loglin}}
}
\examples{

# 2 x 2 x 2
	mosaic3d(Bartlett)
	box3d()
	
# 2 x 2 x 3
	mosaic3d(Heart)
	box3d()
	
# 2 x 2 x 2 x 3
	mosaic3d(Detergent)


}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{hplot }
%\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
