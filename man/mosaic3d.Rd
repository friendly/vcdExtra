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
with a given 3D shape
(a unit cube), and successively sub-divides it along the X, Y, Z dimensions
according to the table margins, generating a nested set of 3D tiles.
The volume of the resulting tiles 
is therefore proportional to the frequency represented in the
table cells.
Residuals from a given loglinear model are then used to color or shade each of the tiles.


This is an initial implementation.  The arguments and details are subject to change.
}

\usage{
mosaic3d(x, expected = NULL, residuals = NULL,  
	type = c("observed", "expected"), residuals_type = NULL, 
	shape = cube3d(alpha = alpha), alpha = 0.5, 
	spacing = 0.1, split_dir = 1:3, shading = shading_basic, 
	labeling_args = list(), newpage = TRUE, box=FALSE, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{
A multidimensional \code{array} or \code{table} or \code{\link[vcd]{structable}} 
of frequencies in a contingency table. 
In the present implementation, the dimensions are taken in sequential order.
Use \code{link[base]{aperm}} or \code{\link[vcd]{structable}} to change this.
}
  \item{expected}{
optionally, an array of expected frequencies of the same dimension as \code{x}, or alternatively 
the corresponding loglinear model specification as used by \code{link[stats]{loglin}}
or \code{link[MASS]{loglm}} (see \code{\link[vcd]{structable}} for details).}
  \item{residuals}{
optionally, an array of residuals of the same dimension as \code{x} (see details).
}
  \item{type}{
a character string indicating whether the \code{observed} or the \code{expected} 
frequencies in the table should be visualized.
}
  \item{residuals_type}{
a character string indicating the type of residuals to be computed when none are
supplied.  If  residuals  is  \code{NULL},  \code{residuals_type}  must  be  one  of 
\code{"pearson"} (default;  giving  components  of  Pearson's  chi-squared),  
\code{"deviance"}  (giving components of the likelihood ratio  chi-squared), 
or \code{"FT"} for the  Freeman-Tukey residuals.  
The value  of this  argument can  be abbreviated.  
}
  \item{shape}{
The initial 3D shape on which the mosaic is based.  Typically this is a
call to an rgl function, and must produce a \code{shape3d} object.
The default is a "unit cube" on (-1, +1), with transparency specified by
\code{alpha}.
}
  \item{alpha}{
Specifies the transparency of the 3D tiles used to compose the 3D mosaic.
}
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
  \item{labeling_args}{
This argument is intended to be used to specify details of the rendering
of labels for the table dimensions, but at present has no effect.
}
  \item{newpage}{
logical indicating whether a new page should be created for the plot or not.
}
  \item{box}{
logical indicating whether a bounding box should be drawn around the plot.
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

Residuals, used to color and shade the 3D tiles, can be passed explicitly,
or, more typically,
are computed as  needed from observed and expected  frequencies,
where  the  expected  frequencies  are  optionally  computed  for  a   specified
loglinear model.
}
\value{
Invisibly, an object of class \code{"structable"} corresponding to the plot.
}
\references{
Friendly, M. (1995).
Conceptual and Visual Models for Categorical Data, 
\emph{The American Statistician}, \bold{49}, 153-160.

Friendly, M. \emph{Visualizing Categorical Data}, Cary NC: SAS Insitute, 2000.
Web materials: \url{http://www.math.yorku.ca/SCS/vcd/}.

Theus, M. & Lauer, S. R. W. (1999)
Visualizing Loglinear Models. 
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
\code{\link[vcd]{strucplot}},
\code{\link[vcd]{mosaic}}, 
\code{\link[graphics]{mosaicplot}}

\code{\link[stats]{loglin}},
\code{link[MASS]{loglm}} for details on fitting loglinear models
}

\examples{
# 2 x 2 x 2
	mosaic3d(Bartlett, box=TRUE)
  # compare with model of mutual independence
	mosaic3d(Bartlett, type="expected", box=TRUE)
	
# 2 x 2 x 3
	mosaic3d(Heart, box=TRUE)
	
\dontrun{
# 2 x 2 x 2 x 3
# illustrates infelicities in labeling >3 dimensions
	mosaic3d(Detergent)
}

}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{hplot }
%\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
