\name{mcaplot}
\alias{mcaplot}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Simple and enhanced plot of MCA solutions
}
\description{
This function is intended as an alternative to \code{\link[ca]{plot.mjca}}
for plotting multiple correspondence analysis solutions.
It provides more flexibility for labeling factor levels and connecting
them with lines. It does not support some features of
\code{plot.mjca} (centroids, supplementary points, arrows, etc.) 
}
\usage{
mcaplot(obj, map = "symmetric", dim = 1:2, 
        col = c("blue", "red", "brown", "black", "green3", "purple"), 
        pch = 15:20, cex = 1.2, pos = 3, lines = TRUE, lwd = 2, 
        legend = FALSE, legend.pos = "topright", 
        xlab = "_auto_", ylab = "_auto_", 
        rev.axes  = c(FALSE, FALSE),
        ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{obj}{An \code{"mjca"} object}

  \item{map}{
Character string specifying the map type, i.e., the scaling applied to coordinates
for different types of MCA representations.
Allowed options include:
        \code{"symmetric"} (default),
        \code{"rowprincipal"},   \code{"colprincipal"}, \code{"symbiplot"},
        \code{"rowgab"}, \code{"colgab"}, \code{"rowgreen"}, \code{"colgreen"}.
        See \code{\link[ca]{mjca}} for details.
}

  \item{dim}{Dimensions to plot, an integer vector of length 2}

  \item{col}{Vector of colors, one for each factor in the MCA}

  \item{pch}{Vector of point symbols for the category levels, one for each factor}

  \item{cex}{Character size for points and level labels}

  \item{pos}{Position of level labels relative to the category points; either a single number
             or a vector of length equal to the number of category points.}

  \item{lines}{A logical or an integer vector indicating which factors are to be
             joined with lines using \code{\link{multilines}}}

  \item{lwd}{Line width(s) for the lines}
  
  \item{legend}{Logical; draw a legend for the factor names?}
  
  \item{legend.pos}{Position of the legend in the plot, as in \code{\link[graphics]{legend}}}
  
  \item{xlab,ylab}{Labels for horizontal and vertical axes. 
The default, \code{"_auto_"} means that the function auto-generates a label of 
the form \code{"Dimension X (xx.x \%)"}
}

  \item{rev.axes}{A logical vector of length 2, where TRUE reverses the direction of the
             corresponding axis}

  \item{\dots}{Arguments passed down to \code{plot}}
  
}
%\details{
%%  ~~ If necessary, more details than the description above ~~
%}
\value{
 Returns the coordinates of the category points invisibly
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
}
%\references{
%% ~put references to the literature/web site here ~
%}

\author{
Michael Friendly
}
%\note{
%%  ~~further notes~~
%}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
\code{\link[ca]{mjca}},
\code{\link[ca]{plot.mjca}}

\code{\link[ca]{cacoord}} returns CA and MCA coordinates,
\code{\link[ca]{multilines}} draw multiple lines according to a factor,

}
\examples{
require(ca)
data(Titanic)
titanic.mca <- mjca(Titanic)
mcaplot(titanic.mca, legend=TRUE, legend.pos="topleft")

data(HairEyeColor)
haireye.mca <- mjca(HairEyeColor)
mcaplot(haireye.mca, legend=TRUE, cex.lab=1.3)


}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{hplot}
%\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
