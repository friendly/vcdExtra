\name{vcdExtra-package}
\alias{vcdExtra-package}
\alias{vcdExtra}
\docType{package}
\title{
Extensions and additions to vcd: Visualizing Categorical Data
}
\description{
This package provides additional data sets, documentation, and
a few functions designed to extend the vcd package.  
}
\details{
\tabular{ll}{
Package: \tab vcdExtra\cr
Type: \tab Package\cr
Version: \tab 0.4-1\cr
Date: \tab 2010-02-25\cr
License: \tab GPL version 2 or newer\cr
LazyLoad: \tab yes\cr
}
The main purpose of this package is to serve as a sandbox for
introducing extensions of
mosaic plots that apply to loglinear models fitted using
\code{glm()} and related, generalized nonlinear models fitted
with \code{gnm()} in the \code{\link[gnm]{gnm-package}} package.
The method \code{\link{mosaic.glm}} is an initial implementation,
intended to extend the \code{\link[vcd]{mosaic.loglm}} method in the vcd
package to a wider class of models.

In addition, there are several new data sets, a new vignette,
\describe{
  \item{vcd-tutorial}{Tutorial: Working with categorical data with R and the vcd package, \code{vignette("vcd-tutorial", package = "vcdExtra") }}
}
and a few functions for manipulating categorical data sets.

Some of these extensions may be migrated into vcd.
}
\author{
Michael Friendly

Maintainer: Michael Friendly <friendly AT yorku.ca>
}
\references{
Friendly, M. \emph{Visualizing Categorical Data}, Cary NC: SAS Insitute, 2000.
Web materials: \url{http://www.math.yorku.ca/SCS/vcd/}.

Meyer, D.; Zeileis, A. & Hornik, K. 
The Strucplot Framework: Visualizing Multi-way Contingency Tables with vcd 
\emph{Journal of Statistical Software}, 2006, 17, 1-48.
Available in R via \code{vignette("strucplot", package = "vcd")}

Turner, H. and Firth, D. \emph{Generalized nonlinear models in R: An overview of the gnm package},
2007, 
\url{http://eprints.ncrm.ac.uk/472/}. Available in R via \code{vignette("gnmOverview", package = "gnm")}.

}
\keyword{ package }
\seealso{
\code{\link[gnm]{gnm-package}},\code{\link[vcd]{mosaic}}
%~~ Optional links to other man pages, e.g. ~~
%~~ \code{\link[<pkg>:<pkg>-package]{<pkg>}} ~~
}
%\examples{
%}
