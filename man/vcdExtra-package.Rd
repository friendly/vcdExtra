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
Version: \tab 0.2\cr
Date: \tab 2009-02-01\cr
License: \tab GPL version 2 or newer\cr
LazyLoad: \tab yes\cr
}
The main purpose of this package is to introduce extensions of
mosaic plots that apply to loglinear models fitted using
\code{glm()} and related, generalized nonlinear models fitted
with \code{gnm()} in the \code{\link[gnm]{gnm-package}} package.
The methods \code{\link{mosaic.glm}} and \code{\link{mosaic.gnm}}
are initial implementations, intended to complement the
\code{\link[vcd]{mosaic.loglm}} method in the vcd package.

In addition ...
\describe{
  \item{vcd-tutorial}{\code{vignette("vcd-tutorial", package = "vcdExtra") }}
}
}
\author{
Michael Friendly

Maintainer: Michael Friendly <friendly@yorku.ca>
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
