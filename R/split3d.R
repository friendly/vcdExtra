# split a 3D object along dimension dim, according to the proportions or
# frequencies specified in vector p



#' Subdivide a 3D Object
#'
#' Subdivides a `shape3d` object or a list of `shape3d` objects into
#' objects of the same shape along a given dimension according to the
#' proportions or frequencies specified in vector(s).
#'
#' `split3d` is the basic workhorse used in \code{\link{mosaic3d}}, but
#' may be useful in other contexts.
#'
#' `range3d` and `center3d` are utility functions, also useful in
#' other contexts.
#'
#' The resulting list of `shape3d` objects is actually composed of
#' *copies* of the input object(s), scaled according to the proportions in
#' `p` and then translated to make their range along the splitting
#' dimension equal to that of the input object(s).
#'
#' @aliases split3d split3d.shape3d split3d.list range3d center3d
#' @param obj A `shape3d` object, or a list composed of them
#' @param \dots Other arguments for split3d methods
#' @param p For a single `shade3d` object, a vector of proportions (or a
#' vector of non-negative numbers which will be normed to proportions)
#' indicating the number of subdivisions and their scaling along dimension
#' `dim`. For a list of `shade3d` objects, a matrix whose columns
#' indicate the subdivisions of each object.
#' @param dim The dimension along which the object is to be subdivided. Either
#' an integer: 1, 2, or 3, or a character: "x", "y", or "z".
#' @param space The total space used to separate the copies of the object along
#' dimension `dim`. The unit inter-object space is therefore
#' `space/(length(p)-1)`.
#' @return `split3d` returns a list of `shape3d` objects.
#'
#' `range3d` returns a 2 x 3 matrix, whose first row contains the minima
#' on dimensions x, y, z, and whose second row contains the maxima.
#'
#' `center3d` returns a numeric vector containing the means of the minima
#' and maxima on dimensions x, y, z.
#' @author Duncan Murdoch, with refinements by Michael Friendly
#' @seealso \code{\link{mosaic3d}}
#'
#' \code{\link[rgl]{shapelist3d}} for the plotting of lists of `shape3d`
#' objects.
#' @importFrom rgl translate3d
#' @keywords dplot
#' @examples
#'
#' if (require(rgl)) {
#'   open3d()
#'   cube <- cube3d(alpha=0.4)
#'   sl1 <- split3d(cube, c(.2, .3, .5), 1)
#'   col <- c("#FF000080", "#E5E5E580", "#0000FF80")
#'   shapelist3d(sl1, col=col)
#'
#'   open3d()
#'   p <- matrix(c(.6, .4, .5, .5, .2, .8), nrow=2)
#'   sl2 <- split3d(sl1, p, 2)
#'   shapelist3d(sl2, col=col)
#'   }
#'
#' @export split3d
split3d <- function(obj, ...) {
	UseMethod("split3d")
}

#' @rdname split3d
#' @export
split3d.shape3d <- function(obj, p, dim, space=.10, ...) {
	range <-range3d(obj)
	min <- range[1,]
	p <- p/sum(p)                 # assure proportions
	uspace <- space/(length(p)-1) # unit space between objects
	scales <- p * (1-space)
	shifts <- c(0, cumsum(p)[-length(p)])*diff(range[,dim])
	result <- list()
	for (i in seq_along(p)) {
		xscale <- yscale <- zscale <- 1
		xshift <- yshift <- zshift <- 0

		if (dim == 1 || tolower(dim)=='x') {
			xscale <- scales[i]
			xshift <- shifts[i] + min[1]*(1-xscale) + (uspace * (i-1))
		} else if (dim == 2|| tolower(dim)=='y') {
			yscale <- scales[i]
			yshift <- shifts[i] + min[2]*(1-yscale) + (uspace * (i-1))
		} else if (dim == 3|| tolower(dim)=='y') {
			zscale <- scales[i]
			zshift <- shifts[i] + min[3]*(1-zscale) + (uspace * (i-1))
		}

		result[[i]] <- rgl::translate3d(rgl::scale3d(obj, xscale, yscale, zscale),
				xshift, yshift, zshift)

	}
	result
}

# split a list of 3D objects, according to the proportions specified in
# the columns of p.

#' @rdname split3d
#' @export
split3d.list <- function(obj, p, dim, space=.10, ...) {
	nl <- length(obj)
	if (!is.matrix(p) || ncol(p) != nl) stop(gettextf("p must be a matrix with %i columns", nl))
	sl <- list()
	for (i in seq_along(obj)) {
		sl <- c(sl, split3d(obj[[i]], p[,i], dim=dim, space=space))
	}
	sl
}

#range3d <- function(obj, ...) {
#	UseMethod("range3d")
#}

#' @rdname split3d
#' @export
range3d <- function(obj) {
	if (!"vb" %in% names(obj)) stop("Not a mesh3d or shape3d object")
  x <- with(obj, range(vb[1,]/vb[4,]))
  y <- with(obj, range(vb[2,]/vb[4,]))
  z <- with(obj, range(vb[3,]/vb[4,]))
  result <- cbind(x,y,z)
  rownames(result)<- c('min', 'max')
  result
}

#' @rdname split3d
#' @export
center3d <- function(obj) {
	range <-range3d(obj)
	colMeans(range)
}
