#####################################
## Produce a 3D mosaic plot using rgl
#####################################

# TODO: provide formula interface
# TODO: handle zero margins (causes display to be erased in shapelist3d)
# DONE: handle zero cells
# DONE: generalize the calculation of residuals
# DONE: allow display of type=c("observed", "expected")
# DONE: if ndim>3, provide for labels at max or min
# DONE: make object oriented and provide a loglm method

# mosaic3d: provide observed array of counts and either residuals, expected frequencies,
# or a loglin set of margins to fit



#'
#' 3D Mosaic Plots
#'
#' Produces a 3D mosaic plot for a contingency table (or a
#' \code{link[MASS]{loglm}} model) using the \code{\link[rgl]{rgl-package}}.
#'
#' Generalizing the 2D mosaic plot, this begins with a given 3D shape (a unit
#' cube), and successively sub-divides it along the X, Y, Z dimensions
#' according to the table margins, generating a nested set of 3D tiles. The
#' volume of the resulting tiles is therefore proportional to the frequency
#' represented in the table cells. Residuals from a given loglinear model are
#' then used to color or shade each of the tiles.
#'
#' This is a developing implementation.  The arguments and details are subject
#' to change.
#'
#' Friendly (1995), Friendly (2000, Sect. 4.5) and Theus and Lauer (1999) have
#' all used the idea of 3D mosaic displays to explain various aspects of
#' loglinear models (the iterative proportional fitting algorithm, the
#' structure of various models for 3-way and n-way tables, etc.), but no
#' implementation of 3D mosaics was previously available.
#'
#' For the default method, residuals, used to color and shade the 3D tiles, can
#' be passed explicitly, or, more typically, are computed as needed from
#' observed and expected frequencies. In this case, the expected frequencies
#' are optionally computed for a specified loglinear model given by the
#' `expected` argument. For the loglm method, residuals and observed
#' frequencies are calculated from the model object.
#'
#' @aliases mosaic3d mosaic3d.default mosaic3d.loglm
#'
#' @param x A \code{link[MASS]{loglm}} model object. Alternatively, a multidimensional `array` or `table`
#'          or\code{\link[vcd]{structable}} of frequencies in a contingency table.  In the present implementation, the dimensions
#'          are taken in sequential order. Use \code{link[base]{aperm}} or \code{\link[vcd]{structable}} to change this.
#' @param expected optionally, for contingency tables, an array of expected frequencies of the same dimension as `x`, or alternatively the
#'          corresponding loglinear model specification as used by\code{link[stats]{loglin}} or \code{link[MASS]{loglm}} (see
#'          \code{\link[vcd]{structable}} for details).
#' @param residuals optionally, an array of residuals of the same dimension as `x` (see details).
#' @param type a character string indicating whether the `"observed"` or the `"expected"` frequencies in the table should be
#'          visualized by the volume of the 3D tiles.
#' @param residuals_type a character string indicating the type of residuals to be computed when none are supplied.  If residuals is `NULL`,
#'          `residuals_type` must be one of `"pearson"` (default; giving components of Pearson's chi-squared), `"deviance"`
#'          (giving components of the likelihood ratio chi-squared), or `"FT"` for the Freeman-Tukey residuals.  The value of this
#'          argument can be abbreviated.
#' @param shape The initial 3D shape on which the mosaic is based.  Typically this is a call to an rgl function, and must produce a `shape3d` object.
#'          The default is a "unit cube" on (-1, +1), with transparency specified by `alpha`.
#' @param alpha Specifies the transparency of the 3D tiles used to compose the 3D mosaic.
#' @param spacing A number or vector giving the total amount of space used to separate the 3D tiles along each of the
#'          dimensions of the table. The values specified are re-cycled to the number of table dimensions.
#' @param split_dir A numeric vector composed of the integers `1:3` or a character vector composed of `c("x", "y", "z")`, where
#'          `split_dir[i]` specifies the axis along which the tiles should be split
#'          for dimension `i` of the table. The values specified are re-cycled to the number of table dimensions.
#' @param shading A function, taking an array or vector of residuals for the
#'          given model, returning a vector of colors.  At present, only the default `shading=shading_basic` is provided.
#'          This is roughly equivalent to the
#'          use of the `shade` argument in \code{\link[graphics]{mosaicplot}} or to
#'          the use of `gp=shading_Friendly` in \code{\link[vcd]{mosaic}}.
#' @param interpolate a vector of interpolation values for the `shading` function.
#' @param zero_size The radius of a small sphere used to mark zero cells in the display.
#' @param label_edge A character vector composed of `c("-", "+")` indicating whether the labels for a given table dimension
#'          are to be written at the minima (`"-"`) or maxima (`"+"`) of the *other*
#'          dimensions in the plot. The default is `rep( c('-', '+'), each=3, length=ndim)`, meaning that the first three
#'          table variables are labeled at the minima, and successive ones at the maxima.
#' @param labeling_args This argument is intended to be used to specify details of the rendering of labels for the table dimensions, but
#'          at present has no effect.
#' @param newpage logical indicating whether a new page should be created for the plot or not.
#' @param box logical indicating whether a bounding box should be drawn around the plot.
#' @param \dots Other arguments passed down to `mosaic.default` or 3D functions.
#'
#' @return Invisibly, the list of `shape3d` objects used to draw the 3D
#' mosaic, with names corresponding to the concatenation of the level labels,
#' separated by ":".
#'
#' @author Michael Friendly, with the help of Duncan Murdoch and Achim Zeileis
#'
#' @seealso
#' \code{\link[vcd]{strucplot}}, \code{\link[vcd]{mosaic}},
#' \code{\link[graphics]{mosaicplot}}
#'
#' \code{\link[stats]{loglin}}, \code{\link[MASS]{loglm}} for details on
#' fitting loglinear models
#' @family mosaic plots
#'
#' @references Friendly, M. (1995). Conceptual and Visual Models for
#' Categorical Data, *The American Statistician*, **49**, 153-160.
#'
#' Friendly, M. *Visualizing Categorical Data*, Cary NC: SAS Institute,
#' 2000. Web materials: <http://www.datavis.ca/books/vcd/>.
#'
#' Theus, M. & Lauer, S. R. W. (1999) Visualizing Loglinear Models.
#' *Journal of Computational and Graphical Statistics*, **8**, 396-412.
#' @keywords hplot
#' @examples
#'
#' # 2 x 2 x 2
#' if(requireNamespace("rgl")){
#' mosaic3d(Bartlett, box=TRUE)
#' # compare with expected frequencies under model of mutual independence
#' mosaic3d(Bartlett, type="expected", box=TRUE)
#'
#' # 2 x 2 x 3
#' mosaic3d(Heart, box=TRUE)
#' }
#'
#' \dontrun{
#' # 2 x 2 x 2 x 3
#' # illustrates a 4D table
#' mosaic3d(Detergent)
#'
#' # compare 2D and 3D mosaics
#' demo("mosaic-hec")
#' }
#'
#'
#' @export
mosaic3d <- function(x, ...) {
	UseMethod("mosaic3d")
}

#' @rdname mosaic3d
#' @export
mosaic3d.loglm <-
function (x, type = c("observed", "expected"),
    residuals_type = c("pearson", "deviance"),
#    gp = shading_hcl, gp_args = list(),
    ...)
{
    residuals_type <- match.arg(tolower(residuals_type), c("pearson", "deviance"))
    if (is.null(x$fitted))
        x <- update(x, fitted = TRUE)
    expected <- fitted(x)
    residuals <- residuals(x, type = "pearson")
    observed <- residuals * sqrt(expected) + expected
    if (residuals_type == "deviance")
        residuals <- residuals(x, type = "deviance")
#    gp <- if (inherits(gp, "grapcon_generator"))
#        do.call("gp", c(list(observed, residuals, expected, x$df),
#            as.list(gp_args)))
#    else gp
    mosaic3d.default(observed, residuals = residuals, expected = expected,
        type = type, residuals_type = residuals_type,
#        gp = gp,
        ...)
}

#' @rdname mosaic3d
#' @importFrom vcd is.structable
#' @export
mosaic3d.default <- function(x, expected=NULL, residuals=NULL,
		type = c("observed", "expected"), residuals_type = NULL,
		shape=rgl::cube3d(alpha=alpha), alpha=0.5,
		spacing=0.1, split_dir=1:3,
		shading=shading_basic, interpolate=c(2,4), zero_size=.05,
		label_edge,
		labeling_args=list(), newpage=TRUE, box=FALSE, ...) {

  if (!requireNamespace("rgl")) stop("rgl is required")

  type <- match.arg(type)
  if (is.null(residuals)) {
      residuals_type <- if (is.null(residuals_type))
          "pearson"
      else match.arg(tolower(residuals_type), c("pearson",
          "deviance", "ft"))
  }

  ## convert structable object
  if (vcd::is.structable(x)) {
    x <- as.table(x)
  }

  ## table characteristics
  levels <- dim(x)
  ndim <- length(levels)
  dn <- dimnames(x)
  if (is.null(dn))
    dn <- dimnames(x) <- lapply(levels, seq)
  vnames <- names(dimnames(x))
  if (is.null(vnames))
    vnames <- names(dn) <- names(dimnames(x)) <- LETTERS[1:ndim]

  ## replace NAs by 0
  if (any(nas <- is.na(x))) x[nas] <- 0

  ## model fitting:
  ## calculate expected if needed
  if ((is.null(expected) && is.null(residuals)) ||
      !is.numeric(expected)) {
      if (inherits(expected, "formula")) {
          fm <- loglm(expected, x, fitted = TRUE)
          expected <- fitted(fm)
          df <- fm$df
      } else {
          if (is.null(expected))
              expected <-  as.list(1:ndim)
          fm <- loglin(x, expected, fit = TRUE, print = FALSE)
          expected <- fm$fit
          df <- fm$df
      }
  }

  ## compute residuals
  if (is.null(residuals))
    residuals <- switch(residuals_type,
                        pearson = (x - expected) / sqrt(ifelse(expected > 0, expected, 1)),
                        deviance = {
                          tmp <- 2 * (x * log(ifelse(x == 0, 1, x / ifelse(expected > 0, expected, 1))) - (x - expected))
                          tmp <- sqrt(pmax(tmp, 0))
                          ifelse(x > expected, tmp, -tmp)
                        },
                        ft = sqrt(x) + sqrt(x + 1) - sqrt(4 * expected + 1)
                        )
  ## replace NAs by 0
  if (any(nas <- is.na(residuals))) residuals[nas] <- 0

	# switch observed and expected if required
  observed <- if (type == "observed") x else expected
  expected <- if (type == "observed") expected else x


	# replicate arguments to number of dimensions
	spacing <- rep(spacing, length=ndim)
	split_dir <- rep(split_dir, length=ndim)
	if(missing(label_edge))
		label_edge <- rep( c('-', '+'), each=3, length=ndim)

	zeros <- observed <= .Machine$double.eps

	shapelist <- shape
	# sanity check
	if (!inherits(shapelist, "shape3d")) stop("shape must be a shape3d object")

	if (newpage) rgl::open3d()
	for (k in 1:ndim) {
		marg <- margin.table(observed, k:1)
		if (k==1) {
			shapelist <- split3d(shapelist, marg, split_dir[k], space=spacing[k])
			label3d(shapelist, split_dir[k], dn[[k]], vnames[k], edge=label_edge[k], ...)
		}
		else {
			marg <- matrix(marg, nrow=levels[k])
			shapelist <- split3d(shapelist, marg, split_dir[k], space=spacing[k])
			names(shapelist) <- apply(as.matrix(expand.grid(dn[1:k])), 1, paste, collapse=":")
			L <- length(shapelist)
			label_cells <- if (label_edge[k]=='-') 1:levels[k]
											else (L-levels[k]+1):L
			label3d(shapelist[label_cells], split_dir[k], dn[[k]], vnames[k], edge=label_edge[k], ...)
		}
	}

	# assign colors
	# TODO: allow alpha to control transparency of side walls
	col <- shading(residuals, interpolate=interpolate)

	# display, but exclude the zero cells
	rgl::shapelist3d(shapelist[!as.vector(zeros)], col=col[!as.vector(zeros)], ...)

	# plot markers for zero cells
	if (any(zeros)) {
		ctrs <- t(sapply(shapelist, center3d))
		rgl::spheres3d(ctrs[as.vector(zeros),], radius=zero_size)
	}

#	invisible(structable(observed))
	invisible(shapelist)

}


# basic shading_Friendly, adapting the simple code used in mosaicplot()

shading_basic <- function(residuals, interpolate=TRUE) {
	if (is.logical(interpolate))
		interpolate <- c(2, 4)
	else if (any(interpolate <= 0) || length(interpolate) > 5)
		stop("invalid 'interpolate' specification")
	shade <- sort(interpolate)
	breaks <- c(-Inf, -rev(shade), 0, shade, Inf)
	colors <- c(hsv(0, s = seq.int(1, to = 0, length.out = length(shade) +
									1)), hsv(4/6, s = seq.int(0, to = 1, length.out = length(shade) +
									1)))
	colors[as.numeric(cut(residuals, breaks))]
}


# provide labels for 3D objects below/above their extent along a given dimension
# FIXME: kludge for interline gap between level labels and variable name
# TODO: how to pass & extract labeling_args, e.g., labeling_args=list(at='min', fontsize=10)

label3d <- function(objlist, dim, text, varname, offset=.05, adj, edge="-", gap=.1, labeling_args, ...) {
	if(missing(adj)) {
		if (dim < 3) adj <- ifelse(edge == '-', c(0.5, 1), c(0.5, 0))
		else         adj <- ifelse(edge == '-', c(1, 0.5), c(0, 0.5))
	}
	ranges <- lapply(objlist, range3d)
	loc <- t(sapply(ranges, colMeans))   # positions of labels on dimension dim
	min <- t(sapply(ranges, function(x) x[1,]))  # other dimensions at min values
	max <- t(sapply(ranges, function(x) x[2,]))  # other dimensions at max values
	xyz <- if (edge == '-') (min - offset) else (max + offset)
	xyz[,dim] <- loc[,dim]
	if(!missing(varname)) {
		loclab <- colMeans(loc)               # NB: doesn't take space into acct
		xyzlab <- if (edge == '-')
			min[1,] - offset - gap
			else max[1,] + offset + gap
		xyzlab[dim] <- loclab[dim]
		xyz <- rbind(xyz, xyzlab)
		text <- c(text, varname)
	}
	result <- c(labels = rgl::texts3d(xyz, texts=text, adj=adj, ...))
	invisible(result)
}



