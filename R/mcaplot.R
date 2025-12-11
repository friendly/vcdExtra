#' Simple and enhanced plot of MCA solutions
#'
#' This function is intended as an alternative to \code{\link[ca]{plot.mjca}}
#' for plotting multiple correspondence analysis solutions. It provides more
#' flexibility for labeling factor levels and connecting them with lines. It
#' does not support some features of `plot.mjca` (centroids, supplementary
#' points, arrows, etc.)
#'
#'
#' @param obj An `"mjca"` object
#' @param map Character string specifying the map type, i.e., the scaling
#'        applied to coordinates for different types of MCA representations. Allowed
#'        options include: `"symmetric"` (default), `"rowprincipal"`,
#'        `"colprincipal"`,
#'        `"symbiplot"`,
#'        `"rowgab"`,
#'        `"colgab"`,
#'        `"rowgreen"`,
#'        `"colgreen"`.
#'        See \code{\link[ca]{mjca}} for details.
#' @param dim Dimensions to plot, an integer vector of length 2
#' @param col Vector of colors, one for each factor in the MCA
#' @param pch Vector of point symbols for the category levels, one for each factor
#' @param cex Character size for points and level labels
#' @param pos Position of level labels relative to the category points; either
#'        a single number or a vector of length equal to the number of category points.
#' @param lines A logical or an integer vector indicating which factors are to
#'        be joined with lines using \code{\link[ca]{multilines}}
#' @param lwd Line width(s) for the lines
#' @param legend Logical; draw a legend for the factor names?
#' @param legend.pos Position of the legend in the plot, as in \code{\link[graphics]{legend}}
#' @param xlab,ylab Labels for horizontal and vertical axes.  The default,
#'        `"_auto_"` means that the function auto-generates a label of the form
#'        `"Dimension X (xx.x \%)"`
#' @param rev.axes A logical vector of length 2, where TRUE reverses the
#'        direction of the corresponding axis
#' @param \dots Arguments passed down to `plot`
#' @return Returns the coordinates of the category points invisibly
#'
#' @author Michael Friendly
#' @seealso \code{\link[ca]{mjca}}, \code{\link[ca]{plot.mjca}}
#'          \code{\link[ca]{cacoord}} returns CA and MCA coordinates,
#'          \code{\link[ca]{multilines}} draw multiple lines according to a factor,
#' @keywords hplot
#' @importFrom ca cacoord multilines
#' @examples
#'
#' require(ca)
#' data(Titanic)
#' titanic.mca <- mjca(Titanic)
#' mcaplot(titanic.mca, legend=TRUE, legend.pos="topleft")
#'
#' data(HairEyeColor)
#' haireye.mca <- mjca(HairEyeColor)
#' mcaplot(haireye.mca, legend=TRUE, cex.lab=1.3)
#'
#'
#'
#' @export mcaplot
mcaplot <- function(obj,
                    map="symmetric",
                    dim=1:2,
	col=c("blue", "red", "brown", "black", "green3", "purple"),
	pch=15:20, cex=1.2, pos=3,
	lines=TRUE, lwd=2,
	legend=FALSE, legend.pos="topright",
  xlab      = "_auto_",
  ylab      = "_auto_",
  rev.axes  = c(FALSE, FALSE),
	...) {

  if(!requireNamespace("ca", quietly=TRUE)) stop("The ca package is required")
	if(!inherits(obj, "mjca")) stop("Only defined for mjca objects")
	coords <- cacoord(obj, type=map, rows=FALSE)
	coords <- data.frame(coords, obj$factors)     # extract factor names & levels
	nlev <- obj$levels.n
	nfac <- length(nlev)
	pch <- rep_len(pch, nfac)
	col <- rep_len(col, nfac)
	lwd <- rep_len(lwd, nfac)
	rev.axes <- rep(rev.axes, length.out=2)

	if(any(dim > ncol(coords)))
      stop("dim must be valid dimensions of the coordinates")
	labs <- pctlab(obj)
	if (xlab == "_auto_") xlab <- labs[dim[1]]
	if (ylab == "_auto_") ylab <- labs[dim[2]]

  if(isTRUE(rev.axes[1])) coords[, dim[1]] <- -coords[, dim[1]]
  if(isTRUE(rev.axes[2])) coords[, dim[2]] <- -coords[, dim[2]]

	plot(coords[, dim], type='n', asp=1, xlab=xlab, ylab=ylab, ...)
	points(coords[,dim], pch=rep(pch, nlev), col=rep(col, nlev), cex=cex)
	text(coords[,dim], labels=coords$level, col=rep(col, nlev), pos=pos, cex=cex, xpd=TRUE)
	if (is.logical(lines)) lines <- if(lines) 1:nfac else NULL
	if(length(lines))
		multilines(coords[, dim], group=coords$factor, which=lines, col=col, lwd=lwd)
	abline(h = 0, v = 0, lty = "longdash", col="gray")

	if (legend) {
		factors <- coords$factor
		factors <- factors[!duplicated(factors)]
		legend(legend.pos, legend=factors,
			title="Factor", title.col="black",
			col=col, text.col=col, pch=pch,
			bg=rgb(.95, .95, .95, .3), cex=cex)
	}
	invisible(coords)
}

pctlab <- function(obj, prefix="Dimension ", decimals=1) {
  values <- obj$sv^2
  if (obj$lambda == "JCA"){
    pct <- rep_len(NA, length(values))
    } else {
	if (obj$lambda == "adjusted") {
      values <- obj$inertia.e
      pct <- round(100 * values, decimals)
      } else {
      pct <- round(100 * values / sum(values), decimals)
      }
    }
  pctval <- ifelse(is.na(pct), NULL, paste0(" (", pct, "%)"))
  paste0(prefix, 1:length(values), pctval)
}

