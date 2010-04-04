#####################################
## Produce a 3D mosaic plot using rgl
#####################################

# TODO:  
#  -- if ndim>3, must adjust baseline for labels to avoid overplotting
#  -- generalize the calculation of residuals
#  -- provide formula interface

# provide observed array of counts and either residuals, expected frequencies,
# or a loglin set of margins to fit

mosaic3d <- function(observed, expected=NULL, residuals=NULL, margin=NULL,
           spacing=0.1, split_dir=1:3, shading=shading_basic, ...) {


    dn <- dimnames(observed)
    vnames <- names(dn)
    levels <- dim(observed)
    ndim <- length(levels)

		# replicate arguments to number of dimensions
    spacing <- rep(spacing, length=ndim)
    split_dir <- rep(split_dir, length=ndim)

		shapelist <- cube3d(alpha=0.3)
		
		open3d()
		for (k in 1:ndim) {
			marg <- margin.table(observed, k:1)
			if (k==1) {
				shapelist <- split3d(shapelist, marg, split_dir[k], space=spacing[k])
				label3d(shapelist, split_dir[k], dn[[k]], ...)
			}
			else {
				marg <- matrix(marg, nrow=levels[k])
				shapelist <- split3dlist(shapelist, marg, split_dir[k], space=spacing[k])
				label3d(shapelist[1:levels[k]], split_dir[k], dn[[k]], ...)
			}
		}
		# fit model, if necessary to obtain residuals
		if (is.null(residuals)) {
			if (is.null(expected)) {
        if (is.null(margin)) margin <- as.list(1:ndim)
        expected <- stats::loglin(observed, margin, fit = TRUE, print = FALSE)$fit
      }
      residuals = (observed - expected)/sqrt(expected)
		}
		col <- shading(residuals)
		# display
		shapelist3d(shapelist, col=col, ...)
		
}

# split a 3D object along dimension dim, according to the proportions or
# frequencies specified in vector p

split3d <- function(obj, p, dim, space=.10) {
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

     if (dim == 1 || dim=='x') {
       xscale <- scales[i]
       xshift <- shifts[i] + min[1]*(1-xscale) + (uspace * (i-1))
     } else if (dim == 2|| dim=='y') {
       yscale <- scales[i]
       yshift <- shifts[i] + min[2]*(1-yscale) + (uspace * (i-1))
     } else if (dim == 3|| dim=='y') {
       zscale <- scales[i]
       zshift <- shifts[i] + min[3]*(1-zscale) + (uspace * (i-1))
     }

     result[[i]] <- translate3d(scale3d(obj, xscale, yscale, zscale),
                                xshift, yshift, zshift)

   }
   result
}

# split a list of 3D objects, according to the proportions specified in
# the columns of p.
# TODO: Could a single function handle all cases?

split3dlist <- function(obj, p, dim, space=.10) {
	sl <- list()
	for (i in seq_along(obj)) {
		sl <- c(sl, split3d(obj[[i]], p[,i], dim=dim, space=space))
	}
	sl	
}

# basic shading_Friendly, adapting the simple code used in mosaicplot()

shading_basic <- function(residuals, shade=TRUE) {
    if (is.logical(shade)) 
        shade <- c(2, 4)
    else if (any(shade <= 0) || length(shade) > 5) 
        stop("invalid 'shade' specification")
    shade <- sort(shade)
    breaks <- c(-Inf, -rev(shade), 0, shade, Inf)
    colors <- c(hsv(0, s = seq.int(1, to = 0, length.out = length(shade) + 
        1)), hsv(4/6, s = seq.int(0, to = 1, length.out = length(shade) + 
        1)))
    colors[as.numeric(cut(residuals, breaks))]
}

# return a 2 x 3 matrix containing the range of a 3D object

range3d <- function(obj) {
	if (!"vb" %in% names(obj)) stop("Not a mesh3d or shape3d object")
  x <- with(obj, range(vb[1,]/vb[4,]))
  y <- with(obj, range(vb[2,]/vb[4,]))
  z <- with(obj, range(vb[3,]/vb[4,]))
  result <- cbind(x,y,z)
  rownames(result)<- c('min', 'max')
  result
}

# provide labels for 3D objects below their extent along a given dimension

label3d <- function(objlist, dim, text, offset=.1, adj=c(0.5, 1), ...) {
	ranges <- lapply(objlist, range3d)
	loc <- t(sapply(ranges, colMeans))   # positions of labels on dimension dim
	min <- t(sapply(ranges, function(x) x[1,]))  # other dimensions at min values
	xyz <- min - offset
	xyz[,dim] <- loc[,dim]
	texts3d(xyz, texts=text, adj=adj, ...)
}

testme <- FALSE
if(testme) {
	require(vcdExtra)
	mosaic3d(Bartlett)
	mosaic3d(Heart)
	mosaic3d(Detergent)
}

