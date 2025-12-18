### return a data.frame giving brief summaries of data sets in packages
#  package:    a character vector giving the package(s) to look in
#  allClass:   include all classes of the item (TRUE) or just the last class (FALSE)
#  incPackage: include package name in result?
#  maxTitle: maximum length of data set Title

# DONE: Make package="vcdExtra" default
# DONE: Add `ndim` arg to also return number of dimensions

#' Information on Data Sets in Packages
#'
#' The \code{\link[utils]{data}} function is used both to load data sets from
#' packages, and give a display of the names and titles of data sets in one or
#' more packages, however it does not return a result that can be easily used
#' to get additional information about the nature of data sets in packages.
#'
#' The `datasets()` function is designed to produce a more useful summary
#' display of data sets in one or more packages.  It extracts the `class`
#' and dimension information (`dim` or codelength) of each item, and
#' formats these to provide additional descriptors.
#'
#' The requested packages must be installed, and are silently loaded in order
#' to extract `class` and size information.
#'
#' @param package     a character vector giving the package(s) to look in
#' @param allClass    a logical variable. Include all classes of the item (`TRUE`) or just the last class (`FALSE`)?
#' @param incPackage  include the package name in result?
#' @param maxTitle    maximum length of data set Title
#' @param ndim        logical; include the number of dimensions in the result?
#'
#' @return A `data.frame` whose rows correspond to data sets found in `package`.
#'
#' The columns (for a single package) are:
#' \item{Item}{data set name, a character variable}
#' \item{class}{class, the object class of the data set, typically one of `"data.frame"`, `"table"`, `"array"` ...}
#' \item{dim}{an abbreviation of the dimensions of the data set, in a form like `"36x3"` for a data.frame or matrix with 36 rows and 3 columns.}
#' \item{Title}{data set title}
#'
#' @note In Rmd documents, `datasets("package") |> knitr::kable()` can be used
#' to create a more pleasing display.
#'
#' @author Michael Friendly, with R-help from Curt Seeliger
#' @seealso \code{\link[utils]{data}}, \code{\link[knitr]{kable}}
#' @keywords package data
#' @examples
#'
#' datasets("vcdExtra")
#' # datasets(c("vcd", "vcdExtra"))
#' datasets("datasets", maxTitle=50)
#'
#' # just list dataset names in a package
#' datasets("vcdExtra")[,"Item"]
#' datasets("vcd")[,"Item"]
#'
#' # Find one-way tables in vcd (dim doesn't contain "x")
#' datasets("vcd") |>
#'   dplyr::filter(class=="table",
#'                 !stringr::str_detect(dim, "x"))
#'
#'
#' @export datasets
datasets <- function(package = "vcdExtra",
                     allClass=FALSE,
                  	 incPackage=length(package) > 1,
                  	 maxTitle=NULL,
                     ndim = FALSE)
{
	# make sure requested packages are available and loaded
	for (i in seq_along(package)) {
		if (!isNamespaceLoaded(package[i]))
			if (requireNamespace(package[i], quietly=TRUE))
				cat(paste("Loading package:", package[i], "\n"))
			else stop(paste("Package", package[i], "is not available"))
	}

	dsitems <- data(package=package)$results
	wanted <- c('Package', 'Item','Title')
	ds <- as.data.frame(dsitems[,wanted], stringsAsFactors=FALSE)

	getData <- function(x, pkg) {
	  # fix items with " (...)" in names, e.g., "BJsales.lead (BJsales)" in datasets
	  objname <- gsub(" .*", "", x)

	  e <- loadNamespace(pkg)
	  if (!exists(x, envir = e)) {
	    dataname <- sub("^.*\\(", "", x)
	    dataname <- sub("\\)$", "", dataname)
	    e <- new.env()
	    data(list = dataname, package = pkg, envir = e)
	  }
	  get(objname, envir = e)
	}

	getDim <- function(i) {
	  data <- getData(ds$Item[i], ds$Package[i])
		if (is.null(dim(data))) length(data) else paste(dim(data), collapse='x')
	}

	getnDim <- function(i) {
	  data <- getData(ds$Item[i], ds$Package[i])
	  length(dim(data))
	}

	getClass <- function(i) {
	  data <- getData(ds$Item[i], ds$Package[i])
		cl <- class(data)
		if (length(cl)>1 && !allClass) cl[length(cl)] else cl
	}

	ds$ndim <- unlist(lapply(seq_len(nrow(ds)), getnDim ))
	ds$dim <- unlist(lapply(seq_len(nrow(ds)), getDim ))
	ds$class <- unlist(lapply(seq_len(nrow(ds)), getClass ))
	if (!is.null(maxTitle)) ds$Title <- substr(ds$Title, 1, maxTitle)

  # what to return?
	fields <- if(ndim) c('Item','class', 'ndim', 'dim','Title') else c('Item','class','dim','Title')
	if (incPackage) fields <- c("Package", fields)
	# return the requested fields
	ds[fields]

}
