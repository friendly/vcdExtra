### return a data.frame giving brief summaries of data sets in packages
#  package:    a character vector giving the package(s) to look in
#  allClass:   include all classes of the item (TRUE) or just the last class (FALSE)
#  incPackage: include package name in result?
#  maxTitle: maximum length of data set Title

datasets <- function(package, allClass=FALSE, 
		incPackage=length(package) > 1,
		maxTitle=NULL) 
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
	getClass <- function(i) {
	  data <- getData(ds$Item[i], ds$Package[i])
		cl <- class(data)
		if (length(cl)>1 && !allClass) cl[length(cl)] else cl
	}
	ds$dim <- unlist(lapply(seq_len(nrow(ds)), getDim ))
	ds$class <- unlist(lapply(seq_len(nrow(ds)), getClass ))
	if (!is.null(maxTitle)) ds$Title <- substr(ds$Title, 1, maxTitle)
	if (incPackage)
		ds[c('Package', 'Item','class','dim','Title')]
	else
		ds[c('Item','class','dim','Title')]
}
