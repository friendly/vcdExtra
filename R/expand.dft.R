# Originally from Marc Schwarz
# Ref: http://tolstoy.newcastle.edu.au/R/e6/help/09/01/1873.html

# 23 Feb 22: Fix warning from type.convert

expand.dft <- function(x, var.names = NULL, freq = "Freq", ...)
{
  #  allow: a table object, or a data frame in frequency form
  if(inherits(x, "table"))
    x <- as.data.frame.table(x, responseName = freq)

  freq.col <- which(colnames(x) == freq)
  if (length(freq.col) == 0)
      stop(paste(sQuote("freq"), "not found in column names"))

  DF <- sapply(1:nrow(x),
               function(i) x[rep(i, each = x[i, freq.col, drop = TRUE]), ],
               simplify = FALSE)

  DF <- do.call("rbind", DF)[, -freq.col, drop=FALSE]

  for (i in 1:ncol(DF))
  {
    DF[[i]] <- type.convert(as.character(DF[[i]]), as.is=TRUE, ...)
## DONE ##: Generates warning: 
##    1: In type.convert.default(as.character(DF[[i]]), ...) :
##      'as.is' should be specified by the caller; using TRUE
  }

  rownames(DF) <- NULL

  if (!is.null(var.names))
  {
    if (length(var.names) < dim(DF)[2])
    {
      stop(paste("Too few", sQuote("var.names"), "given."))
    } else if (length(var.names) > dim(DF)[2]) {
      stop(paste("Too many", sQuote("var.names"), "given."))
    } else {
      names(DF) <- var.names
    }
  }

  DF
}

# make this a synonym
expand.table <- expand.dft
