# Originally from Marc Schwarz
# Ref: http://tolstoy.newcastle.edu.au/R/e6/help/09/01/1873.html

# 23 Feb 22: Fix warning from type.convert



#' Expand a frequency table to case form
#' 
#' Converts a frequency table, given either as a table object or a data frame
#' in frequency form to a data frame representing individual observations in
#' the table.
#' 
#' `expand.table` is a synonym for `expand.dft`.
#' 
#' @aliases expand.dft expand.table
#' @param x A table object, or a data frame in frequency form containing
#' factors and one numeric variable representing the cell frequency for that
#' combination of factors.
#' @param var.names A list of variable names for the factors, if you wish to
#' override those already in the table
#' @param freq The name of the frequency variable in the table
#' @param \dots Other arguments passed down to `type.convert`.  In
#' particular, pay attention to `na.strings` (default:
#' `na.strings=NA` if there are missing cells) and `as.is` (default:
#' `as.is=FALSE`, converting character vectors to factors).
#' @return A data frame containing the factors in the table and as many
#' observations as are represented by the total of the `freq` variable.
#' @author Mark Schwarz
#' @seealso \code{\link[utils]{type.convert}},
#' \code{\link[gnm]{expandCategorical}}
#' @references Originally posted on R-Help, Jan 20, 2009,
#' http://tolstoy.newcastle.edu.au/R/e6/help/09/01/1873.html
#' 
#' Friendly, M. and Meyer, D. (2016).  *Discrete Data Analysis with R:
#' Visualization and Modeling Techniques for Categorical and Count Data*.  Boca
#' Raton, FL: Chapman & Hall/CRC. <http://ddar.datavis.ca>.
#' @keywords manip array
#' @examples
#' 
#' library(vcd)
#' art <- xtabs(~Treatment + Improved, data = Arthritis)
#' art
#' artdf <- expand.dft(art)
#' str(artdf)
#' 
#' # 1D case
#' (tab <- table(sample(head(letters), 20, replace=TRUE)))
#' expand.table(tab, var.names="letter")
#' 
#' 
#' @export
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
#' @export
expand.table <- expand.dft
