# calculate bivariate logits and OR


#' Bivariate Logits and Log Odds Ratio
#' 
#' This function calculates the log odds and log odds ratio for two binary
#' responses classified by one or more stratifying variables.
#' 
#' It is useful for plotting the results of bivariate logistic regression
#' models, such as those fit using \code{\link[VGAM]{vglm}} in the \pkg{VGAM}.
#' 
#' For two binary variables with levels 0,1 the logits are calculated assuming
#' the columns in `Y` are given in the order 11, 10, 01, 00, so the logits
#' give the log odds of the 1 response compared to 0.  If this is not the case,
#' either use `rev=TRUE` or supply `Y[,4:1]` as the first argument.
#' 
#' @param Y A four-column matrix or data frame whose columns correspond to the
#' 2 x 2 combinations of two binary responses.
#' @param add Constant added to all cells to allow for zero frequencies.  The
#' default is 0.5 if `any(Y)==0` and 0 otherwise.
#' @param colnames Names for the columns of the results. The default is
#' `c("logit1", "logit2", "logOR")`.  If less than three names are
#' supplied, the remaining ones are filled in from the default.
#' @param row.vars A data frame or matrix giving the factor levels of one or
#' more factors corresponding to the rows of `Y`
#' @param rev A logical, indicating whether the order of the columns in
#' `Y` should be reversed.
#' @return A data frame with `nrow(Y)` rows and `3 + ncol(row.vars)`
#' columns
#' @author Michael Friendly
#' @seealso \code{\link[VGAM]{vglm}}
#' @references Friendly, M. and Meyer, D. (2016).  *Discrete Data Analysis
#' with R: Visualization and Modeling Techniques for Categorical and Count
#' Data*.  Boca Raton, FL: Chapman & Hall/CRC. <http://ddar.datavis.ca>.
#' @keywords manip
#' @examples
#' 
#' data(Toxaemia)
#' tox.tab <- xtabs(Freq~class + smoke + hyper + urea, Toxaemia)
#' 
#' # reshape to 4-column matrix
#' toxaemia <- t(matrix(aperm(tox.tab), 4, 15))
#' colnames(toxaemia) <- c("hu", "hU", "Hu", "HU")
#' rowlabs <- expand.grid(smoke=c("0", "1-19", "20+"), class=factor(1:5))
#' toxaemia <- cbind(toxaemia, rowlabs)
#' 
#' # logits for H and U
#' logitsTox <- blogits(toxaemia[,4:1], 
#'                      add=0.5, 
#'                      colnames=c("logitH", "logitW"), 
#'                      row.vars=rowlabs)
#' logitsTox
#' 
#' 
#' @export blogits
blogits <- function(Y, add, colnames, row.vars, rev=FALSE) {

  if (ncol(Y) != 4) stop("Y must have 4 columns")
  if (missing(add)) add <- if (any(Y==0)) 0.5 else 0
  Y <- Y + add
  if (rev) Y <- Y[,4:1]
  L <- matrix(0, nrow(Y), 3)
  L[,1] <- log( (Y[,1] + Y[,2]) / (Y[,3] + Y[,4]) )
  L[,2] <- log( (Y[,1] + Y[,3]) / (Y[,2] + Y[,4]) )
  L[,3] <- log( (Y[,1] * Y[,4]) / ((Y[,2] * Y[,3])) )
  cn <- c("logit1", "logit2", "logOR")
  colnames(L) <- if(missing(colnames)) cn else c(colnames, cn[-(1:length(colnames))])
  if(!missing(row.vars)) L <- cbind(L, row.vars)
  L
}

