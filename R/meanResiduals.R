##' Computes the mean working residual from a model fitted using
##' Iterative Weighted Least Squares for each level of a factor or
##' interaction of factors.
##'
##' <details>
##'
##' @title Average Residuals within Factor Levels
##' @param object model object for which \code{object$residuals} gives the
##' working residuals and \code{object$weights} gives the working weights.
##' @param by a list of factors.
##' @param standardized logical: if \code{TRUE}, the mean residuals are
##' standardized to be approximately standard normal.
##' @param as.table logical: if \code{TRUE} the result is returned as a table
##' cross-classified by the factors passed to \code{by}.
##' @param ... currently ignored
##' @return If \code{as.table == TRUE}, the mean residuals cross-classified
##' by the factors passed to \code{by}, otherwise a vector of mean residuals.
##' In either case the returned object has a single attribute,
##' \code{"weights"} which gives the weight associated with each
##' grouped residual.
##' @author Heather Turner
##' @example examples/meanResiduals.R
meanResiduals <- function(object, by = NULL, standardized = TRUE, as.table = TRUE, ...){
  r <- object$residuals
  ## recompute weights for better accuracy
  w  <- as.numeric(object$prior.weights * object$family$mu.eta(predict(object, type = "link"))^2/
                   object$family$variance(object$fitted))
  if (is.null(by))
    stop("`by' must be specified in order to compute grouped residuals")
  if (!is.list(by))
    stop("`by' must be a list")
  agg.wts <- tapply(w, by, sum) #unlike rowsum, keeps all levels of interaction
  res <- tapply(r * w, by, sum)/agg.wts
  if (standardized) res <- res * sqrt(agg.wts)
  if (!as.table){
    structure(c(tab), weights = c(agg.wts))
  }
  else
    structure(as.table(res), weights = as.table(agg.wts))
}


