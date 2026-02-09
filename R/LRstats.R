# ✔️DONE: fixed buglet when deviance() returns a null
# ✔️DONE: fixed bug: residual df calculated incorrectly but this now depends on objects having a df.residual component. This is TRUE for lm, glm, polr, negbin objects
# ✔️DONE: made generic, adding a glmlist method
# ✔️DONE: Provide a `label` argument to allow use of symbolic formulas as labels for the models, via `get_models()`



#' Brief Summary of Model Fit for glm and loglm Models
#'
#' For `glm` objects, the `print` and `summary` methods give too
#' much information if all one wants to see is a brief summary of model
#' goodness of fit, and there is no easy way to display a compact comparison of
#' model goodness of fit for a collection of models fit to the same data. All
#' `loglm` models have equivalent glm forms, but the `print` and
#' `summary` methods give quite different results.
#'
#' `LRstats` provides a brief summary for one or more models fit to the
#' same dataset for which `logLik` and `nobs` methods exist (e.g.,
#' `glm` and `loglm` models).
#'
#' The function relies on residual degrees of freedom for the LR chisq test
#' being available in the model object.  This is true for objects inheriting
#' from `lm`, `glm`, `loglm`, `polr` and `negbin`.
#'
#' @aliases LRstats LRstats.glmlist LRstats.loglmlist LRstats.default
#'
#' @param object a fitted model object for which there exists a logLik method
#'        to extract the corresponding log-likelihood
#' @param \dots optionally more fitted model objects
#' @param saturated saturated model log likelihood reference value (use 0 if
#'        deviance is not available)
#' @param sortby either a numeric or character string specifying the column in
#'        the result by which the rows are sorted (in decreasing order)
#' @param label character string specifying how to label the rows: `"name"`
#'         (default) uses the model object names; `"formula"` uses model formulas or
#'         bracket notation obtained from \code{\link{get_models}}.
#'         Only available for \code{glmlist} and \code{loglmlist} objects.
#' @param label.args a list of additional arguments passed to \code{\link{get_models}}
#'         when `label = "formula"`. Useful arguments include `abbrev` (logical or integer)
#'         to abbreviate factor names and `sep` to change the separator in bracket notation.
#'
#' @return A data frame (also of class `anova`) with columns
#' `       c("AIC", "BIC", "LR Chisq", "Df", "Pr(>Chisq)")`. Row names are taken
#'         from the names of the model object(s) or their model formulas.
#'
#' @author Achim Zeileis, Michael Friendly
#'
#' @seealso \code{\link[stats]{logLik}}, \code{\link[stats]{glm}},
#' \code{\link[MASS]{loglm}},
#'
#' \code{\link{logLik.loglm}}, \code{\link{modFit}},
#' \code{\link{get_models}}
#'
#' @family glmlist functions
#' @keywords models
#' @examples
#'
#' data(Mental)
#' indep <- glm(Freq ~ mental+ses,
#'                 family = poisson, data = Mental)
#' LRstats(indep)
#' Cscore <- as.numeric(Mental$ses)
#' Rscore <- as.numeric(Mental$mental)
#'
#' coleff <- glm(Freq ~ mental + ses + Rscore:ses,
#'                 family = poisson, data = Mental)
#' roweff <- glm(Freq ~ mental + ses + mental:Cscore,
#'                 family = poisson, data = Mental)
#' linlin <- glm(Freq ~ mental + ses + Rscore:Cscore,
#'                 family = poisson, data = Mental)
#'
#' # compare models using object names (default)
#' LRstats(indep, coleff, roweff, linlin)
#'
#' # compare models in a glmlist, using formula labels
#' mods <- glmlist(indep, coleff, roweff, linlin)
#' LRstats(mods, label = "formula")
#'
#' # loglmlist example with bracket notation labels
#' data(Titanic)
#' tit.joint <- seq_loglm(Titanic, type = "joint")
#' LRstats(tit.joint)
#' LRstats(tit.joint, label = "formula")
#' LRstats(tit.joint, label = "formula", label.args = list(abbrev = TRUE))
#'
#' @export
LRstats <- function(object, ...) {
	UseMethod("LRstats")
}

#' @rdname LRstats
#' @export
LRstats.glmlist <- function(object, ..., saturated = NULL, sortby = NULL,
                            label = c("name", "formula"), label.args = list()) {
    label <- match.arg(label)
    ns <- sapply(object, function(x) length(x$residuals))
    if (any(ns != ns[1L]))
        stop("models were not all fitted to the same size of dataset")
    nmodels <- length(object)
    if (nmodels == 1)
        return(LRstats.default(object[[1L]], saturated = saturated))

    rval <- lapply(object, LRstats.default, saturated = saturated)
    rval <- do.call(rbind, rval)

    if (label == "formula") {
        rownames(rval) <- do.call(get_models, c(list(x = object), label.args))
    }

    if (!is.null(sortby)) {
        rval <- rval[order(rval[, sortby], decreasing = TRUE), ]
    }
    rval
}

# could just do LRstats.loglmlist <- LRstats.glmlist

#' @rdname LRstats
#' @export
LRstats.loglmlist <- function(object, ..., saturated = NULL, sortby = NULL,
                              label = c("name", "formula"), label.args = list()) {
    label <- match.arg(label)
    ns <- sapply(object, function(x) length(x$residuals))
    if (any(ns != ns[1L]))
        stop("models were not all fitted to the same size of dataset")
    nmodels <- length(object)
    if (nmodels == 1)
        return(LRstats.default(object[[1L]], saturated = saturated))

    rval <- lapply(object, LRstats.default, saturated = saturated)
    rval <- do.call(rbind, rval)

    if (label == "formula") {
        rownames(rval) <- do.call(get_models, c(list(x = object), label.args))
    }

    if (!is.null(sortby)) {
        rval <- rval[order(rval[, sortby], decreasing = TRUE), ]
    }
    rval
}

#' @rdname LRstats
#' @export
LRstats.default <- function(object, ..., saturated = NULL, sortby=NULL)
{
  ## interface methods for logLik() and nobs()
  ## - use S4 methods if loaded
  ## - use residuals() if nobs() is not available
  logLik0 <- if("stats4" %in% loadedNamespaces()) stats4::logLik else logLik
  nobs0   <- function(x, ...) {
    nobs1 <- if("stats4" %in% loadedNamespaces()) stats4::nobs else nobs
    nobs2 <- function(x, ...) NROW(residuals(x, ...))
    rval <- try(nobs1(x, ...), silent = TRUE)
    if(inherits(rval, "try-error") | is.null(rval)) rval <- nobs2(x, ...)
    return(rval)
  }
  dof <- function(x) {
  	if (inherits(x, "loglm")) {
  		rval <- x$df
  		} else {
  		rval <- try(x$df.residual, silent=TRUE)
  		}
  	if (inherits(rval, "try-error") || is.null(rval)) stop(paste("Can't determine residual df for a", class(x), "object"))
  	rval
  	}

  ## collect all objects
  objects <- list(object, ...)
  nmodels <- length(objects)

  ## check sample sizes
  ns <- sapply(objects, nobs0)
  if(any(ns != ns[1L])) stop("models were not all fitted to the same size of dataset")

  ## extract log-likelihood and df (number of parameters)
  ll <- lapply(objects, logLik0)
  par <- as.numeric(sapply(ll, function(x) attr(x, "df")))
	df <- as.numeric(sapply(objects, function(x) dof(x)))
  ll <- sapply(ll, as.numeric)

  ## compute saturated reference value (use 0 if deviance is not available)
  if(is.null(saturated)) {
    dev <- try(sapply(objects, deviance), silent = TRUE)
    if(inherits(dev, "try-error") || any(sapply(dev, is.null))) {
      saturated <- 0
    } else {
      saturated <- ll + dev/2
    }
  }

  ## setup ANOVA-style matrix
  rval <- matrix(rep(NA, 5 * nmodels), ncol = 5)
  colnames(rval) <- c("AIC", "BIC", "LR Chisq", "Df", "Pr(>Chisq)")
  rownames(rval) <- as.character(sapply(match.call(), deparse)[-1L])[1:nmodels]
  rval[,1] <- -2 * ll + 2 * par
  rval[,2] <- -2 * ll + log(ns) * par
  rval[,3] <- -2 * (ll - saturated)
  rval[,4] <- df
  rval[,5] <- pchisq(rval[,3], df, lower.tail = FALSE)

	if (!is.null(sortby)) {
		rval <- rval[order(rval[,sortby], decreasing=TRUE),]
	}

  ## return
  structure(as.data.frame(rval), heading = "Likelihood summary table:", class = c("anova", "data.frame"))
}
