#' Sequential Loglinear Models for an N-way Table
#'
#' This function takes an n-way contingency table and fits a series of
#' sequential models to the 1-, 2-, ... n-way marginal tables, corresponding to
#' a variety of types of loglinear models.
#'
#' Sequential marginal models for an n-way tables begin with the model of
#' equal-probability for the one-way margin (equivalent to a
#' \code{\link[stats]{chisq.test}}) and add successive variables one at a time
#' in the order specified by `vorder`.
#'
#' All model types give the same result for the two-way margin, namely the test
#' of independence for the first two factors.
#'
#' Sequential models of *joint independence* (`type="joint"`) have a
#' particularly simple interpretation, because they decompose the likelihood
#' ratio test for the model of mutual independence in the full n-way table, and
#' hence account for "total" association in terms of portions attributable to
#' the conditional probabilities of each new variable, given all prior
#' variables.
#'
#' @param x a contingency table in array form, with optional category labels specified in the dimnames(x) attribute,
#'          or else a data.frame in frequency form, with the frequency variable named `"Freq"`.
#' @param type type of sequential model to fit, a character string. One of `"joint"`, `"conditional"`,
#'          `"mutual"`, `"markov"`, or `"saturated"`.
#' @param marginals which marginal sub-tables to fit? A vector of a (sub)set of the integers, `1:nf` where
#'          `nf` is the number of factors in the full n-way table.
#' @param vorder order of variables, a permutation of the integers `1:nf`, used to reorder the variables in
#'          the original table for the purpose of fitting sequential marginal models.
#' @param k conditioning variable(s) for `type` = `"joint"`, `"conditional"` or Markov chain order
#'          for `type` = `"markov"`
#' @param prefix prefix used to give names to the sequential models
#' @param fitted argument passed to `loglm` to store the fitted values in the model objects
#' @param \dots other arguments, passed down
#'
#' @return An object of class `"loglmlist"`, each of which is a class `"loglm"` object
#'
#' @note One-way marginal tables are a bit of a problem here, because they
#' cannot be fit directly using \code{\link[MASS]{loglm}}. The present version
#' uses \code{\link[stats]{loglin}}, and repairs the result to look like a
#' `loglm` object (sort of).
#'
#' @author Michael Friendly
#'
#' @seealso \code{\link{loglin-utilities}} for descriptions of sequential
#' models, \code{\link{conditional}}, \code{\link{joint}},
#' \code{\link{mutual}}, \dots{}
#'
#' \code{\link{loglmlist}}
#' @family loglinear models
#'
#' @references These functions were inspired by the original SAS implementation
#' of mosaic displays, described in the *User's Guide*,
#' <http://www.datavis.ca/mosaics/mosaics.pdf>
#'
#' @keywords models
#' @examples
#'
#' data(Titanic, package="datasets")
#' # variables are in the order Class, Sex, Age, Survived
#' tt <- seq_loglm(Titanic)
#'
#'
#'
#' @export seq_loglm
seq_loglm <- function(
	x,
	type = c("joint", "conditional", "mutual", "markov", "saturated"),
	marginals = 1:nf,  # which marginals to fit?
	vorder = 1:nf,     # order of variables in the sequential models
	k = NULL,          # conditioning variable(s) for "joint", "conditional" or order for "markov"
	prefix = 'model',
	fitted = TRUE,     # keep fitted values?
	...
	)
{
  if (inherits(x, "data.frame") && "Freq" %in% colnames(x)) {
    x <- xtabs(Freq ~ ., data=x)
  }
  if (!inherits(x, c("table", "array"))) stop("not an xtabs, table, array or data.frame with a 'Freq' variable")

	nf <- length(dim(x))
	x <- aperm(x, vorder)
	factors <- names(dimnames(x))
	indices <- 1:nf

  type = match.arg(type)
#  models <- as.list(rep(NULL, length(marginals)))
  models <- list()
  for (i in marginals) {
		mtab <- margin.table(x, 1:i)
		if (i==1) {
			# KLUDGE: use loglin, but try to make it look like a loglm object
			mod <- loglin(mtab, margin=NULL, print=FALSE)
		  mod$model.string = paste("=", factors[1])
		  mod$margin <- list(factors[1])
#		  mod$margin <- names(dimnames(mtab))
#		  names(mod$margin) <- factors[1]
      if (fitted) {
        fit <- mtab
  		  fit[] <- (sum(mtab) / length(mtab))
  		  mod$fitted <- fit
		  }
		  mod$nobs <- length(mtab)
		  mod$frequencies <- mtab
		  mod$deviance <- mod$lrt
		  class(mod) <- c("loglin", "loglm")
		  }
		else {
  		expected <- switch(type,
  			'conditional' = conditional(i, mtab, with=if(is.null(k)) i else k),
  			'joint' = joint(i, mtab, with=if(is.null(k)) i else k),
  			'mutual' = mutual(i, mtab),
  			'markov' = markov(i, mtab, order=if(is.null(k)) 1 else k),
  			'saturated' = saturated(i, mtab)
  			)

  		form <- loglin2formula(expected)
#  		mod <- loglm(formula=form, data=mtab, fitted=TRUE)
      mod <- eval(bquote(MASS::loglm(.(form), data=mtab, fitted=fitted)))
  		mod$model.string <- loglin2string(expected, brackets=if (i<nf) '()' else '[]')

		}
#  	cat(i, "  model.string: ", mod$model.string, "\n")
#  	cat("model:\n"); print(mod)
  	models[[i]] <- mod
	}
	names(models) <- paste(prefix, '.', indices, sep='')
	models <- models[marginals]
	class(models) <- "loglmlist"
	invisible(models)
}

