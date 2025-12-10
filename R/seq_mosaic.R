#' Sequential Mosaics and Strucplots for an N-way Table
#'
#' This function takes an n-way contingency table and plots mosaics for series
#' of sequential models to the 1-, 2-, ... n-way marginal tables, corresponding
#' to a variety of types of loglinear models.
#'
#' This function produces similar plots to the use of
#' \code{\link{mosaic.loglmlist}}, called with the result of
#' \code{\link{seq_loglm}}.
#'
#' @param x a contingency table in array form, with optional category labels
#'        specified in the `dimnames(x)` attribute, or else a data.frame in frequency
#'        form, with the frequency variable named `"Freq"`.
#' @param panel a \code{\link[vcd]{strucplot}} panel function, typically
#'        \code{\link[vcd]{mosaic}} or \code{\link[vcd]{sieve}. NOT yet implemented.}
#' @param type type of sequential model to fit, a character string. One of
#'        `"joint"`, `"conditional"`, `"mutual"`, `"markov"`, or
#'         `"saturated"`.
#' @param plots which marginal sub-tables to plot? A vector of a (sub)set of
#'        the integers, `1:nf` where `nf` is the number of factors in the
#'        full n-way table.
#' @param vorder order of variables, a permutation of the integers `1:nf`,
#'        used to reorder the variables in the original table for the purpose of
#'        fitting sequential marginal models.
#' @param k conditioning variable(s) for `type` = `"joint"`,
#'        `"conditional"` or Markov chain order for `type` = `"markov"`
#' @param \dots other arguments passed to \code{\link[vcd]{mosaic}}.
#'
#' @return None. Used for its side-effect of producing plots
#'
#' @author Michael Friendly
#' @seealso \code{\link{loglin-utilities}} for descriptions of sequential
#'         models, \code{\link{conditional}}, \code{\link{joint}}, \code{\link{mutual}}, \dots{}
#'         \code{\link{loglmlist}}, \code{\link{mosaic.loglmlist}},
#'         \code{\link{seq_loglm}}
#'
#'         \code{\link{mosaic.glm}}, \code{\link[vcd]{mosaic}},
#'         \code{\link[vcd]{strucplot}}, for the many parameters that control the details of mosaic plots.
#' @references
#' These functions were inspired by the original SAS implementation
#' of mosaic displays, described in the *User's Guide for Mosaics*,
#' <http://www.datavis.ca/mosaics/mosaics.pdf>
#' @keywords hplots
#' @examples
#'
#'
#' data(Titanic, package="datasets")
#'
#' seq_mosaic(Titanic)  # models of joint independence, Survived last
#' seq_mosaic(Titanic, type="condit")
#' seq_mosaic(Titanic, type="mutual")
#'
#' # other panel functions and options: presently BUGGED
#' \dontrun{
#' seq_mosaic(Titanic, type="mutual", panel=sieve,
#'    gp=shading_Friendly, labeling=labeling_values)
#' }
#'
#'
#' @export seq_mosaic
seq_mosaic <- function(
	x,
	panel = mosaic,
	type = c("joint", "conditional", "mutual", "markov", "saturated"),
	plots = 1:nf,      # which plots to produce?
	vorder = 1:nf,      # order of variables in the sequential plots
	k = NULL,          # conditioning variable(s) for "joint", "conditional" or order for "markov"
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
	for (i in plots) {
		mtab <- margin.table(x, 1:i)
		df <- NULL
		if (i==1) {
		  expected <- mtab
		  expected[] <- sum(mtab) / length(mtab)
		  df <- length(mtab)-1
		  model.string = paste("=", factors[1])
		  }
		else {
  		expected <- switch(type,
  			'conditional' = conditional(i, mtab, with=if(is.null(k)) i else k),
  			'joint' = joint(i, mtab, with=if(is.null(k)) i else k),
  			'mutual' = mutual(i, mtab),
  			'markov' = markov(i, mtab, order=if(is.null(k)) 1 else k),
  			'saturated' = saturated(i, mtab)
  			)
    model.string <- loglin2string(expected, brackets=if (i<nf) '()' else '[]')
  	}
		panel(mtab, expected=expected, df=df, main=model.string, ...)
	}
}

