
#' Mosaic Displays for glmlist and loglmlist Objects
#'
#' This function provides a convenient interface for viewing mosaic displays
#' associated with a collection of glm models for frequency tables that have
#' been stored in a `glmlist` or `loglmlist` object.  You can plot
#' either selected models individually, or mosaics for all models in an array
#' of viewports.
#'
#' Most details of the plots produced can be controlled via \dots{} arguments
#' as shown in some of the examples below.  In particular, with
#' `panel=sieve` you need to also pass `gp=shading_Friendly` to get a
#' color version.
#'
#' @aliases mosaic.glmlist mosaic.loglmlist
#' @param x         a `glmlist` or `loglmlist` object
#' @param selection the index or name of one `glm` or `loglm` object
#'        in `x`.  If no selection is specified, a menu of models is presented or
#'        all models are plotted.
#' @param panel     a \code{\link[vcd]{strucplot}} panel function, typically
#'        \code{\link[vcd]{mosaic}} or \code{\link[vcd]{sieve}}
#' @param type      a character string indicating whether the `"observed"` or
#'        the `"expected"` values of the table should be visualized
#' @param legend    logical: show a legend for residuals in the mosaic display(s)?
#'        The default behavior is to include a legend when only a single plot is
#'        shown, i.e., if `ask` is `TRUE` or a `selection` has been
#'        specified.
#' @param main either a logical, or a vector of character strings used for
#'        plotting the main title.  If main is a logical and `TRUE`, the name of
#'        the selected glm object is used.
#' @param ask logical: should the function display a menu of models, when one
#'        is not specified in `selection`? If `selection` is not supplied
#'        and `ask` is `TRUE` (the default), a menu of model names is
#'        presented; if `ask` is `FALSE`, mosaics for all models are plotted
#'        in an array.
#' @param graphics logical: use a graphic dialog box when `ask=TRUE`?
#' @param rows,cols when `ask=FALSE`, the number of rows and columns in
#'        which to plot the mosaics.
#' @param newpage start a new page? (only applies to `ask=FALSE`)
#' @param \dots other arguments passed to \code{\link{mosaic.glm}} and
#'        ultimately to \code{\link[vcd]{mosaic}}.
#' @return Returns the result of \code{\link{mosaic.glm}}.
#'
#' @author Michael Friendly
#' @importFrom grid viewport pushViewport
#' @seealso \code{\link{glmlist}}, \code{\link{loglmlist}}, \code{\link{Kway}}
#'
#' \code{\link{mosaic.glm}}, \code{\link[vcd]{mosaic}},
#' \code{\link[vcd]{strucplot}}, for the many parameters that control the
#' details of mosaic plots.
#' @family mosaic plots
#' @family glmlist functions
#'
#' @references
#' David Meyer, Achim Zeileis, and Kurt Hornik (2006). The
#' Strucplot Framework: Visualizing Multi-Way Contingency Tables with vcd.
#' *Journal of Statistical Software*, 17(3), 1-48.
#' <https://www.jstatsoft.org/v17/i03/>,
#' available as `vignette("strucplot", package="vcd")`.
#'
#' @keywords hplot
#' @examples
#'
#' data(JobSatisfaction, package="vcd")
#'
#' # view all pairwise mosaics
#' pairs(xtabs(Freq~management+supervisor+own, data=JobSatisfaction),
#'     shade=TRUE, diag_panel=pairs_diagonal_mosaic)
#'
#' modSat <- Kway(Freq ~ management+supervisor+own, data=JobSatisfaction,
#'                family=poisson, prefix="JobSat")
#' names(modSat)
#'
#' \dontrun{
#' mosaic(modSat)              # uses menu, if interactive()
#' }
#' mosaic(modSat, "JobSat.1")  # model label
#' mosaic(modSat, 2)           # model index
#'
#' # supply a formula to determine the order of variables in the mosaic
#' mosaic(modSat, 2, formula=~own+supervisor+management)
#'
#' mosaic(modSat, ask=FALSE)   # uses viewports
#'
#' # use a different panel function, label the observed valued in the cells
#' mosaic(modSat, 1, main=TRUE, panel=sieve, gp=shading_Friendly, labeling=labeling_values)
#'
#' data(Mental)
#' indep <- glm(Freq ~ mental+ses,
#'                 family = poisson, data = Mental)
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
#' # assign names for the plot labels
#' modMental <- glmlist(Indep=indep, ColEff=coleff, RowEff=roweff, `Lin x Lin`=linlin)
#' mosaic(modMental, ask=FALSE, margins=c(3,1,1,2), labeling_args=list(abbreviate_labs=5))
#'
#'
#'
#' @importFrom grid grid.newpage grid.layout pushViewport popViewport
#' @export
mosaic.glmlist <- function(x, selection,
		panel=mosaic,
		type=c("observed", "expected"),
		legend=ask | !missing(selection),
		main=NULL,
		ask=TRUE, graphics=TRUE, rows, cols, newpage=TRUE,
		...) {

#	calls <- sapply(x, mod.call)  # get model calls as strings
	models <- names(x)
	if (!is.null(main)) {
		if (is.logical(main) && main)
			main <- models
	}
	else main <- rep(main, length(x))

	type=match.arg(type)
	if (!missing(selection)){
		if (is.character(selection)) selection <- gsub(" ", "", selection)
		return(panel(x[[selection]], type=type, main=main[selection], legend=legend, ...))
	}
	# perhaps make these model labels more explicit for the menu
	if (ask & interactive()){
		repeat {
			selection <- menu(models, graphics=graphics, title="Select Model to Plot")
			if (selection == 0) break
			else panel(x[[selection]], type=type, main=main[selection], legend=legend, ...)
		}
	}
	else {
		nmodels <- length(x)
		mfrow <- mfrow(nmodels)
		if (missing(rows) || missing(cols)){
			rows <- mfrow[1]
			cols <- mfrow[2]
		}

		if (newpage) grid.newpage()
		lay <- grid.layout(nrow=rows, ncol = cols)
		pushViewport(viewport(layout = lay, y = 0, just = "bottom"))
		for (i in 1:rows) {
			for (j in 1:cols){
				if ((sel <-(i-1)*cols + j) > nmodels) break
				pushViewport(viewport(layout.pos.row=i, layout.pos.col=j))
				panel(x[[sel]], type=type, main=main[sel], newpage=FALSE, legend=legend, ...)
				popViewport()
			}
		}
	}
}

#' @rdname mosaic.glmlist
#' @export
mosaic.loglmlist <- function(x, selection,
		panel=mosaic,
		type=c("observed", "expected"),
		legend=ask | !missing(selection),
		main=NULL,
		ask=TRUE, graphics=TRUE, rows, cols, newpage=TRUE,
		...) {

	models <- names(x)
	strings <- as.vector(sapply(x, function(x) x$model.string))
	if (!is.null(main)) {
		if (is.logical(main) && main)
			main <- ifelse(as.vector(sapply(strings, is.null)), models, strings)
	}
	else main <- rep(main, length(x))

	type=match.arg(type)
	if (!missing(selection)){
		if (is.character(selection)) selection <- gsub(" ", "", selection)
		return(panel(x[[selection]], type=type, main=main[selection], legend=legend, ...))
	}
	# perhaps make these model labels more explicit for the menu
	if (ask & interactive()){
		repeat {
			selection <- menu(models, graphics=graphics, title="Select Model to Plot")
			if (selection == 0) break
			else panel(x[[selection]], type=type, main=main[selection], legend=legend, ...)
		}
	}
	else {
		nmodels <- length(x)
		mfrow <- mfrow(nmodels)
		if (missing(rows) || missing(cols)){
			rows <- mfrow[1]
			cols <- mfrow[2]
		}

		if (newpage) grid.newpage()
		lay <- grid.layout(nrow=rows, ncol = cols)
		pushViewport(viewport(layout = lay, y = 0, just = "bottom"))
		for (i in 1:rows) {
			for (j in 1:cols){
				if ((sel <-(i-1)*cols + j) > nmodels) break
				pushViewport(viewport(layout.pos.row=i, layout.pos.col=j))
				panel(x[[sel]], type=type, main=main[sel], newpage=FALSE, legend=legend, ...)
				popViewport()
			}
		}
	}
}


# from effects::utilities.R
mfrow <- function(n, max.plots=0){
	# number of rows and columns for array of n plots
	if (max.plots != 0 & n > max.plots)
		stop(paste("number of plots =",n," exceeds maximum =", max.plots))
	rows <- round(sqrt(n))
	cols <- ceiling(n/rows)
	c(rows, cols)
}

# from plot.lm: get model call as a string
# TODO: should use abbreviate()
mod.call <- function(x) {
        cal <- x$call
        if (!is.na(m.f <- match("formula", names(cal)))) {
            cal <- cal[c(1, m.f)]
            names(cal)[2L] <- ""
        }
        cc <- deparse(cal, 80)
        nc <- nchar(cc[1L], "c")
        abbr <- length(cc) > 1 || nc > 75
        cap <- if (abbr)
            paste(substr(cc[1L], 1L, min(75L, nc)), "...")
        else cc[1L]
		cap
}

