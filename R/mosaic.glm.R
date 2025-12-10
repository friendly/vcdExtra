# mosaic plot for a glm object
#  Allow it to use residuals of any type computed by residuals()
#  or to pass residuals calculated by another function, e.g., rstandard(), rstudent()
#
#  Allow to apply to any model with discrete factors
# last modified: 3/6/2009 1:51PM
#  - fixed buggy version using ideas from vcd:::plot.loglm
#  - now use $data component when it is a table

## TODO: move to utility.R
#is.discrete.model <- function(model)
# 	all(attr(terms(model), "dataClasses")[-1] %in% c("factor", "ordered"))





#' Mosaic plots for fitted generalized linear and generalized nonlinear models
#'
#' Produces mosaic plots (and other plots in the \code{\link[vcd]{strucplot}}
#' framework) for a log-linear model fitted with \code{\link[stats]{glm}} or
#' for a generalized nonlinear model fitted with \code{\link[gnm]{gnm}}.
#'
#' These methods extend the range of strucplot visualizations well beyond the
#' models that can be fit with \code{\link[MASS]{loglm}}. They are intended for
#' models for counts using the Poisson family (or quasi-poisson), but should be
#' sensible as long as (a) the response variable is non-negative and (b) the
#' predictors visualized in the \code{strucplot} are discrete factors.
#'
#' For both poisson family generalized linear models and loglinear models,
#' standardized residuals provided by \code{rstandard} (sometimes called
#' adjusted residuals) are often preferred because they have constant unit
#' asymptotic variance.
#'
#' The \code{sieve} and \code{assoc} methods are simple convenience interfaces
#' to this plot method, setting the panel argument accordingly.
#'
#' @aliases mosaic.glm sieve.glm assoc.glm
#' @param x A \code{glm} or \code{gnm} object. The response variable, typically
#' a cell frequency, should be non-negative.
#' @param formula A one-sided formula with the indexing factors of the plot
#' separated by '+', determining the order in which the variables are used in
#' the mosaic.  A formula must be provided unless \code{x$data} inherits from
#' class \code{"table"} -- in which case the indexing factors of this table are
#' used, or the factors in \code{x$data} (or model.frame(x) if \code{x$data} is
#' an environment) exactly cross-classify the data -- in which case this set of
#' cross-classifying factors are used.
#' @param panel Panel function used to draw the plot for visualizing the
#' observed values, residuals and expected values. Currently, one of
#' \code{"mosaic"}, \code{"assoc"}, or \code{"sieve"} in \code{vcd}.
#' @param type A character string indicating whether the \code{"observed"} or
#' the \code{"expected"} values of the table should be visualized by the area
#' of the tiles or bars.
#' @param residuals An optional array or vector of residuals corresponding to
#' the cells in the data, for example, as calculated by
#' \code{residuals.glm(x)}, \code{residuals.gnm(x)}.
#' @param residuals_type If the \code{residuals} argument is \code{NULL},
#' residuals are calculated internally and used in the display.  In this case,
#' \code{residual_type} can be \code{"pearson"}, \code{"deviance"} or
#' \code{"rstandard"}.  Otherwise (when \code{residuals} is supplied),
#' \code{residuals_type} is used as a label for the legend in the plot.
#' @param gp Object of class \code{"gpar"}, shading function or a corresponding
#' generating function (see \code{\link[vcd]{strucplot}} Details and
#' \code{\link[vcd]{shadings}}).  Ignored if shade = FALSE.
#' @param gp_args A list of arguments for the shading-generating function, if
#' specified.
#' @param \dots Other arguments passed to the \code{panel} function e.g.,
#' \code{\link[vcd]{mosaic}}
#'
#' @return The \code{structable} visualized by \code{\link[vcd]{strucplot}} is
#' returned invisibly.
#'
#' @author Heather Turner, Michael Friendly, with help from Achim Zeileis
#'
#' @seealso \code{\link[stats]{glm}}, \code{\link[gnm]{gnm}},
#' \code{\link[vcd]{plot.loglm}}, \code{\link[vcd]{mosaic}}
#' @family mosaic plots
#'
#' @keywords hplot models multivariate
#' @importFrom vcd mosaic
#' @importFrom vcd shading_hcl
#' @importFrom gnm meanResiduals
#' @examples
#' library(vcdExtra)
#'
#' GSStab <- xtabs(count ~ sex + party, data=GSS)
#' # using the data in table form
#' mod.glm1 <- glm(Freq ~ sex + party, family = poisson, data = GSStab)
#' res <- residuals(mod.glm1)
#' std <- rstandard(mod.glm1)
#'
#' # For mosaic.default(), need to re-shape residuals to conform to data
#' stdtab <- array(std,
#'                 dim=dim(GSStab),
#'                 dimnames=dimnames(GSStab))
#'
#' mosaic(GSStab,
#'        gp=shading_Friendly,
#'        residuals=stdtab,
#'        residuals_type="Std\nresiduals",
#'        labeling = labeling_residuals)
#'
#'
#' # Using externally calculated residuals with the glm() object
#' mosaic(mod.glm1,
#'        residuals=std,
#'        labeling = labeling_residuals,
#'        shade=TRUE)
#'
#' # Using residuals_type
#' mosaic(mod.glm1,
#'        residuals_type="rstandard",
#'        labeling = labeling_residuals, shade=TRUE)
#'
#' ## Ordinal factors and structured associations
#' data(Mental)
#' xtabs(Freq ~ mental+ses, data=Mental)
#' long.labels <- list(set_varnames = c(mental="Mental Health Status",
#'                                      ses="Parent SES"))
#'
#' # fit independence model
#' # Residual deviance: 47.418 on 15 degrees of freedom
#' indep <- glm(Freq ~ mental+ses,
#'              family = poisson, data = Mental)
#'
#' long.labels <- list(set_varnames = c(mental="Mental Health Status",
#'                                      ses="Parent SES"))
#' mosaic(indep,
#'        residuals_type="rstandard",
#'        labeling_args = long.labels,
#'        labeling=labeling_residuals)
#'
#' # or, show as a sieve diagram
#' mosaic(indep,
#'        labeling_args = long.labels,
#'        panel=sieve,
#'        gp=shading_Friendly)
#'
#' # fit linear x linear (uniform) association.  Use integer scores for rows/cols
#' Cscore <- as.numeric(Mental$ses)
#' Rscore <- as.numeric(Mental$mental)
#'
#' linlin <- glm(Freq ~ mental + ses + Rscore:Cscore,
#'                 family = poisson, data = Mental)
#'
#' mosaic(linlin,
#'        residuals_type="rstandard",
#'        labeling_args = long.labels,
#'        labeling=labeling_residuals,
#'        suppress=1,
#'        gp=shading_Friendly,
#'        main="Lin x Lin model")
#'
#' ##  Goodman Row-Column association model fits even better (deviance 3.57, df 8)
#' if (require(gnm)) {
#' Mental$mental <- C(Mental$mental, treatment)
#' Mental$ses <- C(Mental$ses, treatment)
#' RC1model <- gnm(Freq ~ ses + mental + Mult(ses, mental),
#'                 family = poisson, data = Mental)
#'
#' mosaic(RC1model,
#'        residuals_type="rstandard",
#'        labeling_args = long.labels,
#'        labeling=labeling_residuals,
#'        suppress=1,
#'        gp=shading_Friendly,
#'        main="RC1 model")
#'  }
#'
#'  ############# UCB Admissions data, fit using glm()
#'
#' structable(Dept ~ Admit+Gender,UCBAdmissions)
#'
#' berkeley <- as.data.frame(UCBAdmissions)
#' berk.glm1 <- glm(Freq ~ Dept * (Gender+Admit), data=berkeley, family="poisson")
#' summary(berk.glm1)
#'
#' mosaic(berk.glm1,
#'        gp=shading_Friendly,
#'        labeling=labeling_residuals,
#'        formula=~Admit+Dept+Gender)
#'
#' # the same, displaying studentized residuals;
#' # note use of formula to reorder factors in the mosaic
#' mosaic(berk.glm1,
#'        residuals_type="rstandard",
#'        labeling=labeling_residuals,
#'        shade=TRUE,
#' 	     formula=~Admit+Dept+Gender,
#' 	     main="Model: [DeptGender][DeptAdmit]")
#'
#' ## all two-way model
#' berk.glm2 <- glm(Freq ~ (Dept + Gender + Admit)^2, data=berkeley, family="poisson")
#' summary(berk.glm2)
#'
#' mosaic(berk.glm2,
#'        residuals_type="rstandard",
#'        labeling = labeling_residuals,
#'        shade=TRUE,
#' 	     formula=~Admit+Dept+Gender,
#' 	     main="Model: [DeptGender][DeptAdmit][AdmitGender]")
#'
#' anova(berk.glm1, berk.glm2, test="Chisq")
#'
#' # Add 1 df term for association of [GenderAdmit] only in Dept A
#' berkeley <- within(berkeley,
#'                    dept1AG <- (Dept=='A')*(Gender=='Female')*(Admit=='Admitted'))
#' berkeley[1:6,]
#'
#' berk.glm3 <- glm(Freq ~ Dept * (Gender+Admit) + dept1AG, data=berkeley, family="poisson")
#' summary(berk.glm3)
#'
#' mosaic(berk.glm3,
#'        residuals_type = "rstandard",
#'        labeling = labeling_residuals,
#'        shade=TRUE,
#' 	      formula = ~Admit+Dept+Gender,
#' 	      main = "Model: [DeptGender][DeptAdmit] + DeptA*[GA]")
#'
#' # compare models
#' anova(berk.glm1, berk.glm3, test="Chisq")
#'
#'
#' @export
mosaic.glm <-	function(x, formula = NULL,
                         panel=mosaic, type=c("observed", "expected"),
                         residuals=NULL,
                         residuals_type = c("pearson", "deviance", "rstandard"),
                         gp = shading_hcl, gp_args = list(), ...)
{

	#require(vcd)
	if (!inherits(x,"glm")) stop("mosaic.glm requires a glm object")

	df.residual <- x$df.residual
	observed <- x$data

        if (is.null(formula) && inherits(observed, "table"))
            formula <- reformulate(names(dimnames(observed)))

        if (is.null(formula)) {
            if (is.environment(observed)) observed <- model.frame(x)
            else {
                if (!is.null(x$call$subset))
                    observed <- subset(observed, eval(x$call$subset, observed))
                if (!is.null(x$na.action))
                    observed <- observed[-x$na.action,]
            }
            ## get all factors excluding response
            factors <- sapply(observed, inherits, "factor")
            resp <- as.character(x$formula[[2]])
            factors <- observed[setdiff(colnames(observed[factors]), resp)]
            ## drop unused levels
            for(nm in names(factors)) {
                f <- factors[[nm]]
                if(is.factor(f) &&
                   length(unique(f[!is.na(f)])) < length(levels(f)))
                    factors[[nm]] <- factors[[nm]][, drop = TRUE]
            }
            ok <- TRUE
            ## check cross-classifying
            if (ok <- isTRUE(all(table(factors) == 1))) {
              warning("no formula provided, assuming ",
                      deparse(formula(terms(~ . , data = factors))),
                      "\n", call. = FALSE)
            }
            if (!ok)
              stop("cannot identify indexing factors from ", substitute(x),
                   "$data - please provide formula", call. = FALSE)
        }
        else {
            if (length(formula) == 3) formula <- formula[-2]
            ## get indexing factors allowing for missing data, subset etc
            factors <- do.call("model.frame", list(formula = formula,
                                                   data = observed,
                                                   subset = x$call$subset,
                                                   na.action = na.pass,
                                                   drop.unused.levels = TRUE))
            ## following loop needed due to bug in model.frame.default (fixed for R 2.12)
            for(nm in names(factors)) {
                f <- factors[[nm]]
                if(is.factor(f) && length(unique(f[!is.na(f)])) < length(levels(f)))
                    factors[[nm]] <- factors[[nm]][, drop = TRUE]
            }
            if (!is.null(x$na.action))
                factors <- factors[-x$na.action,]
        }

        if (x$family$family == "poisson") {
            observed <- as.table(tapply(x$y, factors, sum))
            expected <- as.table(tapply(fitted(x), factors, sum))
        }
        else{
            observed <- as.table(tapply(x$prior.weights, factors, sum))
            expected <- as.table(tapply(x$prior.weights * x$weights, factors, sum))
        }
        ## replace any missing values with zero
        observed[is.na(observed)] <- 0 #else strucplot would do this
        expected[is.na(expected)] <- 0

	type <- match.arg(tolower(type), c("observed", "expected"))
	if (any(observed < 0, na.rm = TRUE))
            stop("requires a non-negative response vector")

        ## reshape the residuals to conform to the structure of data

        ## if max one residual per cell, use residuals_type
        if (max(table(factors)) == 1) {
            residuals_type <- match.arg(tolower(residuals_type),
                                        c("pearson", "deviance", "rstandard"))
            if (missing(residuals))
                residuals <- if (residuals_type=="rstandard") rstandard(x)
                else residuals(x, type=residuals_type)
            residuals <- as.table(tapply(residuals, factors, sum))
            df <- x$df.residual
        }
        ## for marginal views, use aggregated working residuals
        else {
            residuals <- meanResiduals(x, factors)
            residuals_type <- "working" #what is this used for?
            df <- attr(residuals, "df")
            if (df == 0) {
                warning("There are zero degrees of freedom ",
                        "for the test of normality")
                df <- NA
            }
        }
        ## replace any missing values with zero
        residuals[is.na(residuals)] <- 0

	gp <- if (inherits(gp, "grapcon_generator"))
            do.call("gp", c(list(observed, residuals, expected, df),
                            as.list(gp_args)))
        else gp

	panel(observed, residuals=residuals, expected=expected, type=type,
              residuals_type=residuals_type, gp=gp, ...)
}

## convenience functions for sieve and assoc plots

#' @rdname mosaic.glm
#' @importFrom vcd sieve
#' @export
sieve.glm <-
		function (x, ...)
{
	mosaic(x, panel = sieve, ...)
}

#' @rdname mosaic.glm
#' @importFrom vcd assoc
#' @export
assoc.glm <-
		function (x, ...)
{
	mosaic(x, panel = assoc, ...)
}

