
#' Fit All K-way Models in a GLM
#'
#' Generate and fit all 0-way, 1-way, 2-way, ... k-way terms in a glm.
#'
#' This function is designed mainly for hierarchical loglinear models (or
#' `glm`s in the poisson family), where it is desired to find the
#' highest-order terms necessary to achieve a satisfactory fit.
#'
#' Using \code{\link[stats]{anova}} on the resulting \code{\link{glmlist}}
#' object will then give sequential tests of the pooled contributions of all
#' terms of degree \eqn{k+1} over and above those of degree \eqn{k}.
#'
#' This function is also intended as an example of a generating function for
#' \code{\link{glmlist}} objects, to facilitate model comparison, extraction,
#' summary and plotting of model components, etc., perhaps using `lapply`
#' or similar.
#'
#' With `y` as the response in the `formula`, the 0-way (null) model
#' is `y ~ 1`.  The 1-way ("main effects") model is that specified in the
#' `formula` argument.  The k-way model is generated using the formula
#' `. ~ .^k`. With the default `order = nt`, the final model is the
#' saturated model.
#'
#' As presently written, the function requires a two-sided formula with an
#' explicit response on the LHS. For frequency data in table form (e.g.,
#' produced by `xtabs`) you the `data` argument is coerced to a
#' data.frame, so you should supply the `formula` in the form `Freq ~
#' ` \dots{}.
#'
#' @param formula a two-sided formula for the 1-way effects in the model. The LHS should be the response, and the RHS
#'        should be the first-order terms connected by `+` signs.
#' @param family a description of the error distribution and link function to be used in the model.  This can be a character
#'        string naming a family function, a family function or the result of a call to a family function.
#'        (See \code{\link[stats]{family}} for details of family functions.)
#' @param data an optional data frame, list or environment (or object coercible
#' by \code{\link[base]{as.data.frame}} to a data frame) containing the
#' variables in the model. If not found in data, the variables are taken from
#' `environment(formula)`, typically the environment from which `glm`
#' is called.
#' @param \dots Other arguments passed to `glm`
#' @param order Highest order interaction of the models generated. Defaults to
#' the number of terms in the model formula.
#' @param prefix Prefix used to label the models fit in the `glmlist`
#' object.
#' @return An object of class `glmlist`, of length `order+1`
#' containing the 0-way, 1-way, ...  models up to degree `order`.
#' @author Michael Friendly and Heather Turner
#'
#' @seealso \code{\link{glmlist}}, \code{\link{Summarise}} (soon to be
#' deprecated), \code{\link{LRstats}}
#'
#' @family glmlist functions
#' @keywords models
#' @examples
#'
#' ## artificial data
#' factors <- expand.grid(A=factor(1:3),
#'                        B=factor(1:2),
#'                        C=factor(1:3),
#'                        D=factor(1:2))
#' Freq <- rpois(nrow(factors), lambda=40)
#' df <- cbind(factors, Freq)
#'
#' mods3 <- Kway(Freq ~ A + B + C, data=df, family=poisson)
#' LRstats(mods3)
#' mods4 <- Kway(Freq ~ A + B + C + D, data=df, family=poisson)
#' LRstats(mods4)
#'
#' # JobSatisfaction data
#' data(JobSatisfaction, package="vcd")
#' modSat <- Kway(Freq ~ management+supervisor+own,
#'                data=JobSatisfaction,
#'                family=poisson, prefix="JobSat")
#' LRstats(modSat)
#' anova(modSat, test="Chisq")
#'
#' # Rochdale data: very sparse, in table form
#' data(Rochdale, package="vcd")
#' \dontrun{
#' modRoch <- Kway(Freq~EconActive + Age + HusbandEmployed + Child +
#'                      Education + HusbandEducation + Asian + HouseholdWorking,
#'                 data=Rochdale, family=poisson)
#' LRstats(modRoch)
#' }
#'
#' @export Kway
Kway <- function(formula, family=poisson, data, ..., order=nt, prefix="kway") {

   if (is.character(family))
       family <- get(family, mode = "function", envir = parent.frame())
   if (is.function(family))
       family <- family()
   if (is.null(family$family)) {
       print(family)
       stop("'family' not recognized")
   }
   if (missing(data))
        data <- environment(formula)

   models <- list()
   mod <- glm(formula, family=family, data, ...)
   mod$call$formula <- formula
   terms <- terms(formula)
   tl <- attr(terms, "term.labels")
   nt <- length(tl)
   models[[1]] <- mod
   for(i in 2:order) {
       models[[i]] <- update(mod, substitute(.~.^p, list(p = i)))
   }      # null model
   mod0 <- update(mod, .~1)
   models <- c(list(mod0), models)
   names(models) <- paste(prefix, 0:order, sep = ".")
   class(models) <- "glmlist"
   models
}

