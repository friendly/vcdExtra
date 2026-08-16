# Term-level likelihood-ratio tests for MASS::loglm models -- second draft.
#
# This file lives under dev/, alongside the original prototype in
# anova-like.R (kept for reference/history) and the design notes in
# README.md / anova-like.md.
#
# drop1.loglm() is the primary interface (per MF, README.md). For each term
# in the model's generating class (the highest-order terms in $margin --
# "Just the generating class", README.md), it drops that term, refits, and
# reports the change in both the LR statistic (G^2) and the Pearson
# statistic (X^2), together with the LR-based p-value.
#
# LRanova() is a thin wrapper around drop1.loglm() that adds a partial R^2
# effect-size column relative to a baseline model (default: mutual
# independence over the variables in the generating class), after checking
# that the baseline is nested within the fitted model.
#
# --- What changed from anova-like.R, and why -------------------------------
#
# The first draft rebuilt each reduced-model formula by hand from $margin
# via .margin_formula()/.drop_loglm_margin(). That discards the response
# variable, which is why LRanova() failed on DaytonSurvey with "formula
# specifies no response": DaytonSurvey is fit as `Freq ~ ...` (data.frame
# form), and the hand-built formula `~ A:B + ...` has no LHS. The
# array/table examples (UCBAdmissions, HairEyeColor) happened to work
# because those formulas have no response to lose in the first place.
#
# The fix is to not rebuild formulas at all. MASS already supplies
# update.loglm(), extractAIC.loglm(), and a working terms()/formula() for
# "loglm" objects -- which means stats::drop1() and MASS::dropterm() already
# dispatch to their .default methods and work correctly for loglm objects,
# response variable included (verified against both UCBAdmissions and
# DaytonSurvey; MASS 7.3-66 / R 4.6.1). So drop1.loglm() below refits each
# reduced model via update(object, ~ . - term), exactly as
# drop1.default()/dropterm.default() do, instead of reconstructing formulas.
#
# The one thing base R's drop1()/dropterm() don't give us is the Pearson X^2
# alongside the LR G^2 (they only expose a deviance/AIC-derived LRT). loglm
# objects conveniently store both $lrt and $pearson directly on the fitted
# object, so we read those off the full model and each refit rather than
# going through extractAIC().
#
# Reused from the vcdExtra package itself (via devtools::load_all()):
#   - get_model(x, type = "brackets") for the bracket-notation "[A,B] [C,D]"
#     headings, consistent with LRstats()/seq_loglm() output elsewhere in
#     the package, instead of a raw deparse(formula(x)).

devtools::load_all(".", quiet = TRUE)

#' Term-level tests and effect sizes for loglm models
#'
#' `drop1.loglm()` performs single-term deletion tests for the generating
#' class of a fitted [MASS::loglm()] model: for each term in that
#' class, it drops that term, refits via `update()`, and reports the change
#' in both the LR statistic (G^2) and the Pearson statistic (X^2).
#'
#' `LRanova()` wraps `drop1.loglm()` and adds a partial R^2 effect-size
#' column: the fraction of a baseline model's G^2 attributable to each
#' dropped term.
#'
#' `assoc_strength()` wraps `drop1.loglm()` and adds a partial Cramer's V or
#' Cohen's w effect-size column, converting each term's partial Pearson X^2
#' into a bounded or semi-bounded measure of association strength.
#'
#' @details
#' All three functions test terms in the model's *generating class* --
#' the set of highest-order terms in a hierarchical loglinear model, from
#' which every lower-order relative (main effects, lower interactions) is
#' implied and so doesn't need to be listed or tested separately.
#'
#' For example, with four factors `A`, `B`, `C`, `D` and the model
#' `~ (A + B + C + D)^2` (all terms up to two-way), the fitted model
#' contains ten terms in all -- four main effects and six two-way
#' interactions -- but its generating class is just the six two-way terms:
#' `A:B`, `A:C`, `A:D`, `B:C`, `B:D`, `C:D` (bracket notation `[A,B] [A,C]
#' [A,D] [B,C] [B,D] [C,D]`). None of the four main effects is maximal on
#' its own -- `A`, say, is already implied by `A:B` (or any of `A:C`,
#' `A:D`) -- so they're excluded from the generating class even though
#' they're very much still part of the fitted model. This is exactly what
#' `MASS::loglm()` stores in `object$margin`, and exactly the set of terms
#' `scope` defaults to and `drop1.loglm()` tests: dropping `A:B` removes
#' only that interaction, leaving `A` and `B`'s main effects in place
#' (still implied by `A:C`/`A:D` and `B:C`/`B:D` respectively), so the
#' reduced model stays hierarchical.
#'
#' @param object a fitted `MASS::loglm` model
#' @param scope character vector of term labels (colon-separated, as in
#'   `"A:B"`) to test; must be a subset of the model's generating class.
#'   Defaults to the full generating class. Used by `drop1.loglm()` and
#'   `assoc_strength()`.
#' @param baseline `NULL` (default) to use the model of mutual independence
#'   over the variables in `object`'s generating class, or a fitted `loglm`
#'   model nested within `object` to use as the reference for partial R^2.
#'   Used by `LRanova()` only.
#' @param method `"Cramer"` (default) for partial Cramer's V, generalized to
#'   terms of any order by using the *smallest* factor level count among the
#'   term's variables in place of the usual two-way `min(r,c)`; this reduces
#'   exactly to the ordinary two-way Cramer's V (as used by
#'   [vcd::assocstats()]) when the term has two factors. Bounded to `[0,1]`.
#'   `"Cohen"` for Cohen's w = `sqrt(Delta-X^2 / N)`: unbounded, but requires
#'   no reference to table shape, so it stays well-defined uniformly for
#'   terms of any order. Used by `assoc_strength()` only.
#' @param test `"Chisq"` (default) to include an LR p-value column, or
#'   `"none"` to omit it.
#' @param abbrev passed to [get_model()] for the `Model:`/`Baseline:`
#'   heading line(s); `FALSE` (default) for full factor names, or an integer
#'   (e.g. `4`) to abbreviate each factor name to that many characters --
#'   useful when the generating class has many terms and the heading line
#'   gets long (see the DaytonSurvey example below).
#' @param ... currently unused in `drop1.loglm()`; passed on to it from
#'   `LRanova()`/`assoc_strength()`.
#'
#' @return An object of class `c("anova", "data.frame")` with columns `Df`,
#'   `LR Chisq`, `Pearson Chisq`, and (if `test = "Chisq"`) `Pr(>Chi)`, one
#'   row per tested term plus a `<none>` reference row. The fitted models
#'   underlying each row -- `object` itself as `"<none>"`, plus the refit
#'   dropping each term in `scope` -- are attached as `attr(., "models")`, a
#'   `loglmlist` (see [loglmlist()]) with names matching the table's row
#'   names. This is free (the models are already fit to compute the
#'   Delta-statistics) and makes the whole `*.loglmlist` toolchain available
#'   without refitting, e.g. `mosaic(attr(result, "models"), ask = FALSE)`
#'   for a grid of the full model and every drop-one model, or
#'   `LRstats(attr(result, "models"))` for an AIC/BIC comparison.
#'
#'   `LRanova()` adds a `Partial R2` column. The baseline model is attached
#'   separately as `attr(., "baseline")`, since it's a single reference
#'   model rather than one of the `<none>`/drop-one rows in
#'   `attr(., "models")`.
#'
#'   `assoc_strength()` adds a `"Cramer's V"` or `"Cohen's w"` column. Note
#'   this is a *partial* association -- conditional on the other terms in
#'   `object` -- not the marginal association from `vcd::assocstats()` on
#'   the term's own two-way margin; the two can differ sharply (see the
#'   UCBAdmissions `Admit:Gender` example below, a classic Simpson's-paradox
#'   case).
#'
#' @author Michael Friendly
#' @seealso [mosaic.loglmlist()], [LRstats()], [get_models()]
#'
#' @aliases drop1.loglm LRanova assoc_strength
#' @rdname drop1.loglm
#' @export
#'
#' @examples
#' library(MASS)
#' ucb <- loglm(~ (Admit + Gender + Dept)^2, data = UCBAdmissions)
#'
#' drop1.loglm(ucb)
#' drop1.loglm(ucb, scope = "Admit:Gender")   # test only a subset
#' drop1.loglm(ucb, abbrev = 4)               # abbreviate factor names
#' names(attr(drop1.loglm(ucb), "models"))    # the fitted models, for free
#'
#' # data.frame + `Freq ~ ...` form works too, not just array/table data
#' data(DaytonSurvey, package = "vcdExtra")
#' DS <- loglm(Freq ~ (cigarette + alcohol + marijuana + sex + race)^2,
#'             data = DaytonSurvey)
#' drop1.loglm(DS)
#'
#' LRanova(ucb)  # default baseline: mutual independence over Admit, Gender, Dept
#'
#' # supply a specific (nested) baseline other than the default -- here, one
#' # that already includes Admit:Gender, so partial R^2 is now relative to a
#' # smaller remaining G^2 than the mutual-independence baseline above
#' partial_baseline <- loglm(~ Admit + Gender + Dept + Admit:Gender, data = UCBAdmissions)
#' LRanova(ucb, baseline = partial_baseline)
#'
#' assoc_strength(ucb)                     # Cramer's V (default)
#' assoc_strength(ucb, method = "Cohen")   # Cohen's w
#'
#' # partial (conditional on Dept) vs. marginal association -- the small
#' # partial Admit:Gender V here versus the much larger marginal V below is
#' # the classic Simpson's-paradox story for this dataset
#' vcd::assocstats(margin.table(UCBAdmissions, c(1, 2)))
#'
#' # Cramer's V and Cohen's w coincide when every factor in a term is binary,
#' # and diverge once a term involves a factor with more levels
#' hec <- loglm(~ (Hair + Eye + Sex)^2, data = HairEyeColor)
#' assoc_strength(hec)  # Hair, Eye each have 4 levels
drop1.loglm <- function(object, scope, test = c("Chisq", "none"), abbrev = FALSE, ...) {
  if (!inherits(object, "loglm")) {
    stop("'object' must inherit from class 'loglm'.", call. = FALSE)
  }
  test <- match.arg(test)

  if (is.null(object$margin) || !length(object$margin)) {
    stop("'object' has no generating class ($margin); cannot determine ",
         "droppable terms.", call. = FALSE)
  }

  margin_labels <- vapply(object$margin, paste, collapse = ":",
                           FUN.VALUE = character(1L))

  if (missing(scope)) {
    scope <- margin_labels
  } else {
    if (!is.character(scope)) {
      stop("'scope' must be a character vector of term labels.", call. = FALSE)
    }
    if (!all(scope %in% margin_labels)) {
      stop("'scope' must be a subset of the generating class: ",
           paste(margin_labels, collapse = ", "), call. = FALSE)
    }
  }

  env <- environment(stats::formula(object))
  refit_drop <- function(term) {
    upd <- stats::update(object,
                          stats::as.formula(paste("~ . -", term)),
                          evaluate = FALSE)
    eval(upd, envir = env)
  }
  reduced <- lapply(scope, refit_drop)

  # Free: these models are already fit to get the Delta-statistics below, so
  # attaching them costs nothing and makes mosaic.loglmlist()/LRstats.loglmlist()/
  # get_models() available on the drop-one models without refitting.
  models <- c(list(object), reduced)
  names(models) <- c("<none>", scope)
  class(models) <- "loglmlist"

  df      <- c(object$df,      vapply(reduced, `[[`, numeric(1L), "df"))
  lrt     <- c(object$lrt,     vapply(reduced, `[[`, numeric(1L), "lrt"))
  pearson <- c(object$pearson, vapply(reduced, `[[`, numeric(1L), "pearson"))

  delta_df      <- df - df[1L]
  delta_lrt     <- lrt - lrt[1L]
  delta_pearson <- pearson - pearson[1L]
  delta_df[1L] <- delta_lrt[1L] <- delta_pearson[1L] <- NA_real_

  aod <- data.frame(
    Df = delta_df,
    `LR Chisq` = delta_lrt,
    `Pearson Chisq` = delta_pearson,
    check.names = FALSE,
    row.names = c("<none>", scope)
  )

  if (test == "Chisq") {
    p <- rep(NA_real_, length(scope) + 1L)
    p[-1L] <- stats::pchisq(delta_lrt[-1L], delta_df[-1L], lower.tail = FALSE)
    aod[["Pr(>Chi)"]] <- p
  }

  attr(aod, "heading") <- c(
    "Single term deletions",
    sprintf("Model: %s", get_model(object, type = "brackets", abbrev = abbrev))
  )
  attr(aod, "models") <- models
  class(aod) <- c("anova", "data.frame")
  aod
}

#' @rdname drop1.loglm
#' @export
LRanova <- function(object, baseline = NULL, test = c("Chisq", "none"), abbrev = FALSE, ...) {
  test <- match.arg(test)
  aod <- drop1.loglm(object, test = test, ...)

  orig_formula <- stats::formula(object)
  has_response <- length(orig_formula) == 3L
  response <- if (has_response) deparse(orig_formula[[2L]]) else NULL

  if (is.null(baseline)) {
    variables <- unique(unlist(object$margin, use.names = FALSE))
    baseline_call <- object$call
    baseline_call$formula <- stats::reformulate(variables, response = response)
    baseline_call[[1L]] <- quote(MASS::loglm)
    baseline <- eval(baseline_call, envir = environment(orig_formula))
  }
  if (!inherits(baseline, "loglm")) {
    stop("'baseline' must be NULL or a fitted 'loglm' model.", call. = FALSE)
  }

  # Nesting check (README: "MF: Yes, ensure nesting"). terms() on a
  # hierarchical loglm already expands to include implied lower-order
  # relatives, so a plain term.labels subset check is sufficient and cheap.
  base_terms  <- attr(stats::terms(baseline), "term.labels")
  model_terms <- attr(stats::terms(object),   "term.labels")
  extra <- setdiff(base_terms, model_terms)
  if (length(extra)) {
    stop("'baseline' is not nested within 'object': term(s) ",
         paste(extra, collapse = ", "),
         " appear in 'baseline' but not in 'object'.", call. = FALSE)
  }
  if (baseline$df < object$df) {
    stop("'baseline' has fewer residual df (", baseline$df, ") than 'object' (",
         object$df, "); 'baseline' must be the more restrictive model.",
         call. = FALSE)
  }

  baseline_g2 <- baseline$lrt
  is_term_row <- rownames(aod) != "<none>"
  partial_r2 <- rep(NA_real_, nrow(aod))
  if (is.finite(baseline_g2) && baseline_g2 > 0) {
    partial_r2[is_term_row] <- aod[["LR Chisq"]][is_term_row] / baseline_g2
  }
  aod[["Partial R2"]] <- partial_r2

  attr(aod, "heading") <- c(
    "Single term deletions, with partial R^2",
    sprintf("Model:    %s", get_model(object, type = "brackets", abbrev = abbrev)),
    sprintf("Baseline: %s  (G^2 = %.3f, df = %d)",
            get_model(baseline, type = "brackets", abbrev = abbrev), baseline_g2, baseline$df)
  )
  attr(aod, "baseline") <- baseline
  aod
}

#' @rdname drop1.loglm
#' @export
assoc_strength <- function(object, scope, method = c("Cramer", "Cohen"), abbrev = FALSE, ...) {
  method <- match.arg(method)
  aod <- drop1.loglm(object, scope = scope, ...)

  full <- if (!is.null(object$fitted)) object else stats::update(object, fitted = TRUE)
  N <- sum(full$fitted)
  dn <- dimnames(full$fitted)

  is_term_row <- rownames(aod) != "<none>"
  term_vars <- strsplit(rownames(aod)[is_term_row], ":", fixed = TRUE)
  x2 <- aod[["Pearson Chisq"]][is_term_row]

  effect <- rep(NA_real_, nrow(aod))
  if (method == "Cohen") {
    effect[is_term_row] <- sqrt(x2 / N)
    colname <- "Cohen's w"
  } else {
    k <- vapply(term_vars, function(vars) min(lengths(dn[vars])) - 1, numeric(1L))
    effect[is_term_row] <- sqrt(x2 / (N * k))
    colname <- "Cramer's V"
  }

  aod[[colname]] <- effect
  attr(aod, "heading") <- c(
    sprintf("Single term deletions, with %s", colname),
    sprintf("Model: %s", get_model(object, type = "brackets", abbrev = abbrev))
  )
  aod
}


# Worked examples ---------------------------------------------------------

if(FALSE) {
  library(MASS)

  ## Array/table-based data, no response variable in the formula -----------
  ucb_all_2way <- MASS::loglm(
    ~ (Admit + Gender + Dept)^2,
    data = UCBAdmissions
  ) |> print()

  ucb_drop1 <- drop1.loglm(ucb_all_2way) |> print()

  ## attr(., "models") is a loglmlist of {<none>, one per dropped term},
  ## free to compute (already fit for the Delta-statistics above) and usable
  ## with the existing *.loglmlist toolchain without refitting anything:
  mosaic(attr(ucb_drop1, "models"), ask = FALSE)   # grid of all models
  mosaic(attr(ucb_drop1, "models"), "Admit:Gender") # just the one of interest
  LRstats(attr(ucb_drop1, "models"))                # AIC/BIC comparison, for free
  get_models(attr(ucb_drop1, "models"))

  LRanova(ucb_all_2way) |> print()

  ## Partial effect sizes: Admit:Gender should come out much weaker than the
  ## marginal Cramer's V (0.143, per vcd::assocstats on the raw 2-way margin)
  ## -- the well-known Simpson's-paradox story for this dataset (the gender
  ## gap is largely a department effect).
  assoc_strength(ucb_all_2way)
  assoc_strength(ucb_all_2way, method = "Cohen")
  vcd::assocstats(margin.table(UCBAdmissions, c(1, 2)))

  ## Compare to base R, unchanged by this file -------------------------------
  stats::drop1(ucb_all_2way, test = "Chisq")
  MASS::dropterm(ucb_all_2way, test = "Chisq")

  ## ------------------------------------------------------------

  hair_eye_all_2way <- MASS::loglm(~ (Hair + Eye + Sex)^2, data = HairEyeColor)
  drop1.loglm(hair_eye_all_2way)
  LRanova(hair_eye_all_2way)
  assoc_strength(hair_eye_all_2way)

  ## data.frame + Freq response: this is the case that broke the first draft
  data("DaytonSurvey")

  DS_indep <- loglm(
    Freq ~ (cigarette + alcohol + marijuana + sex + race),
    data = DaytonSurvey
  ) |> print()

  DS_all_two_way <- update(DS_indep, . ~ .^2) |> print()

  DS_drop1 <- drop1.loglm(DS_all_two_way) |> print()
  DS_anova <- LRanova(DS_all_two_way) |> print()

  ## scope subset
  drop1.loglm(DS_all_two_way, scope = c("cigarette:alcohol", "cigarette:marijuana"))
  assoc_strength(DS_all_two_way)
  assoc_strength(DS_all_two_way, method = "Cohen")

  ## nesting check: a deliberately bad (non-nested) baseline should error
  ## (includes a 3-way term that DS_all_two_way, capped at 2-way, doesn't have)
  bad_baseline <- loglm(Freq ~ cigarette * alcohol * marijuana, data = DaytonSurvey)
  tryCatch(LRanova(DS_all_two_way, baseline = bad_baseline),
           error = function(e) cat("Expected error:", conditionMessage(e), "\n"))
}
