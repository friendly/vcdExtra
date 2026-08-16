# Prototype term-level likelihood-ratio tests for MASS::loglm models.
#
# This file deliberately lives under dev/: the API and the definitions of the
# effect-size columns still need review before promotion to R/.

.margin_formula <- function(margin) {
  if (!length(margin)) {
    return(stats::as.formula("~ 1"))
  }

  labels <- vapply(
    margin,
    function(x) paste(x, collapse = ":"),
    character(1L)
  )
  stats::reformulate(labels)
}

.drop_loglm_margin <- function(object, index) {
  reduced_margin <- object$margin[-index]
  reduced_formula <- .margin_formula(reduced_margin)

  model_call <- object$call
  model_call$formula <- reduced_formula
  model_call[[1L]] <- quote(MASS::loglm)
  eval(model_call, envir = environment(stats::formula(object)))
}

LRanova <- function(model, baseline = NULL) {
  if (!inherits(model, "loglm")) {
    stop("`model` must inherit from class 'loglm'.", call. = FALSE)
  }
  if (is.null(model$margin) || !length(model$margin)) {
    stop("The model does not contain a non-empty generating class.", call. = FALSE)
  }

  reduced <- lapply(seq_along(model$margin), function(i) {
    .drop_loglm_margin(model, i)
  })

  delta_g2 <- vapply(reduced, function(x) x$lrt - model$lrt, numeric(1L))
  delta_x2 <- vapply(reduced, function(x) x$pearson - model$pearson, numeric(1L))
  delta_df <- vapply(reduced, function(x) x$df - model$df, numeric(1L))

  if (is.null(baseline)) {
    variables <- unique(unlist(model$margin, use.names = FALSE))
    baseline_call <- model$call
    baseline_call$formula <- stats::reformulate(variables)
    baseline_call[[1L]] <- quote(MASS::loglm)
    baseline <- eval(
      baseline_call,
      envir = environment(stats::formula(model))
    )
  }
  if (!inherits(baseline, "loglm")) {
    stop("`baseline` must be NULL or a fitted 'loglm' model.", call. = FALSE)
  }

  baseline_g2 <- baseline$lrt
  partial_r2 <- if (is.finite(baseline_g2) && baseline_g2 > 0) {
    delta_g2 / baseline_g2
  } else {
    rep(NA_real_, length(delta_g2))
  }

  terms <- vapply(
    model$margin,
    function(x) paste(x, collapse = ":"),
    character(1L)
  )

  result <- data.frame(
    `Delta-G^2` = delta_g2,
    `Delta-X^2` = delta_x2,
    Df = delta_df,
    `Pr(>Chisq)` = stats::pchisq(delta_g2, delta_df, lower.tail = FALSE),
    `Partial R^2` = partial_r2,
    check.names = FALSE,
    row.names = terms
  )
  attr(result, "heading") <- "Term-level Analysis of Association"
  attr(result, "baseline.G2") <- baseline_g2
  class(result) <- c("anova", "data.frame")
  result
}


# Worked examples ---------------------------------------------------------

library(MASS)
ucb_all_2way <- MASS::loglm(
  ~ (Admit + Gender + Dept)^2,
  data = UCBAdmissions
) |> print()


ucb_anova <- LRanova(ucb_all_2way) |>
  print()

str(ucb_anova)

MASS::dropterm(ucb_all_2way, test = "Chisq")


## ------------------------------------------------------------


hair_eye_all_2way <- MASS::loglm(
  ~ (Hair + Eye + Sex)^2,
  data = HairEyeColor
)
LRanova(hair_eye_all_2way)

## ------------------------------------------------------------
##
data("DaytonSurvey")

DS_indep <- loglm(
  Freq ~ (cigarette + alcohol + marijuana + sex + race),
  data = DaytonSurvey
) |> print()

# add all two-way terms
DS_all_two_way <- update(DS_indep, . ~ .^2) |>
  print()


DS_anova_2 <- LRanova(DS_all_two_way) |>
  print()

# Gives:
# Error in loglm1.data.frame(formula, data, ..., .call = .call, .formula = .formula) :
#   formula specifies no response

