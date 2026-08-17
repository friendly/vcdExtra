# Comparison script for drop1.loglm() (R/drop1.loglm.R) vs. the base-R
# stats::drop1.glm() method, applied to the *same* hierarchical model fit two
# ways: MASS::loglm() vs. glm(..., family = poisson) on the equivalent
# Freq ~ (...)^2 data.
#
# Question: since stats::drop1.glm() already exists, does drop1.loglm() add
# anything, or does it just duplicate what a user could get from
# glm(family = poisson) + drop1()? Findings below, to fold into the
# documentation if they clarify anything:
#
#   1. stats::drop1.default() computes its default `scope` via
#      stats:::drop.scope(), which -- for a hierarchical formula like
#      `(A + B + C)^2` -- already restricts to the terms that can be dropped
#      *without* violating marginality, i.e. exactly the maximal/top-order
#      terms. For a model whose only maximal terms are the two-way
#      interactions, that is exactly the same term set as
#      MASS::loglm()'s $margin (what drop1.loglm() tests by default). No
#      manual `scope =` is needed on the glm side to make the comparison
#      fair -- see part 1 below.
#
#   2. Given that, `Df` and the LR statistic agree exactly (to floating-point
#      tolerance) between drop1.loglm()'s "LR Chisq" and drop1.glm()'s "LRT"
#      -- confirmed on all three of drop1.loglm()'s own @examples models
#      (UCBAdmissions, DaytonSurvey, HairEyeColor) in part 1 below.
#
#   3. What drop1.loglm() adds, given (2):
#        - works directly on table/array or `Freq ~ ...` data.frame input;
#          no manual conversion to a glm-ready data.frame or family=poisson
#          call needed.
#        - reports the Pearson X^2 alongside the LR G^2 in the same table;
#          drop1.glm() only ever reports the deviance-based LRT.
#        - attaches every fitted model (full + each drop-one) as a
#          `loglmlist`, reusable for free with mosaic()/LRstats()/
#          get_models(); drop1.glm() discards its refits.
#        - the heading uses bracket notation for the generating class
#          (via get_model()), more legible than a glm deviance/AIC table for
#          a loglinear audience.
#        - restricting scope to the generating class isn't just a default
#          that can be overridden into something misleading: it's the only
#          thing drop1.loglm() will test (validated against $margin) --
#          see part 2 below for what goes wrong if drop1.glm() is pointed at
#          a non-marginal term instead.
#
# Run after devtools::load_all() (for get_model()/drop1.loglm() etc).

library(MASS)      # loglm(), UCBAdmissions, HairEyeColor
library(vcdExtra)  # drop1.loglm(), DaytonSurvey

compare_drop1 <- function(loglm_fit, glm_fit, label) {
  d_loglm <- drop1.loglm(loglm_fit)
  d_glm   <- drop1(glm_fit, test = "Chisq")

  # drop1.loglm() always has a "<none>" row plus one row per margin term, in
  # that order; confirm drop1.glm()'s default scope landed on the same term
  # set, in the same order, before comparing the numbers row-for-row.
  same_terms <- identical(rownames(d_loglm), rownames(d_glm))

  rows <- rownames(d_loglm) != "<none>"
  ok_df  <- isTRUE(all.equal(d_loglm[rows, "Df"], unname(d_glm[rows, "Df"])))
  ok_lrt <- isTRUE(all.equal(d_loglm[rows, "LR Chisq"], unname(d_glm[rows, "LRT"]),
                              tolerance = 1e-6))

  cat(sprintf(
    "%s: same terms/order %s, Df %s, LR Chisq vs. LRT %s\n",
    label,
    if (same_terms) "OK" else "MISMATCH",
    if (ok_df) "OK" else "MISMATCH",
    if (ok_lrt) "OK" else "MISMATCH"
  ))
  stopifnot(same_terms, ok_df, ok_lrt)
  invisible(list(loglm = d_loglm, glm = d_glm))
}

# ==============================================================================
# Part 1: drop1.loglm() vs. drop1.glm() with its DEFAULT scope, on the three
# models used in drop1.loglm()'s own @examples.
# ==============================================================================

# --- UCBAdmissions: ~ (Admit + Gender + Dept)^2 -----------------------------

ucb <- loglm(~ (Admit + Gender + Dept)^2, data = UCBAdmissions)
ucb_glm <- glm(Freq ~ (Admit + Gender + Dept)^2,
               data = as.data.frame(UCBAdmissions), family = poisson)

# the term set stats::drop1.glm()'s default `scope` (via drop.scope()) lands
# on, vs. loglm()'s $margin -- these should be the same terms, same order
cat("drop1.glm() default scope:", paste(drop.scope(ucb_glm), collapse = ", "), "\n")
cat("loglm $margin:            ", paste(vapply(ucb$margin, paste, collapse = ":",
                                                FUN.VALUE = character(1)),
                                         collapse = ", "), "\n\n")

print(compare_drop1(ucb, ucb_glm, "UCBAdmissions"))

# --- DaytonSurvey: Freq ~ (cigarette+alcohol+marijuana+sex+race)^2 ---------

data(DaytonSurvey, package = "vcdExtra")
DS <- loglm(Freq ~ (cigarette + alcohol + marijuana + sex + race)^2, data = DaytonSurvey)
DS_glm <- glm(Freq ~ (cigarette + alcohol + marijuana + sex + race)^2,
              data = DaytonSurvey, family = poisson)

compare_drop1(DS, DS_glm, "DaytonSurvey")

# --- HairEyeColor: ~ (Hair + Eye + Sex)^2 -----------------------------------

hec <- loglm(~ (Hair + Eye + Sex)^2, data = HairEyeColor)
hec_glm <- glm(Freq ~ (Hair + Eye + Sex)^2,
               data = as.data.frame(HairEyeColor), family = poisson)

compare_drop1(hec, hec_glm, "HairEyeColor")

cat("\nAll three models: drop1.loglm() and stats::drop1.glm() agree exactly",
    "on Df and the LR statistic. drop1.glm() has no Pearson X^2 column and",
    "does not retain its refits.\n")

# ==============================================================================
# Part 2: what happens if drop1.glm() is pointed (manually) at a term outside
# the generating class -- e.g. the "Admit" main effect, while "Admit:Gender"
# and "Admit:Dept" remain in the model. drop1.loglm() can't do this: `scope`
# is validated against $margin and errors for anything else. Included here to
# document *why* that validation exists.
#
# stats:::drop1.glm()'s source (see ?drop1) shows it does NOT drop a term by
# rewriting the formula and rebuilding a fresh model.matrix() -- it deletes
# exactly the model-matrix COLUMNS tagged to that term (via the design
# matrix's "assign" attribute) from the ORIGINAL full-model matrix, then
# refits with glm.fit() directly on what's left:
#
#     ii <- seq_along(asgn)[asgn == ndrop[i]]
#     jj <- setdiff(seq(ncol(x)), ii)
#     z  <- glm.fit(x[, jj, drop = FALSE], y, wt, ...)
#
# That is a well-defined, strictly-nested comparison in column-space terms --
# but for a main effect whose interactions survive, it is NOT the same
# reduced model you'd get by writing out the reduced formula yourself and
# re-deriving a model.matrix() from scratch, because the surviving
# interaction columns (contrast-coded relative to the now-absent main
# effect) can no longer reconstruct it. The two are demonstrably different
# models below, and only the column-deletion one is what drop1.glm() reports.
# ==============================================================================

cat("\ndrop1.glm(ucb_glm, scope = \"Admit\") -- dropping a non-marginal term:\n")
d_admit <- drop1(ucb_glm, scope = "Admit", test = "Chisq")
print(d_admit)

refit_formula <- update(ucb_glm, . ~ . - Admit)
cat(sprintf("\nFull model deviance:                                %.3f\n", deviance(ucb_glm)))
cat(sprintf("drop1()'s column-deletion 'Admit' deviance:         %.3f  (LRT vs. full: %.2f, printed above)\n",
            d_admit["Admit", "Deviance"], d_admit["Admit", "LRT"]))
cat(sprintf("update(., . ~ . - Admit)'s formula-refit deviance:  %.3f  (== full model deviance)\n",
            deviance(refit_formula)))
cat(
  "\nGender/Dept plus the surviving Admit:Gender/Admit:Dept interactions already\n",
  "reconstruct the full fit once refit from a *formula* -- symbolically dropping\n",
  "\"Admit\" changes nothing there. drop1()'s column-deletion approach instead\n",
  "removes only the raw \"Admit\" columns from the ORIGINAL design matrix and\n",
  "refits via glm.fit() directly (see stats:::drop1.glm source above), which IS a\n",
  "strictly smaller column space -- so it reports a real-looking LRT (~74) for a\n",
  "deletion that has no interpretable hierarchical-model meaning.\n",
  sep = ""
)

cat("\ndrop1.loglm(ucb, scope = \"Admit\") refuses to run this at all:\n")
print(tryCatch(
  drop1.loglm(ucb, scope = "Admit"),
  error = function(e) cat("  error:", conditionMessage(e), "\n")
))

cat("\nDone.\n")
