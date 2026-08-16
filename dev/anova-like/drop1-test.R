# Lightweight check suite for drop1.loglm.R -- stopifnot()-based, not
# testthat (testthat coverage is deprioritized for now; see README.md).
# Run with: Rscript dev/anova-like/drop1-test.R
# Consolidates the ad hoc checks used while developing drop1.loglm()/
# LRanova()/assoc_strength(), so they can be rerun after any change instead
# of re-verified by hand.

source("dev/anova-like/drop1.loglm.R")
suppressMessages(library(MASS))

ok <- function(desc) cat("OK:", desc, "\n")
near <- function(x, y, tol = 1e-2) isTRUE(all.equal(unname(x), y, tolerance = tol))

## drop1.loglm() on array/table data (UCBAdmissions) ------------------------

ucb <- loglm(~ (Admit + Gender + Dept)^2, data = UCBAdmissions)
d1 <- drop1.loglm(ucb)

stopifnot(
  inherits(d1, "anova"),
  identical(rownames(d1), c("<none>", "Admit:Gender", "Admit:Dept", "Gender:Dept")),
  near(d1["Admit:Gender", "LR Chisq"], 1.53),
  near(d1["Admit:Dept",   "LR Chisq"], 763.40),
  near(d1["Gender:Dept",  "LR Chisq"], 1128.70),
  near(d1["Admit:Gender", "Pearson Chisq"], 1.11)
)
ok("drop1.loglm() matches MASS::dropterm()/stats::drop1() reference values (UCBAdmissions)")

## scope subset ---------------------------------------------------------

d1_scope <- drop1.loglm(ucb, scope = "Admit:Gender")
stopifnot(identical(rownames(d1_scope), c("<none>", "Admit:Gender")))
ok("scope subset restricts rows to the requested term(s)")

stopifnot(inherits(tryCatch(drop1.loglm(ucb, scope = "Not:ARealTerm"), error = function(e) e), "error"))
ok("scope outside the generating class errors")

## abbrev only affects the Model: heading, not the table body ---------------

d1_abbrev <- drop1.loglm(ucb, abbrev = 4)
stopifnot(
  identical(rownames(d1_abbrev), rownames(d1)),
  near(d1_abbrev[["LR Chisq"]][-1], d1[["LR Chisq"]][-1]),
  grepl("Admt", attr(d1_abbrev, "heading")[2], fixed = TRUE)
)
ok("abbrev shortens the Model: heading without changing row names or results")

## models attribute: a loglmlist usable by existing *.loglmlist tools -------

models <- attr(d1, "models")
stopifnot(
  inherits(models, "loglmlist"),
  identical(names(models), rownames(d1)),
  identical(models[["<none>"]], ucb)
)
ok("attr(., 'models') is a loglmlist matching the table's rows")

invisible(capture.output({
  png(tempfile(fileext = ".png")); mosaic(models, ask = FALSE); dev.off()
}))
ok("mosaic() renders attr(., 'models') without error")

lr <- LRstats(models)
stopifnot(nrow(lr) == length(models))
ok("LRstats() works on attr(., 'models') without refitting")

## data.frame + `Freq ~ ...` form (the case that broke the original draft) --

data(DaytonSurvey, package = "vcdExtra")
DS_indep <- loglm(Freq ~ (cigarette + alcohol + marijuana + sex + race), data = DaytonSurvey)
DS <- update(DS_indep, . ~ .^2)
d1_ds <- drop1.loglm(DS)

stopifnot(
  near(d1_ds["cigarette:alcohol",   "LR Chisq"], 185.86),
  near(d1_ds["cigarette:marijuana", "LR Chisq"], 498.13)
)
ok("drop1.loglm() works on data.frame/Freq ~ ... fits (DaytonSurvey)")

## LRanova(): default baseline, partial R^2, nesting check ------------------

la <- LRanova(ucb)
stopifnot(
  near(la["Admit:Dept", "Partial R2"], 0.36393),
  near(la["Gender:Dept", "Partial R2"], 0.53807),
  inherits(attr(la, "baseline"), "loglm"),
  inherits(attr(la, "models"), "loglmlist")
)
ok("LRanova() partial R^2 matches reference values and carries baseline + models attrs")

bad_baseline <- loglm(Freq ~ cigarette * alcohol * marijuana, data = DaytonSurvey)
stopifnot(inherits(tryCatch(LRanova(DS, baseline = bad_baseline), error = function(e) e), "error"))
ok("LRanova() rejects a non-nested baseline")

## assoc_strength(): Cramer vs. Cohen -----------------------------------

as_cramer <- assoc_strength(ucb)
as_cohen  <- assoc_strength(ucb, method = "Cohen")
stopifnot(
  near(as_cramer["Admit:Dept", "Cramer's V"], 0.39228),
  near(as_cramer[["Cramer's V"]][-1], as_cohen[["Cohen's w"]][-1])  # every factor here is binary
)
ok("Cramer's V and Cohen's w coincide when every factor in a term is binary (UCBAdmissions)")

hec <- loglm(~ (Hair + Eye + Sex)^2, data = HairEyeColor)
as_hec_cramer <- assoc_strength(hec)
as_hec_cohen  <- assoc_strength(hec, method = "Cohen")
stopifnot(!near(as_hec_cramer["Hair:Eye", "Cramer's V"], as_hec_cohen["Hair:Eye", "Cohen's w"]))
ok("Cramer's V and Cohen's w diverge once a term has a factor with >2 levels (HairEyeColor)")

cat("\nAll checks passed.\n")
