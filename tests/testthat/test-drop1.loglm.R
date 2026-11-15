skip_if_not_installed("MASS")

test_that("drop1.loglm() and assoc_strength() find data not in the global environment", {
  fit_local <- function(fitted) {
    hec <- margin.table(HairEyeColor, c(1, 3))
    MASS::loglm(~ Hair + Sex, data = hec, fitted = fitted)
  }
  hec <- margin.table(HairEyeColor, c(1, 3))
  d_global <- drop1(MASS::loglm(~ Hair + Sex, data = hec))
  rm(hec)

  local_mod <- fit_local(fitted = FALSE)
  d_local <- drop1(local_mod)
  expect_s3_class(d_local, "anova")
  expect_equal(rownames(d_local), rownames(d_global))
  expect_equal(as.list(d_local), as.list(d_global), ignore_attr = TRUE)

  # assoc_strength() refits with fitted = TRUE when the model lacks $fitted
  expect_no_error(assoc_strength(local_mod))
})

test_that("drop1.loglm() works on models from the Hoyt example", {
  Hoyt1 <- collapse.table(Hoyt, Status = c("College", rep("Non-College", 3)))
  mod <- MASS::loglm(~ Status * (Sex + Rank + Occupation) + (Sex * Rank * Occupation),
                     data = Hoyt1, fitted = TRUE)
  d <- drop1(mod, test = "Chisq")
  expect_equal(nrow(d), 5L)
})

test_that("add1.loglm() matches add1.default() and finds non-global data", {
  Hoyt1 <- collapse.table(Hoyt, Status = c("College", rep("Non-College", 3)))
  mod <- MASS::loglm(~ Status * (Sex + Rank + Occupation) + (Sex * Rank * Occupation),
                     data = Hoyt1, fitted = TRUE)
  a <- add1(mod, ~ .^2, test = "Chisq")
  expect_s3_class(a, "anova")
  expect_equal(rownames(a),
               c("<none>", "Status:Sex:Rank", "Status:Sex:Occupation", "Status:Rank:Occupation"))
  expect_equal(a$Df[-1], c(2, 6, 12))
  expect_equal(a[["LR Chisq"]][-1], c(1.1271, 29.0961, 9.1599), tolerance = 1e-4)

  # same LR change as refitting by hand
  m2 <- update(mod, ~ . + Status:Sex:Occupation)
  expect_equal(a["Status:Sex:Occupation", "LR Chisq"], mod$lrt - m2$lrt)
  expect_equal(a["Status:Sex:Occupation", "Pearson Chisq"], mod$pearson - m2$pearson)

  # default scope: the saturated model gives the same hierarchical candidates here
  expect_equal(rownames(add1(mod)), rownames(a))

  # fitted models are attached
  expect_s3_class(attr(a, "models"), "loglmlist")
  expect_equal(names(attr(a, "models")), rownames(a))
})

test_that("add1.loglm() handles character scope, frequency data frames, and bad scopes", {
  ucb <- MASS::loglm(~ Admit + Gender + Dept, data = UCBAdmissions)
  expect_equal(rownames(add1(ucb)), c("<none>", "Admit:Gender", "Admit:Dept", "Gender:Dept"))
  a1 <- add1(ucb, scope = "Admit:Dept", test = "none")
  expect_equal(rownames(a1), c("<none>", "Admit:Dept"))
  expect_false("Pr(>Chi)" %in% names(a1))

  ucb2 <- update(ucb, ~ .^2)
  expect_equal(rownames(add1(ucb2)), c("<none>", "Admit:Gender:Dept"))
  expect_error(add1(ucb2, scope = "Admit:Dept"), "already in the model")
  expect_error(add1(update(ucb, ~ .^3)), "no terms in scope")

  ucb_df <- as.data.frame(UCBAdmissions)
  dmod <- MASS::loglm(Freq ~ Admit + Gender + Dept, data = ucb_df)
  expect_equal(add1(dmod)[["LR Chisq"]], add1(ucb)[["LR Chisq"]], tolerance = 1e-6)
})
