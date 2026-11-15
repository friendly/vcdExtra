skip_if_not_installed("gt")
skip_if_not_installed("MASS")

# Residual values shown in the cells of a color_table (values = "residuals"),
# as a numeric matrix in display order
shown_resids <- function(ct) {
  d <- ct[["_data"]]
  cols <- setdiff(names(d), ct[["_boxhead"]]$var[ct[["_boxhead"]]$type == "stub"])
  m <- as.matrix(d[cols])
  storage.mode(m) <- "numeric"
  unname(m)
}

tab <- as.table(array(c(135, 10, 1, 6, 2, 2, 2, 8), dim = c(2, 2, 2),
  dimnames = list(Related   = c("Not related", "Related"),
                  Financial = c("No gain", "Gain"),
                  Killer    = c("No", "Yes"))))
mod <- MASS::loglm(~ Related * Financial + Killer, data = tab, fitted = TRUE)
r   <- residuals(mod, type = "pearson")

test_that("model residuals go into the right cells with two row variables", {
  fml <- Killer ~ Related + Financial
  ct <- suppressMessages(
    color_table(tab, formula = fml, model = mod, values = "residuals", digits = 4))
  expected <- unname(as.matrix(vcd::structable(fml, data = r)))
  expect_equal(shown_resids(ct), round(expected, 4))
  # the cells swapped by the old bug
  expect_equal(shown_resids(ct)[2:3, 2], c(3.4731, 0.9821))
})

test_that("model residuals go into the right cells with two column variables", {
  fml <- Related + Financial ~ Killer
  ct <- suppressMessages(
    color_table(tab, formula = fml, model = mod, values = "residuals", digits = 4))
  expected <- unname(as.matrix(vcd::structable(fml, data = r)))
  expect_equal(shown_resids(ct), round(expected, 4))
})

test_that("order of the table's dimensions does not matter", {
  tab2 <- aperm(tab, c(3, 2, 1))
  mod2 <- MASS::loglm(~ Related * Financial + Killer, data = tab2, fitted = TRUE)
  fml <- Killer ~ Related + Financial
  ct <- suppressMessages(
    color_table(tab, formula = fml, model = mod2, values = "residuals", digits = 4))
  expected <- unname(as.matrix(vcd::structable(fml, data = r)))
  expect_equal(shown_resids(ct), round(expected, 4))
})

test_that("glm residuals are placed by factor names", {
  df <- as.data.frame(tab)
  gmod <- glm(Freq ~ Related * Financial + Killer, data = df, family = poisson)
  fml <- Killer ~ Related + Financial
  ct <- suppressMessages(
    color_table(tab, formula = fml, model = gmod, values = "residuals", digits = 4))
  expected <- unname(as.matrix(vcd::structable(fml, data = r)))
  expect_equal(shown_resids(ct), round(expected, 4), tolerance = 1e-3)
})

test_that("expected frequencies can be given as an array", {
  fml <- Killer ~ Related + Financial
  ct <- suppressMessages(
    color_table(tab, formula = fml, expected = fitted(mod), values = "residuals", digits = 4))
  expected <- unname(as.matrix(vcd::structable(fml, data = r)))
  expect_equal(shown_resids(ct), round(expected, 4))
})

test_that("model residuals are placed correctly for structable and ftable input", {
  fml <- Killer ~ Related + Financial
  expected <- unname(as.matrix(vcd::structable(fml, data = r)))
  ct_st <- suppressMessages(color_table(vcd::structable(fml, data = tab), model = mod,
                                        values = "residuals", digits = 4))
  expect_equal(shown_resids(ct_st), round(expected, 4))
  ct_ft <- suppressMessages(color_table(ftable(tab, row.vars = 1:2), model = mod,
                                        values = "residuals", digits = 4))
  expect_equal(shown_resids(ct_ft), round(expected, 4))
})

test_that("2-way tables still work with a model", {
  hec <- margin.table(HairEyeColor, 1:2)
  m2 <- MASS::loglm(~ Hair + Eye, data = hec, fitted = TRUE)
  ct <- suppressMessages(color_table(hec, model = m2, values = "residuals", digits = 4))
  expect_equal(shown_resids(ct), unname(round(unclass(residuals(m2, type = "pearson")), 4)))
})

test_that("mismatched model gives an error", {
  bad <- MASS::loglm(~ Hair + Eye, data = margin.table(HairEyeColor, 1:2), fitted = TRUE)
  expect_error(suppressMessages(color_table(tab, model = bad)))
})

test_that("shading note is a message, and only when not shown in the table", {
  expect_message(color_table(tab, formula = Killer ~ Related + Financial), "complete independence")
  expect_silent(color_table(tab, formula = Killer ~ Related + Financial, legend = TRUE))
  expect_message(color_table(tab, model = mod), "fitted model")
  out <- capture.output(invisible(suppressMessages(color_table(tab, model = mod))))
  expect_length(out, 0)
})
