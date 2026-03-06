test_that("as_table returns table if table is supplied", {
  data("HairEyeColor")
  tab <- as_table(HairEyeColor)
  expect_s3_class(tab, "table")
})

test_that("as_table returns table if array is supplied", {
  data("HairEyeColor")
  tab <- as_table(unclass(HairEyeColor)) # unclass() converts table -> array
  expect_s3_class(tab, "table")
})

test_that("as_table returns table if freqform is supplied", {
  data("HairEyeColor")
  tab <- as_table(as.data.frame(HairEyeColor), freq = "Freq")
  expect_s3_class(tab, "table")
})

test_that("as_table returns table if case form is supplied", {
  data("HairEyeColor")
  tab <- as_table(expand.dft(HairEyeColor))
  expect_s3_class(tab, "table")
})

test_that("as_table does not modify table entries", {
  data("HairEyeColor")
  tab <- as_table(HairEyeColor)
  tab_freq <- as_table(as.data.frame(HairEyeColor), freq = "Freq")
  tab_case <- as_table(expand.dft(HairEyeColor))
  expect_equal(sum(tab), sum(HairEyeColor))
  expect_equal(sum(tab_freq), sum(tab_case))
  expect_equal(sum(tab), sum(tab_case))
})

test_that("Proportions sum to one", {
  data("HairEyeColor")
  tab <- as_table(HairEyeColor, prop = TRUE)
  expect_equal(sum(tab), 1)
})

test_that("Marginal proportions sum to one", {
  data("HairEyeColor")
  tab <- as_table(HairEyeColor, prop = c("Hair", "Sex"))
  expect_equal(sum(tab["Black",,"Male"]), 1)
  tab <- as_table(HairEyeColor, prop = "Hair")
  expect_equal(sum(tab["Black",,]), 1)
})
