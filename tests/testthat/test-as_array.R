data("HairEyeColor")

arrayDat <- unclass(HairEyeColor) # Generate array form data
freqForm <- as.data.frame(HairEyeColor) # Generate frequency form data
tidy_freqForm <- dplyr::as_tibble(HairEyeColor) # Generate tidy frequency form data
caseForm <- expand.dft(freqForm) # Generate case form data

test_that("Array form input returns an array", {
  ary <- as_array(arrayDat)
  expect_true(is.array(ary))
})

test_that("Table form input returns an array", {
  ary <- as_array(HairEyeColor)
  expect_true(is.array(ary))
})

test_that("Tibble frequency form input returns an array", {
  ary <- as_array(tidy_freqForm, freq = "n")
  expect_true(is.array(ary))
})

test_that("Frequency form input returns an array", {
  ary <- as_array(freqForm, freq = "Freq")
  expect_true(is.array(ary))
})

test_that("Case form input returns an array", {
  ary <- as_array(caseForm)
  expect_true(is.array(ary))
})

test_that("Check if input was modified", {
  ary <- sum(as_array(HairEyeColor))
  expect_equal(ary, sum(HairEyeColor))
})

