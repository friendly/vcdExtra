data("HairEyeColor")

arrayDat <- as_array(HairEyeColor) # Generate an array
freqForm <- as.data.frame(HairEyeColor) # Generate frequency form data
tidy_freqForm <- dplyr::as_tibble(HairEyeColor) # Generate tidy frequency form data
caseForm <- expand.dft(freqForm) # Generate case form data

test_that("Array form input returns a tibble", {
  ff <- as_freqform(arrayDat)
  expect_s3_class(ff, "tbl")
})

test_that("Table form input returns a tibble", {
  ff <- as_freqform(HairEyeColor)
  expect_s3_class(ff, "tbl")
})

test_that("Tibble frequency form input returns a tibble", {
  ff <- as_freqform(tidy_freqForm, freq = "n")
  expect_s3_class(ff, "tbl")
})

test_that("Frequency form input returns a tibble", {
  ary <- as_freqform(freqForm, freq = "Freq")
  expect_s3_class(ff, "tbl")
})

test_that("Case form input returns a tibble", {
  ary <- as_freqform(caseForm)
  expect_s3_class(ff, "tbl")
})

test_that("Tibble frequency form input returns unmodified", {
  ff <- as_freqform(tidy_freqForm, freq = "n")
  expect_equal(sum(ff$Freq), sum(tidy_freqForm$n))
})

