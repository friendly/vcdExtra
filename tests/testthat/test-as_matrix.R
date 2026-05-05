data("HairEyeColor")

arrayDat <- unclass(HairEyeColor) # Generate array form data
freqForm <- as.data.frame(HairEyeColor) # Generate frequency form data
tidy_freqForm <- dplyr::as_tibble(HairEyeColor) # Generate tidy frequency form data
caseForm <- expand.dft(freqForm) # Generate case form data

test_that("Matrix returns when array supplied", {
  mat <- as_matrix(arrayDat, dim = c("Hair", "Eye"))
  expect_true(inherits(mat, "matrix"))
})

test_that("Matrix returns when table supplied", {
  mat <- as_matrix(HairEyeColor, dim = c("Hair", "Eye"))
  expect_true(inherits(mat, "matrix"))
})

test_that("Matrix returns when frequency form supplied", {
  mat <- as_matrix(freqForm, freq = "Freq", dim = c("Hair", "Eye"))
  expect_true(inherits(mat, "matrix"))
})

test_that("Matrix returns when (tibble) frequency form supplied", {
  mat <- as_matrix(tidy_freqForm, freq = "n", dim = c("Hair", "Eye"))
  expect_true(inherits(mat, "matrix"))
})

test_that("Matrix returns when case form supplied", {
  mat <- as_matrix(caseForm, dim = c("Hair", "Eye"))
  expect_true(inherits(mat, "matrix"))
})

test_that("Correct dimensions returned", {
  mat <- as_matrix(HairEyeColor, dim = c("Hair", "Eye"))
  expect_true(all(names(dimnames(mat)) == c("Hair", "Eye")))
})

  