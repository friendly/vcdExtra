data("HairEyeColor")

freqForm <- as.data.frame(HairEyeColor) # Generate frequency form data
tidy_freqForm <- dplyr::as_tibble(HairEyeColor) # Generate tidy frequency form data
tableForm <- as_table(HairEyeColor) # Generate table form data
arrayDat <- as_array(HairEyeColor) # Generate an array

test_that("Number of rows equal to number of entries", {
  expect_equal(
    sum(freqForm$Freq), 
    HairEyeColor |> as_caseform() |> nrow()
  )
})

test_that("Returns tibble by default", {
  expect_s3_class(as_caseform(freqForm), "tbl")
})