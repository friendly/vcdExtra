data("HairEyeColor")
data("Hoyt", package = "vcdExtra")

test_that("Equal sum produced via comparison with HairEyeColor data", {
  coll.sum <- collapse_levels(
    HairEyeColor,                 
    Hair = list(                  
      Dark = c("Black", "Brown"), 
      Light = c("Blond", "Red")  
    ),
    Eye = list(                   
      Common = c("Brown"),
      Uncommon = c("Blue", "Green", "Hazel")
    )
  ) |> sum()
  expect_equal(sum(HairEyeColor), coll.sum)
})

test_that("Equal sum produced via comparison with Hoyt data", {
  ff_Hoyt <- as_freqform(Hoyt)
  names(ff_Hoyt)[length(ff_Hoyt)] <- "n"
  
  ff_coll <- collapse_levels(
    ff_Hoyt,
    freq = "n",
    Occupation = list(
      High = c(1, 2),
      Middle = 3,
      Low = 4,
      VeryLow = c(5, 6, 7)
    )
  )
  expect_equal(sum(ff_coll$Freq), sum(Hoyt))
})
