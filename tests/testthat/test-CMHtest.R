# The test cases have been published first in the CAMIS project
# (https://psiaims.github.io/CAMIS/)
# The CAMIS test code has been written by
# Christina Fillmore, Lyn Taylor, Yannick Vandendijck and Logan Johnson.
# The work is published at https://github.com/PSIAIMS/CAMIS/blob/main/R/cmh.qmd
# under the Apache License 2.0.
# The code has been adapted here in order to fit into the testthat framework.

data <- read.csv(testthat::test_path("data", "adcibc.csv"))
data2 <- data %>%
  dplyr::filter(
    TRTPN != "54",
    AGEGR1 != ">80"
  )

sas_results <- tibble::tribble(
  ~Scenario , ~Test                 , ~Chisq , ~Df , ~Prob  ,
  1L        , "Correlation"         , 0.2166 ,   1 , 0.6417 ,
  1L        , "Row Means"           , 0.2166 ,   1 , 0.6417 ,
  1L        , "General Association" , 0.2166 ,   1 , 0.6417 ,

  2L        , "Correlation"         , 0.0009 ,   1 , 0.9765 ,
  2L        , "Row Means"           , 2.4820 ,   2 , 0.2891 , # corrected Df
  2L        , "General Association" , 2.4820 ,   2 , 0.2891 , # corrected Df

  3L        , "Correlation"         , 0.0028 ,   1 , 0.9579 ,
  3L        , "Row Means"           , 2.3861 ,   2 , 0.3033 ,
  3L        , "General Association" , 2.3861 ,   2 , 0.3033 ,

  4L        , "Correlation"         , 1.7487 ,   1 , 0.1860 ,
  4L        , "Row Means"           , 1.7487 ,   1 , 0.1860 ,
  4L        , "General Association" , 8.0534 ,   4 , 0.0896 ,

  5L        , "Correlation"         , 0.0854 ,   1 , 0.7701 ,
  5L        , "Row Means"           , 2.4763 ,   2 , 0.2899 ,
  5L        , "General Association" , 7.0339 ,   8 , 0.5330 ,

  6L        , "Correlation"         , 1.6621 ,   1 , 0.1973 ,
  6L        , "Row Means"           , 2.2980 ,   4 , 0.6811 ,
  6L        , "General Association" , 5.7305 ,   8 , 0.6774
)

convert_to_tibble <- function(cmh_test) {
  table <- cmh_test$ALL$table
  result <- tibble::as_tibble(table) %>%
    dplyr::mutate(
      dplyr::across(dplyr::everything(), unlist),
      Test = rownames(table),
      Test = dplyr::case_when(
        Test == "cor" ~ "Correlation",
        Test == "rmeans" ~ "Row Means",
        Test == "cmeans" ~ "Column Means",
        Test == "general" ~ "General Association"
      )
    )
  result[
    match(c("Correlation", "Row Means", "General Association"), result$Test),
    c("Test", "Chisq", "Df", "Prob")
  ]
}

get_sas_results <- function(scenario) {
  sas_results %>%
    dplyr::filter(Scenario == scenario) %>%
    dplyr::select(Test, Chisq, Df, Prob)
}

test_that("2x2x2 schema, rather balanced groups", {
  result <- convert_to_tibble(CMHtest(
    Freq ~ TRTP + SEX | AGEGR1,
    data = data2,
    overall = TRUE
  ))
  expected <- get_sas_results(1L)
  expect_equal(result, expected, tolerance = 1e-3)
})

test_that("3x2x3 schema, rather balanced groups", {
  result <- convert_to_tibble(vcdExtra::CMHtest(
    Freq ~ TRTP + SEX | AGEGR1,
    data = data,
    overall = TRUE
  ))
  expected <- get_sas_results(2L)
  expect_equal(result, expected, tolerance = 1e-3)
})

test_that("3x2x3 schema, one stratum with a single observation to be omitted", {
  stopifnot(any(table(data$RACE) == 1))
  data_subset <- data %>%
    dplyr::filter(RACE != "AMERICAN INDIAN OR ALASKA NATIVE")
  # Question: Could this be done automatically in CMHtest()?
  result <- convert_to_tibble(vcdExtra::CMHtest(
    Freq ~ TRTP + SEX | RACE,
    data = data_subset,
    overall = TRUE
  ))
  expected <- get_sas_results(3L)
  expect_equal(result, expected, tolerance = 1e-3)
})

test_that("2x5x2 schema, ordinal variable", {
  result <- convert_to_tibble(vcdExtra::CMHtest(
    Freq ~ TRTP + AVAL | SEX,
    data = data2,
    overall = TRUE
  ))
  expected <- get_sas_results(4L)
  expect_equal(result, expected, tolerance = 1e-3)
})

test_that("3x5x17 schema, sparse groups, ordinal variable", {
  stopifnot(any(table(data$SITEID) == 1))
  subset_data <- data %>%
    dplyr::filter(SITEID != "702") # only one observation in this stratum
  result <- convert_to_tibble(vcdExtra::CMHtest(
    Freq ~ TRTP + AVAL | SITEID,
    data = subset_data,
    overall = TRUE
  ))
  expected <- get_sas_results(5L)
  expect_equal(result, expected, tolerance = 1e-3)
})

test_that("5x3x3 schema, ordinal variables", {
  result <- convert_to_tibble(vcdExtra::CMHtest(
    Freq ~ AVAL + AGEGR1N | TRTP,
    data = data,
    overall = TRUE
  ))
  expected <- get_sas_results(6L)
  expect_equal(result, expected, tolerance = 1e-3)
})
