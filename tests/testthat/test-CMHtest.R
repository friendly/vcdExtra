# The test case has been published first in the CAMIS project
# (https://psiaims.github.io/CAMIS/)
# The CAMIS test code has been written by
# Christina Fillmore, Lyn Taylor, Yannick Vandendijck and Logan Johnson.
# The work is published at https://github.com/PSIAIMS/CAMIS/blob/main/R/cmh.qmd
# under the Apache License 2.0.

data <- read.csv(testthat::test_path("data", "adcibc.csv")) |>
  dplyr::filter(TRTPN != "54" & AGEGR1 != ">80")

sas_results <- tibble::tribble(
  ~Scenario , ~Test                 , ~Chisq , ~Df , ~Prob  ,
   1L       , "Correlation"         , 0.2166 ,   1 , 0.6417 ,
   1L       , "Row Means"           , 0.2166 ,   1 , 0.6417 ,
   1L       , "General Association" , 0.2166 ,   1 , 0.6417 ,

   2L       , "Correlation"         , 0.0009 ,   1 , 0.9765 ,
   2L       , "Row Means"           , 2.4820 ,   1 , 0.2891 ,
   2L       , "General Association" , 2.4820 ,   1 , 0.2891 ,

   3L       , "Correlation"         , 0.0028 ,   1 , 0.9579 ,
   3L       , "Row Means"           , 2.3861 ,   2 , 0.3033 ,
   3L       , "General Association" , 2.3861 ,   2 , 0.3033 ,

   6L       , "Correlation"         , 1.7487 ,   1 , 0.1860 ,
   6L       , "Row Means"           , 1.7487 ,   1 , 0.1860 ,
   6L       , "General Association" , 8.0534 ,   4 , 0.0896 ,

   9L       , "Correlation"         , 0.0854 ,   1 , 0.7701 ,
   9L       , "Row Means"           , 2.4763 ,   2 , 0.2899 ,
   9L       , "General Association" , 7.0339 ,   8 , 0.5330 ,

  10L       , "Correlation"         , 1.6621 ,   1 , 0.1973 ,
  10L       , "Row Means"           , 2.2980 ,   4 , 0.6811 ,
  10L       , "General Association" , 5.7305 ,   8 , 0.6774
) |>
  dplyr::mutate(lang = "SAS")

test_that("")
