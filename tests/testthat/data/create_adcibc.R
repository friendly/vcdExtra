# The test data is from the CDISC Pilot data set,
# which is publicly available on the PHUSE Test Data Factory repository.

library(dplyr)
url <- "https://github.com/phuse-org/TestDataFactory/raw/main/Updated/TDF_ADaM/adcibc.xpt"
adcbic <- haven::read_xpt(url) %>%
  dplyr::filter(
    EFFFL == "Y",
    ITTFL == "Y",
    AVISIT == "Week 8",
    ANL01FL == "Y",
    TRTPN != "54",
    AGEGR1 != ">80"
  )
write.csv(
  adcbic,
  file = testthat::test_path("data", "adcibc.csv")
)
