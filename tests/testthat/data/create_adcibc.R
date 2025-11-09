# The test data is from the CDISC Pilot data set,
# which is publicly available on the PHUSE Test Data Factory repository.

url <- "https://github.com/phuse-org/TestDataFactory/raw/main/Updated/TDF_ADaM/adcibc.xpt"
adcbic <- haven::read_xpt(url) %>%
  dplyr::filter(
    EFFFL == 'Y' & ITTFL == 'Y',
    AVISITN %in% c(8, 16, 24) & ANL01FL == 'Y'
  )
write.csv(
  adcbic,
  file = testthat::test_path("data", "adcibc.csv")
)
