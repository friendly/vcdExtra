# add concept tags to man files


library(dplyr)
library(tidyr)
library(readxl)

dsets_tagged <- read_excel("inst/extdata/vcdExtra-datasets.xlsx", 
                           sheet="vcdExtra-datasets")
tags <- read_excel("inst/extdata/vcdExtra-datasets.xlsx", 
                   sheet="tags")

dsets_tagged <- dsets_tagged |>
  dplyr::select(-Title, -dim) |>
  dplyr::rename(dataset = Item)

dset_split <- dsets_tagged |>
  tidyr::separate_longer_delim(tags, delim = ";") |>
  dplyr::mutate(tag = stringr::str_trim(tags)) |>
  dplyr::select(-tags)

head(dset_split)

add_concepts <- function(dset_name, topics){
  fname <- glue::glue("man/{dset_name}.Rd")
  lines <- readLines(fname)
  topics <- glue::glue("\concept{[topics]}", open="[", close="]")
  lines <- c(lines, topics)
  
}
