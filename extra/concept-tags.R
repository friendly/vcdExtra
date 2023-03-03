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

# join with concepts

dset_split <- dset_split |>
  left_join(tags, by = "tag") |> 
  rename(concept = topic) 
  


dset_split <- dset_split |>
  group_by(dataset) |>
  summarise(concept = paste(concept, collapse = ";"))  

# read in the man file, append concept lines
add_concepts <- function(dset_name, concepts){
  fname <- glue::glue("man/{dset_name}.Rd")
  lines <- readLines(fname)
  topics <- stringr::str_split(concepts, "; ?") |> unlist()
  topics <- glue::glue("\\concept{{{topics}}}")
  lines <- c(lines, topics)
  cat(tail(lines), sep="\n")
}

add_concepts("Cormorants", c("glm; poisson"))
