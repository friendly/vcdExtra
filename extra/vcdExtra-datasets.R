#' ---
#' title: Classify vcdExtra datasets
#' ---



library(dplyr)
library(tidyr)
library(readxl)

dsets <- datasets("vcdExtra") |> 
  select("Item", "Title", "class", "dim")

write.csv(dsets, file = "extra/vcdExtra-datasets.csv")

# -------------------------------------------------------

#' ## classify them in the equivalent xlsx file ...

dsets_tagged <- read_excel("extra/vcdExtra-datasets.xlsx", 
                           sheet="vcdExtra-datasets")

dsets_tagged <- dsets_tagged |>
  dplyr::select(-Title, -dim) |>
  dplyr::rename(dataset = Item)

#' to invert the table, need to split tags into separate observations
dset_split <- dsets_tagged |>
  tidyr::separate_longer_delim(tags, delim = ";") |>
  dplyr::mutate(tag = stringr::str_trim(tags)) |>
  select(-tags)

#' ## collapse the rows for the same tag
tag_dset <- dset_split |>
  arrange(tag) |>
  dplyr::group_by(tag) |>
  dplyr::summarise(datasets = paste(dataset, collapse = "; ")) |> ungroup()

# get a list of the unique tags
cat(paste(unique(tag_dset$tag), collapse ="\n"))

#' ## make this into a nice table
#' The `tags` sheet contain the `tag` and a `topic` description
tags <- read_excel("extra/vcdExtra-datasets.xlsx", 
                   sheet="tags")

tag_dset <- tag_dset |>
  left_join(tags, by = "tag") |>
  dplyr::relocate(topic, .after = tag)

tag_dset |>
  dplyr::select(-tag) |>
  knitr::kable()

#' ## add links to the names of datasets
#' This function is designed to work with the `pkgdown` site, where documentation
#' for datasets in under `reference/` 
add_links <- function(dsets, 
                      prefix = "reference/", 
                      suffix = ".html",
                      sep = "; ") {

  names <- stringr::str_split_1(dsets, sep)
  names <- glue::glue('[{names}]("{prefix}{names}{suffix}")')
  glue::glue_collapse(names, sep = sep)
}

add_links("Bartlett; Fungicide")

tag_dset |>
  dplyr::select(-tag) |>
  dplyr::mutate(datasets = add_links(datasets)) |>
  knitr::kable()


