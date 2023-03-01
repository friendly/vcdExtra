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
  dplyr::select(-tags)

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
#' This function is designed to work with the `pkgdown` site, 
#' but it isn't clear what the link should be.
#' style = "reference" will work with `pkgdown` but not in a vignette
#' style = "rdrr.io" ??
add_links <- function(dsets, 
                      style = c("reference", "help", "rdrr.io"),
                      sep = "; ") {

  style <- match.arg(style)
  names <- stringr::str_split_1(dsets, sep)
  # if(style == "help")
  #   names <- glue::glue("[{names}](help({names}))")
  # else
  #   names <- glue::glue("[{names}](reference/{names}.html))")

  names <- dplyr::case_when(
    style == "help"      ~ glue::glue("[{names}](help({names}))"),
    style == "reference" ~ glue::glue("[{names}](reference/{names}.html))"),
    style == "rdrr.io"   ~ glue::glue("[{names}](https://rdrr.io/cran/vcdExtra/man/{names}.html)")
  )  
  glue::glue_collapse(names, sep = sep)
}

add_links("Bartlett; Fungicide")
add_links("Bartlett; Fungicide", style="ref")
add_links("Bartlett; Fungicide", style="rdrr")

purrr::map(tag_dset$datasets, add_links)

tag_dset |>
  dplyr::select(-tag) |>
#  dplyr::mutate(datasets = add_links(datasets)) |>
  dplyr::mutate(datasets = purrr::map(datasets, add_links)) |>
  knitr::kable()


