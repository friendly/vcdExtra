# begin to classify vcdExtra datasets
library(dplyr)
library(tidyr)

dsets <- datasets("vcdExtra") |> select("Item", "Title", "class", "dim")

write.csv(dsets, file = "extra/vcdExtra-datasets.csv")

# classify them in the equivalent xlsx file ...

library(readxl)
dsets_tagged <- read_excel("extra/vcdExtra-datasets.xlsx")

dsets_tagged <- dsets_tagged |>
  dplyr::select(-Title, -dim) |>
  dplyr::rename(dataset = Item)
str(dsets_tagged)

# to invert the table, need to split tags into separate observations ???

dset_split <- dsets_tagged |>
  tidyr::separate_longer_delim(tags, delim = ";") |>
  dplyr::mutate(tag = stringr::str_trim(tags)) |>
  select(-tags)

# collapse the rows for the same tag

tag_dset <- dset_split |>
  arrange(tag) |>
  dplyr::group_by(tag, dataset) |>
  dplyr::summarise(tag = paste(tag, collapse = ";"))


  
