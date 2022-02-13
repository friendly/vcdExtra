# Make nice tables of datasets, for use in documentation

library(dplyr)
library(tidyr)

ds <- vcdExtra::datasets("vcdExtra")

dim <- ds$dim

dfs <- ds %>%
  filter(class == "data.frame") %>%
  select(-class) %>%
  rename(name = Item,
         title = Title) %>%
  mutate(title = stringr::str_to_sentence(title)) %>%
  tidyr::separate(dim, into = c("rows", "cols"), sep = "x", convert = TRUE)

knitr::kable(dfs)

tabs <- ds %>%
  filter(class %in% c("table", "array")) %>%
  select(-class) %>%
  rename(name = Item,
         title = Title) %>%
  mutate(title = stringr::str_to_sentence(title))
#  tidyr::separate(dim, into = c("rows", "cols"), sep = "x", convert = TRUE) %>%
  
knitr::kable(tabs)

