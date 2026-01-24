# Make nice tables of datasets, for use in documentation

library(dplyr)
library(tidyr)
library(glue)

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

## -----------------
# for pkgdown

# all data sets
dsn <- ds[,"Item"]
writeLines(paste0("      - ",dsn))

# functions
fns <- sort(getNamespaceExports("vcdExtra"))
writeLines(paste0("      - ",fns))

# make a data vignette with links to the help files

dfs %>%
  mutate(name = glue("[{name}](help({name}))")) %>%
  knitr::kable()

