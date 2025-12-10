# Datasets for categorical data analysis

The `vcdExtra` package contains 45 datasets, taken from the literature
on categorical data analysis, and selected to illustrate various methods
of analysis and data display. These are in addition to the 33 datasets
in the [vcd package](https://cran.r-project.org/package=vcd).

To make it easier to find those which illustrate a particular method,
the datasets in `vcdExtra` have been classified using method tags. This
vignette creates an “inverse table”, listing the datasets that apply to
each method. It also illustrates a general method for classifying
datasets in R packages.

``` r
library(dplyr)
library(tidyr)
library(readxl)
```

## Processing tags

Using the result of `vcdExtra::datasets(package="vcdExtra")` I created a
spreadsheet, `vcdExtra-datasets.xlsx`, and then added method tags.

``` r
dsets_tagged <- read_excel(here::here("inst", "extdata", "vcdExtra-datasets.xlsx"), 
                           sheet="vcdExtra-datasets")

dsets_tagged <- dsets_tagged |>
  dplyr::select(-Title, -dim) |>
  dplyr::rename(dataset = Item)

head(dsets_tagged)
## # A tibble: 6 × 3
##   dataset   class      tags                                
##   <chr>     <chr>      <chr>                               
## 1 Abortion  table      loglinear;logit;2x2                 
## 2 Accident  data.frame loglinear; glm; logistic            
## 3 AirCrash  data.frame reorder; ca                         
## 4 Alligator data.frame loglinear;multinomial;zeros         
## 5 Bartlett  table      2x2;loglinear; homogeneity;oddsratio
## 6 Burt      data.frame ca
```

To invert the table, need to split tags into separate observations, then
collapse the rows for the same tag.

``` r
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
unique(tag_dset$tag)
##  [1] "2x2"         "agree"       "binomial"    "ca"          "glm"        
##  [6] "homogeneity" "lm"          "logistic"    "logit"       "loglinear"  
## [11] "mobility"    "multinomial" "oddsratio"   "one-way"     "ordinal"    
## [16] "poisson"     "reorder"     "square"      "zeros"
```

## Make this into a nice table

Another sheet in the spreadsheet gives a more descriptive `topic` for
corresponding to each tag.

``` r
tags <- read_excel(here::here("inst", "extdata", "vcdExtra-datasets.xlsx"), 
                   sheet="tags")
head(tags)
## # A tibble: 6 × 2
##   tag         topic                     
##   <chr>       <chr>                     
## 1 2x2         2 by 2 tables             
## 2 agree       observer agreement        
## 3 binomial    binomial distributions    
## 4 ca          correspondence analysis   
## 5 glm         generalized linear models 
## 6 homogeneity homogeneity of association
```

Now, join this with the `tag_dset` created above.

``` r
tag_dset <- tag_dset |>
  dplyr::left_join(tags, by = "tag") |>
  dplyr::relocate(topic, .after = tag)

tag_dset |>
  dplyr::select(-tag) |>
  head()
## # A tibble: 6 × 2
##   topic                      datasets                                           
##   <chr>                      <chr>                                              
## 1 2 by 2 tables              Abortion; Bartlett; Heart                          
## 2 observer agreement         Mammograms                                         
## 3 binomial distributions     Geissler                                           
## 4 correspondence analysis    AirCrash; Burt; Draft1970table; Gilby; HospVisits;…
## 5 generalized linear models  Accident; Cormorants; DaytonSurvey; Donner; Draft1…
## 6 homogeneity of association Bartlett
```

### Add links to `help()`

We’re almost there. It would be nice if the dataset names could be
linked to their documentation. This function is designed to work with
the `pkgdown` site. There are different ways this can be done, but what
seems to work is a link to `../reference/{dataset}.html` Unfortunately,
this won’t work in the actual vignette.

``` r
add_links <- function(dsets, 
                      style = c("reference", "help", "rdrr.io"),
                      sep = "; ") {

  style <- match.arg(style)
  names <- stringr::str_split_1(dsets, sep)

  names <- dplyr::case_when(
    style == "help"      ~ glue::glue("[{names}](help({names}))"),
    style == "reference" ~ glue::glue("[{names}](../reference/{names}.html)"),
    style == "rdrr.io"   ~ glue::glue("[{names}](https://rdrr.io/cran/vcdExtra/man/{names}.html)")
  )  
  glue::glue_collapse(names, sep = sep)
}
```

## Make the table

Use [`purrr::map()`](https://purrr.tidyverse.org/reference/map.html) to
apply `add_links()` to all the datasets for each tag.
(`mutate(datasets = add_links(datasets))` by itself doesn’t work.)

``` r
tag_dset |>
  dplyr::select(-tag) |>
  dplyr::mutate(datasets = purrr::map(datasets, add_links)) |>
  knitr::kable()
```

| topic | datasets |
|:---|:---|
| 2 by 2 tables | [Abortion](https://friendly.github.io/vcdExtra/reference/Abortion.md); [Bartlett](https://friendly.github.io/vcdExtra/reference/Bartlett.md); [Heart](https://friendly.github.io/vcdExtra/reference/Heart.md) |
| observer agreement | [Mammograms](https://friendly.github.io/vcdExtra/reference/Mammograms.md) |
| binomial distributions | [Geissler](https://friendly.github.io/vcdExtra/reference/Geissler.md) |
| correspondence analysis | [AirCrash](https://friendly.github.io/vcdExtra/reference/AirCrash.md); [Burt](https://friendly.github.io/vcdExtra/reference/Burt.md); [Draft1970table](https://friendly.github.io/vcdExtra/reference/Draft1970table.md); [Gilby](https://friendly.github.io/vcdExtra/reference/Gilby.md); [HospVisits](https://friendly.github.io/vcdExtra/reference/HospVisits.md); [HouseTasks](https://friendly.github.io/vcdExtra/reference/HouseTasks.md); [Mental](https://friendly.github.io/vcdExtra/reference/Mental.md) |
| generalized linear models | [Accident](https://friendly.github.io/vcdExtra/reference/Accident.md); [Cormorants](https://friendly.github.io/vcdExtra/reference/Cormorants.md); [DaytonSurvey](https://friendly.github.io/vcdExtra/reference/DaytonSurvey.md); [Donner](https://friendly.github.io/vcdExtra/reference/Donner.md); [Draft1970table](https://friendly.github.io/vcdExtra/reference/Draft1970table.md); [GSS](https://friendly.github.io/vcdExtra/reference/GSS.md); [ICU](https://friendly.github.io/vcdExtra/reference/ICU.md); [PhdPubs](https://friendly.github.io/vcdExtra/reference/PhdPubs.md) |
| homogeneity of association | [Bartlett](https://friendly.github.io/vcdExtra/reference/Bartlett.md) |
| linear models | [Draft1970](https://friendly.github.io/vcdExtra/reference/Draft1970.md) |
| logistic regression | [Accident](https://friendly.github.io/vcdExtra/reference/Accident.md); [Donner](https://friendly.github.io/vcdExtra/reference/Donner.md); [ICU](https://friendly.github.io/vcdExtra/reference/ICU.md); [Titanicp](https://friendly.github.io/vcdExtra/reference/Titanicp.md) |
| logit models | [Abortion](https://friendly.github.io/vcdExtra/reference/Abortion.md); [Cancer](https://friendly.github.io/vcdExtra/reference/Cancer.md) |
| loglinear models | [Abortion](https://friendly.github.io/vcdExtra/reference/Abortion.md); [Accident](https://friendly.github.io/vcdExtra/reference/Accident.md); [Alligator](https://friendly.github.io/vcdExtra/reference/Alligator.md); [Bartlett](https://friendly.github.io/vcdExtra/reference/Bartlett.md); [Caesar](https://friendly.github.io/vcdExtra/reference/Caesar.md); [Cancer](https://friendly.github.io/vcdExtra/reference/Cancer.md); [Detergent](https://friendly.github.io/vcdExtra/reference/Detergent.md); [Dyke](https://friendly.github.io/vcdExtra/reference/Dyke.md); [Heckman](https://friendly.github.io/vcdExtra/reference/Heckman.md); [Hoyt](https://friendly.github.io/vcdExtra/reference/Hoyt.md); [JobSat](https://friendly.github.io/vcdExtra/reference/JobSat.md); [Mice](https://friendly.github.io/vcdExtra/reference/Mice.md); [TV](https://friendly.github.io/vcdExtra/reference/TV.md); [Titanicp](https://friendly.github.io/vcdExtra/reference/Titanicp.md); [Toxaemia](https://friendly.github.io/vcdExtra/reference/Toxaemia.md); [Vietnam](https://friendly.github.io/vcdExtra/reference/Vietnam.md); [Vote1980](https://friendly.github.io/vcdExtra/reference/Vote1980.md); [WorkerSat](https://friendly.github.io/vcdExtra/reference/WorkerSat.md) |
| mobility tables | [Glass](https://friendly.github.io/vcdExtra/reference/Glass.md); [Hauser79](https://friendly.github.io/vcdExtra/reference/Hauser79.md); [Mobility](https://friendly.github.io/vcdExtra/reference/Mobility.md); [Yamaguchi87](https://friendly.github.io/vcdExtra/reference/Yamaguchi87.md) |
| multinomial models | [Alligator](https://friendly.github.io/vcdExtra/reference/Alligator.md) |
| odds ratios | [Bartlett](https://friendly.github.io/vcdExtra/reference/Bartlett.md); [Fungicide](https://friendly.github.io/vcdExtra/reference/Fungicide.md) |
| one-way tables | [CyclingDeaths](https://friendly.github.io/vcdExtra/reference/CyclingDeaths.md); [Depends](https://friendly.github.io/vcdExtra/reference/Depends.md); [ShakeWords](https://friendly.github.io/vcdExtra/reference/ShakeWords.md) |
| ordinal variables | [Draft1970table](https://friendly.github.io/vcdExtra/reference/Draft1970table.md); [Gilby](https://friendly.github.io/vcdExtra/reference/Gilby.md); [HairEyePlace](https://friendly.github.io/vcdExtra/reference/HairEyePlace.md); [Hauser79](https://friendly.github.io/vcdExtra/reference/Hauser79.md); [HospVisits](https://friendly.github.io/vcdExtra/reference/HospVisits.md); [JobSat](https://friendly.github.io/vcdExtra/reference/JobSat.md); [Mammograms](https://friendly.github.io/vcdExtra/reference/Mammograms.md); [Mental](https://friendly.github.io/vcdExtra/reference/Mental.md); [Mice](https://friendly.github.io/vcdExtra/reference/Mice.md); [Mobility](https://friendly.github.io/vcdExtra/reference/Mobility.md); [Yamaguchi87](https://friendly.github.io/vcdExtra/reference/Yamaguchi87.md) |
| Poisson distributions | [Cormorants](https://friendly.github.io/vcdExtra/reference/Cormorants.md); [PhdPubs](https://friendly.github.io/vcdExtra/reference/PhdPubs.md) |
| reordering values | [AirCrash](https://friendly.github.io/vcdExtra/reference/AirCrash.md); [Glass](https://friendly.github.io/vcdExtra/reference/Glass.md); [HouseTasks](https://friendly.github.io/vcdExtra/reference/HouseTasks.md) |
| square tables | [Glass](https://friendly.github.io/vcdExtra/reference/Glass.md); [Hauser79](https://friendly.github.io/vcdExtra/reference/Hauser79.md); [Mobility](https://friendly.github.io/vcdExtra/reference/Mobility.md); [Yamaguchi87](https://friendly.github.io/vcdExtra/reference/Yamaguchi87.md) |
| zero counts | [Alligator](https://friendly.github.io/vcdExtra/reference/Alligator.md); [Caesar](https://friendly.github.io/vcdExtra/reference/Caesar.md); [PhdPubs](https://friendly.github.io/vcdExtra/reference/PhdPubs.md); [Vote1980](https://friendly.github.io/vcdExtra/reference/Vote1980.md) |

Voila!
