library(spMisc)

vcd_tut_Rmd <-
rnw2rmd(
  "vignettes/vcd-tutorial.Rnw",
  output = "rmarkdown::html_vignette",
  output_options = c(toc = "true"),
  engine = "knitr::rmarkdown",
  csl = NULL
)
