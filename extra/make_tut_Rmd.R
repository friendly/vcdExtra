# library(spMisc)


## this doesn't work very well.
source("C:/Users/friendly/Dropbox/R/functions/rnw2rmd.R")

vcd_tut_Rmd <-
rnw2rmd(
  "vignettes/vcd-tutorial.Rnw",
  output = "rmarkdown::html_vignette",
  output_options = c(toc = "true"),
  engine = "knitr::rmarkdown",
  csl = "vignettes-new/apa.csl"
)

writeLines(vcd_tut_Rmd,
           "vignettes-new/vcd-tutorial.Rmd")


# works better, with my fixes
source("C:/Users/friendly/Dropbox/R/functions/latex2Rmd.R")


latex2Rmd(input = "vignettes/vcd-tutorial.Rnw", 
          output = "vignettes-new/vcd-tutorial.Rmd")

text <- "\\section[Introduction]{Introduction}\\label{sec:intro}"
txt2 <- "\\section{Introduction}\\label{sec:intro}"

str_replace_all(string = text, pattern = '\\\\section.*?\\{(.*?)\\}', replacement = '# \\1')

str_replace_all(string = txt2, pattern = '\\\\section(\\[.*\\])?\\*?\\{(.*?)\\}', replacement = '# \\2')

text <- "

\\section{Introduction}\\label{sec:intro}

\\section[Subtopic1]{Another topic, with a short TOC string}
Some text ... followed by a code chunk

<<preliminaries, echo=FALSE, results=hide>>=
set.seed(1071)
library(ggplot2)
\\@

More text here ...
"

library(stringr)

# Doesn't work
cat(str_replace_all(string = text, pattern = '<<(.*?)>>=(.*?)@', replacement = '```{r, \\1}\\2```'))

# why does this not swallow the `@`
cat(str_replace_all(string = text, pattern = '<<(.*?)>>=(.*?)', replacement = '```{r, \\1}\\2'))

cat(str_replace_all(string = text, pattern = '<<(.*?)>>=', replacement = '```{r, \\1}'))





