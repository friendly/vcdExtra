library(stringr)

fix_roxy <- function(input, replace=TRUE) {
#  if (missing(output)) output = paste0(tools::file_path_sans_ext(input), ".Rmd")
  
  # read the file as characters
  text <- readLines(input)
#  text <- readChar(input, file.info(input)$size)
  
  pat <- list(
    c(from = 'list\\(\"(.*)\"\\)',          to = '\\\\code{\\1}'),
    c(from = "#' \\\\item",             to = "#'   \\\\item")
  )
  
  
  for (thisPat in pat){
    text <- gsub(thisPat[["from"]], thisPat[["to"]], text, perl=TRUE)
  }
  
#  text
  if (replace) {
    file.rename(input, paste0(input, ".bak"))
    message("Re-writing ", input)
    writeLines(text, input, sep="\n")
  }
  else
    text
  
  
}

fix_roxy("R/datasets.R", replace=TRUE)

new <- fix_roxy("R/datasets.R", replace=FALSE)  
writeLines(new, "R/datasets-new.R", sep="\n")
