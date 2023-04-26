# convert to roxygen docs

library(Rd2roxygen)
Rd2roxygen(getwd(), nomatch = "data.R")

## tidy up files

# generate new man files
devtools::document()
