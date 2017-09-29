# keep rgl from popping up windows
Sys.setenv(RGL_USE_NULL = TRUE)

#it is necessary to build manually, using

Sys.setenv(R_GSCMD="C:/Program Files/gs/gs9.21/bin/gswin64c.exe")
devtools::build(args = c('--resave-data','--compact-vignettes="gs+qpdf"'))

# then, test with win builder
args = c('--resave-data','--compact-vignettes="gs+qpdf"')
devtools::build_win(args=args)

# submit to cran
devtools::submit_cran(args=args)
