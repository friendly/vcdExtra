# keep rgl from popping up windows
Sys.setenv(RGL_USE_NULL = TRUE)

#it is necessary to build manually, using

#Sys.setenv(R_GSCMD="C:/Program Files/gs/gs9.21/bin/gswin64c.exe")
Sys.setenv(R_GSCMD="C:/Program Files/gs/gs9.53.3/bin/gswin64c.exe")

args = c('--resave-data','--compact-vignettes=both')
devtools::build(args = args)

# then, test with win builder
devtools::check_win_devel(args=args)

devtools::check_rhub()

# reverse dependencies
devtools::revdep()
# [1] "aplore3" "catdata" "gnm"     "iarm"    "jmv" 

if (!require("revdepcheck")) remotes::install_github("r-lib/revdepcheck")
revdepcheck::revdep_check(num_workers = 4)

# submit to cran
devtools::release(args=args)

# Warning: 'inst/doc' file
# 'vcd-tutorial.pdf'
# ignored as vignettes have been rebuilt.
# Run R CMD build with --no-build-vignettes to prevent rebuilding.
