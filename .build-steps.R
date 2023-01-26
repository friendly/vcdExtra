# keep rgl from popping up windows
Sys.setenv(RGL_USE_NULL = TRUE)

#it is no longer necessary to build manually, using --compact-vignettes=both
# no longer necessary to use gs to compact pdf vignettes
#Sys.setenv(R_GSCMD="C:/Program Files/gs/gs9.21/bin/gswin64c.exe")
#Sys.setenv(R_GSCMD="C:/Program Files/gs/gs9.53.3/bin/gswin64c.exe")

# Build the pkgdown site
pkgdown::build_site()

# Check package
devtools::check()
devtools::check_win_release()
devtools::check_win_devel()
devtools::check_rhub()

#args = c('--resave-data','--compact-vignettes=both')
devtools::build()

# then, test with win builder
devtools::check_win_devel()

devtools::check_rhub()

# reverse dependencies
devtools::revdep()
# [1] "aplore3" "catdata" "gnm"     "iarm"    "jmv" 

if (!require("revdepcheck")) remotes::install_github("r-lib/revdepcheck")
revdepcheck::revdep_check(num_workers = 4)

# build the pkgdown site
pkgdown::build_site()

# submit to cran
devtools::release(args=args)

