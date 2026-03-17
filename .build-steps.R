# keep rgl from popping up windows
Sys.setenv(RGL_USE_NULL = TRUE)

# The README.Rmd references the vignettes, so they must be installed first
devtools::install(build_vignettes = TRUE)

# build the README.md if it is older than README.Rmd
if (!file.exists("README.md") || file.mtime("README.Rmd") > file.mtime("README.md")) {
  devtools::build_readme()
}

# spellcheck -- fix any issues before running remote checks
(words <- devtools::spell_check())
words$word

# Check package locally
devtools::check()

# Check on win-builder (CRAN's Windows servers)
devtools::check_win_release()
devtools::check_win_devel()

# Check on R-hub (Linux, macOS, Windows R-devel)
#devtools::check_rhub()
rhub::rhub_check(platforms = c("linux", "macos-arm64", "windows"))   # removed: "macos"

# reverse dependencies
devtools::revdep()
# [1] "aplore3" "catdata" "gnm"     "iarm"    "jmv"

# revdepcheck::revdep_reset()
# revdepcheck::revdep_check(num_workers = 4)

# To run revdep check in the background
rstudioapi::jobRunScript(".revdep_run.R")

# build the pkgdown site (also triggered automatically by GHA on push to master)
pkgdown::build_site()

# submit to cran
devtools::release()
