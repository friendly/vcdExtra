# To run this in the background
#   rstudioapi::jobRunScript(".revdep_run.R")

revdepcheck::revdep_reset()
revdepcheck::revdep_check(num_workers = 4)
