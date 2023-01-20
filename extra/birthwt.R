data(birthwt, package="MASS")
# how to do this without attach?
attach(birthwt)
race = factor(race, labels = c("white", "black", "other"))
ptd = factor(ptl > 0)
ftv = factor(ftv)
levels(ftv)[-(1:2)] = "2+"
bwt <- data.frame(low = factor(low), age, lwt, race,
                  smoke = (smoke > 0), ptd, ht = (ht > 0), ui = (ui > 0), ftv)
detach(birthwt)

library(dplyr)

bwt2 <
birthwt |>
  mutate(race = factor(race, labels = c("white", "black", "other")),
         ptd = factor(ptl > 0),
         ftv = factor(ftv),
         levels(ftv)[-(1:2)] = "2+") |>
  mutate(low = factor(low),
         ht = (ht > 0), 
         ui = (ui > 0)) |>
  select(low, age, lwt, race, ht, ui, ftv)
