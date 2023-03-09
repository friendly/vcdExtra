library(dplyr)
Titanicp <- Titanicp |>
  mutate(sibspF = case_match(sibsp,
                            0 ~ "0",
                            1 ~ "1",
                            2:max(sibsp) ~ "2+")) |>
  mutate(sibspF = ordered(sibspF)) |>
  mutate(parchF = case_match(parch,
                             0 ~ "0",
                             1 ~ "1",
                             2:max(parch) ~ "2+")) |>
  mutate(parchF = ordered(parchF)) 
  
# before
table(Titanicp$sibsp, Titanicp$parch)
# after
table(Titanicp$sibspF, Titanicp$parchF)
