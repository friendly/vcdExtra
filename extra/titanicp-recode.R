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

# how to do this with fct_collapse?
Titanicp$sibsp |> 
  as.character() |> 
  forcats::fct_count() |>
  forcats::fct_collapse(
    '0' = '0',
    '1' = '1',
    other = '2+'  
  )

Titanicp |>
  select(sibsp, parch) |>
  xtabs()
