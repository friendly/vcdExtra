library(dplyr)
library(tibble)
ex1_df <- tibble(
  value = letters[1:4],
  d1 = c(0, 0, 1, 1),
  d2 = c(0, 1, NA, NA),
  d3 = c(NA, NA, 0, 1)
)
ex1_df

ex1_list <- list(
  d1 = list(c("a", "b"), c("c", "d")),
  d2 = list("a", "b"),
  d3 =                  list("c", "d")
) 

ex1_tree <- '{ {"a", "b"}
               {"c", "d"}  
             }'


ex2_df <- tibble(
    value = letters[1:4],
    d1 = c(0,  1, 1, 1),
    d2 = c(NA, 0, 1, 1),
    d3 = c(NA, NA, 0, 1)
  )
ex2_df


ex2_list <- list(
  d1 = list("a", c("b", "c", "d")),
  d2 = list(    "b", c("c", "d")),
  d3 = list(         "c", "d")
)

ex2_tree < '{"a", 
                 {"b", 
                      {"c", 
                            "d"}}}'

