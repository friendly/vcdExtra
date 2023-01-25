# begin to classify vcdExtra datasets
library(dplyr)

dsets <- datasets("vcdExtra") |> select("Item", "Title", "class", "dim")

write.csv(dsets, file = "extra/vcdExtra-datasets.csv")
