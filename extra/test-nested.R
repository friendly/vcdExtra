# test representation of nested dichotomies

library(car)
library(dplyr)
data("Womenlf", package = "carData")

women_small <- Womenlf |>
  group_by(partic) |>
  sample_n(size=3) |>
  ungroup()

makeDichotomies <- function(y, dichotomies){
  responses <- matrix(NA, length(y), length(dichotomies))
  for (i in 1L:length(dichotomies)){
    responses[y %in% dichotomies[[i]][[1L]], i] <- 1L
    responses[y %in% dichotomies[[i]][[2L]], i] <- 0L
  }
  colnames(responses) <- names(dichotomies)
  responses
}


dichot_list <- list(work=list(c("fulltime", "parttime"), "not.work"),
                    full=list("fulltime", "parttime"))

with(women_small, makeDichotomies(partic, dichot_list))

if (!require(data.tree)) install.packages("data.tree")
library(data.tree)

tree <- FromListSimple(dichot_list)
tree
plot(tree)

# tree2 <- FromListExplicit(dichot_list)
# tree2
# plot(tree2)


ToDataFrameTree(tree)
