# try to make a simpler version of the Anova table?

Anova.nested <- function(mod, ...){
  result <- lapply(mod, Anova)
  nms <- names(mod)

  # extract the chisq, df for the dichotomies
  chisq <- vector(length(result), mode="list")
  df <- vector(length(result), mode="list")
  names(chisq) <- names(df) <- nms
  for (i in 1L:length(result)){
    chisq[[i]] <- result[[i]]$`LR Chisq`
    df[[i]] <- result[[i]]$`Df`
  }
  browser()
  heading <- attr(result[[1L]], "heading")[1L]
  heading <- sub("Table", "Tables", heading)
  for (i in 1L:length(result)){
    attr(result[[i]], "heading") <- paste("Response:", nms[i])
  }
  attr(result, "heading") <- heading
  class(result) <- "Anova.nested"
  result
}
