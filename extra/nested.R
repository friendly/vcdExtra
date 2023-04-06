# Nested logit models
# 2023-03-01 J. Fox
# 2023-04-06 MF: add coef method

nestedLogit <- function(formula, dichotomies, data, ...){
  
  makeDichotomies <- function(y, dichotomies){
    responses <- matrix(NA, length(y), length(dichotomies))
    for (i in 1L:length(dichotomies)){
      responses[y %in% dichotomies[[i]][[1L]], i] <- 1L
      responses[y %in% dichotomies[[i]][[2L]], i] <- 0L
    }
    colnames(responses) <- names(dichotomies)
    responses
  }
  
  data.name <- substitute(data)
  nested.formula <- formula
  y <- model.response(model.frame(formula, data))
  n.dichot <- length(dichotomies)
  ys <- makeDichotomies(y, dichotomies)
  models <- vector(n.dichot, mode="list")
  resp.names <- names(dichotomies)
  names(models) <- resp.names
  formula[[2]] <- quote(..y)
  for (i in seq(length=n.dichot)){
    data$..y <- ys[, i]
    models[[i]] <- glm(formula, family=binomial, data=data, ...)
    form <- models[[i]]$formula
    form[[2]] <- as.symbol(resp.names[i])
    models[[i]]$formula <- form
    call <- models[[i]]$call
    call$formula <- form
    call$data <- data.name
    models[[i]]$call <- call
    models[[i]]$dichotomy <- dichotomies[[i]]
  }
  class (models) <- "nested"
  attr(models, "formula") <- nested.formula
  models
}


print.nested <- function(x, ...){
  cat("Nested logit models: ")
  print(attr(x, "formula"))
  lapply(x, print, ...)
  invisible(x)
}

summary.nested <- function(object, ...){
  result <- lapply(object, summary, ...)
  for (i in 1:length(result)){
    result[[i]]$dichotomy <- object[[i]]$dichotomy
  }
  class(result) <- "summary.nested"
  attr(result, "formula") <- attr(object, "formula")
  result
}

print.summary.nested <- function(x, ...){
  cat("Nested logit models: ")
  print(attr(x, "formula"))
  cat("\n")
  nms <- names(x)
  for (i in seq(along=x)){
    cat(paste0("Response ", nms[i], ": ", paste(x[[i]]$dichotomy[[1]], collapse=", "), 
               " vs. ", paste(x[[i]]$dichotomy[[2]], collapse=", ")))
    print(x[[i]], ...)
  }
  invisible(return(x))
}
  
Anova.nested <- function(mod, ...){
  result <- lapply(mod, Anova)
  nms <- names(mod)
  heading <- attr(result[[1L]], "heading")[1L]
  heading <- sub("Table", "Tables", heading)
  for (i in seq(along=result)){
    attr(result[[i]], "heading") <- paste("Response:", nms[i])
  }
  attr(result, "heading") <- heading
  class(result) <- "Anova.nested"
  result
}

print.Anova.nested <- function(x, ...){
  if (length(x) < 2) {
    return(invisible(print(x[[1]], ...)))
  }
  cat("\n", attr(x, "heading"), "\n")
  table <- print(x[[1L]], ...)
  for (i in 2L:length(x)){
    cat("\n\n")
    table <- table + print(x[[i]], ...)
  }
  table[, 3L] <- pchisq(table[, 1L], table[, 2L], lower.tail=FALSE)
  attr(table, "heading") <- "Combined Responses"
  class(table) <- c("anova", "data.frame")
  cat("\n\n")
  print(table)
  invisible(x)
}

logits <- function(...){
  logits <- list(...)
  nms <- names(logits)
  if (is.null(nms) || any(nms == "")) stop("logits must be named")
  logits
}

dichotomy <- function(...){
  logit <- list(...)
  if (length(logit) != 2) stop("dichotomy must define two categories")
  logit
}

predict.nested <- function(object, newdata, ...){
  if (missing(newdata)) newdata <- object[[1]]$data
  ndichot <- length(object)
  fitted <- vector(ndichot, mode="list")
  for (i in 1:ndichot){
    p <- predict(object[[i]], newdata=newdata, type="response")
    p <- cbind(p, 1 - p)
    attr(p, "columns") <- object[[i]]$dichotomy
    fitted[[i]] <- p
  }
  response.levels <- unique(unlist(lapply(fitted, function(x) attr(x, "columns"))))
  p <- matrix(1, nrow(newdata), length(response.levels))
  colnames(p) <- response.levels
  for (level in response.levels){
    for (i in 1:ndichot){
      which <- sapply(object[[i]]$dichotomy, function(x) level %in% x)
      if (!any(which)) next
      p[, level] <- p[, level]*fitted[[i]][, which]
    }
  }
  p
}

#' Coefficient method for nested objects
#' 
coef.nested <- function(object, as.matrix=TRUE, ...) {
  result <- if(as.matrix) sapply(object, coef, ...)
  else lapply(object, coef, ...)
  result
}

