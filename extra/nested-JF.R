# Nested logit models
# 2023-03-01 J. Fox
# MF: add some documentation, add coef() method
# MF: set na.action=na.exclude & add predict() method

#' Nested Dichotomies Logit Models for Polytomous Response
#' 
#' This function constructs and fits the set of models for nested dichotomies
#' among the categories of a polytomous response. For a response variable with
#' \code{m} categories, a set of \code{m-1} logit models for these categories
#' fully describes the polytomous response.
#'
#' @param formula      The model formula for the polytomous response
#' @param dichotomies  A list of lists describing the nested dichotomies. see Details
#' @param data         The data
#' @param ...          Other arguments, passed to \code{glm}
#'
#' @return   An object of class \code{c("nested", "glmlist")}. It is a list of the glm() models fit to the
#'           nested dichotomies.
#' @export
#'
#' @examples
#' data(Womenlf, package = "carData")
#' m <- nestedLogit(partic ~ hincome + children, 
#'                  list(work=list(c("fulltime", "parttime"), "not.work"),
#'                       full=list("fulltime", "parttime")),
#'            
#'                  data=Womenlf)
#'
#' m
#' summary(m)
#' Anova(m)

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
  formula[[2]] <- quote(..y)      # is it possible to use the resp.names instead of ..y?
  for (i in 1L:n.dichot){
    data$..y <- ys[, i]
    models[[i]] <- glm(formula, family=binomial, 
                       data=data, 
                       na.action = na.exclude,   # pad with NAs
                       ...)
    form <- models[[i]]$formula
    form[[2]] <- as.symbol(resp.names[i])
    models[[i]]$formula <- form
    call <- models[[i]]$call
    call$formula <- form
    call$data <- data.name
    models[[i]]$call <- call
  }
  class (models) <- c("nested", "glmlist")
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
  class(result) <- "summary.nested"
  attr(result, "formula") <- attr(object, "formula")
  result
}

print.summary.nested <- function(x, ...){
  cat("Nested logit models: ")
  print(attr(x, "formula"))
  cat("\n")
  nms <- names(x)
  for (i in 1L:length(x)){
    cat("Response: ", nms[i])
    print(x[[i]], ...)
  }
  invisible(return(x))
}
  
Anova.nested <- function(mod, ...){
  result <- lapply(mod, Anova)
  nms <- names(mod)
  heading <- attr(result[[1L]], "heading")[1L]
  heading <- sub("Table", "Tables", heading)
  for (i in 1L:length(result)){
    attr(result[[i]], "heading") <- paste("Response:", nms[i])
  }
  attr(result, "heading") <- heading
  class(result) <- "Anova.nested"
  result
}

print.Anova.nested <- function(x, ...){
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

#' Coefficient method for nested objects
#' 
coef.nested <- function(object, as.matrix=TRUE, ...) {
  result <- if(as.matrix) sapply(object, coef, ...)
  else lapply(object, coef, ...)
  result
}

#' Predict method
#' When na.action=na.exclude, predict.glm() will ignore the NA cases
#' in the various dichotomies, but not remove them. 
#' It should be possible to have predict generate predicted values for these
#' cases, but this does not work.
#' 
predict.nested <- function(object, 
                           newdata = NULL,
                           type = c("link", "response", "terms"),
                           # se.fit = FALSE, dispersion = NULL, terms = NULL,
                           # na.action = na.pass, 
                           ...) {
  if (is.null(newdata)) {
    newdata <- object$data
#    newdata <- newdata[, !names(newdata) %in% "..y"]
  }
  result <- lapply(object, predict, newdata=newdata, type=type, ...)
  result  
}

# examples:

library(car)

m <- nestedLogit(partic ~ hincome + children, 
                    list(work=list(c("fulltime", "parttime"), "not.work"),
                         full=list("fulltime", "parttime")),
                    data=Womenlf)
m
summary(m)
Anova(m)

m2 <- nestedLogit(partic ~ log(hincome)*children, 
                 list(work=list(c("fulltime", "parttime"), "not.work"),
                      full=list("fulltime", "parttime")),
                 data=Womenlf)
m2
summary(m2)
Anova(m2)

# test fitted values
m.fit <- predict(m)
sapply(m.fit, length)
str(as.data.frame(m.fit))
