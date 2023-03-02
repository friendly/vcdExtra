# Nested logit models
# 2023-03-01 J. Fox
# MF: add some documentation, add coef() method

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
#' @return   An object of class \code{nested}. It is a list of the glm() models fit to the
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

#' m
#' summary(m)
#' Anova(m)


nestedLogit <- function(formula, dichotomies, data, ...){
  
  makeDichotomies <- function(y, dichotomies){
    responses <- matrix(NA, length(y), length(dichotomies))
    for (i in 1:length(dichotomies)){
      responses[y %in% dichotomies[[i]][[1]], i] <- 1
      responses[y %in% dichotomies[[i]][[2]], i] <- 0
    }
    colnames(responses) <- names(dichotomies)
    responses
  }
  
  cl <- match.call()
  mf <- match.call(expand.dots = FALSE)
  m <- match(c("formula", "data", "subset", "weights", "na.action", 
               "offset"), names(mf), 0L)
  mf <- mf[c(1L, m)]
  mf$drop.unused.levels <- TRUE
  mf[[1L]] <- quote(stats::model.frame)
  mf <- eval(mf, parent.frame())
  mt <- attr(mf, "terms")
  y <- model.response(mf)
  w <- as.vector(model.weights(mf))
  if (!is.null(w) && !is.numeric(w)) 
    stop("'weights' must be a numeric vector")
  offset <- model.offset(mf)
  ny <- length(y)
  if (!is.null(offset)) {
    offset <- as.vector(offset)
   if (NROW(offset) != ny) 
      stop(gettextf("number of offsets is %d, should equal %d (number of observations)", 
                    NROW(offset), ny), domain = NA)
  }
  n.dichot <- length(dichotomies)
  mf$..ys <- makeDichotomies(y, dichotomies)
  models <- vector(n.dichot, mode="list")
  resp.names <- names(dichotomies)
  names(models) <- resp.names
  formula[[2]] <- quote(..y)
  for (i in 1:n.dichot){
    mf$..y <- mf$..ys[, i]
    models[[i]] <- glm(formula, family=binomial, data=mf, ...)
    form <- models[[i]]$formula
    form[[2]] <- as.symbol(resp.names[i])
    models[[i]]$formula <- form
    call <- models[[i]]$call
    call$formula <- form
    call$data <- NULL
    models[[i]]$call <- call
  }
  # question: Each model is of class `glm`, `lm`, so should this also have a class of `glmlist`?
  # vcdExtra has some things for `glmlist` objects
  class (models) <- "nested"
  models
}

print.nested <- function(x, ...){
  lapply(x, print, ...)
  invisible(x)
}

summary.nested <- function(object, ...){
  result <- lapply(object, summary, ...)
  class(result) <- "summary.nested"
  result
}

print.summary.nested <- function(x, ...){
  cat("Nested Logit Models\n\n")
  nms <- names(x)
  for (i in 1:length(x)){
    cat("Response: ", nms[i])
    print(x[[i]], ...)
  }
  invisible(return(x))
}
  

Anova.nested <- function(mod, ...){
  result <- lapply(mod, Anova)
  nms <- names(mod)
  heading <- attr(result[[1]], "heading")[1]
  heading <- sub("Table", "Tables", heading)
  for (i in 1:length(result)){
    attr(result[[i]], "heading") <- paste("Response:", nms[i])
  }
  attr(result, "heading") <- heading
  class(result) <- "Anova.nested"
  result
}

print.Anova.nested <- function(x, ...){
  cat("\n", attr(x, "heading"), "\n")
  table <- print(x[[1]], ...)
  for (i in 2:length(x)){
    cat("\n\n")
    table <- table + print(x[[i]], ...)
  }
  table[, 3] <- pchisq(table[, 1], table[, 2], lower.tail=FALSE)
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

# example:


library(car)

m <- nestedLogit(partic ~ hincome + children, 
                    list(work=list(c("fulltime", "parttime"), "not.work"),
                         full=list("fulltime", "parttime")),
                    data=Womenlf)
m
summary(m)
Anova(m)
