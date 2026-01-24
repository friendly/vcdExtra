#' ---
#' title: test nested dichotomies
#' ---
#' 

source("extra/nested.R")
library(car)
library(dplyr)
data(Womenlf, package = "carData")

#' ## Fit nested dichotomies 'by hand'

Womenlf <- Womenlf |>
  mutate(work = ifelse(partic=="not.work", 0, 1)) |>
  mutate(full = case_when(
    work & partic == "fulltime" ~ 1,
    work & partic == "parttime" ~ 0)
  )

mod.work <- glm(work ~ hincome + children, family=binomial, data=Womenlf)
mod.full <- glm(full ~ hincome + children, family=binomial, data=Womenlf)

#' ## Use nested()

mod.nested <- nestedLogit(partic ~ hincome + children, 
                 logits(work=dichotomy(c("fulltime", "parttime"), "not.work"),
                        full=dichotomy("fulltime", "parttime")),
                 data=Womenlf)

#' ## Compare coefficients
#' 
c.hand <- rbind(work = coef(mod.work),
          full = coef(mod.full))

c.nest <- t(coef(mod.nested))

all.equal(c.hand, c.nest)

#' ## Test predict()
#' 
new <- expand.grid(hincome=seq(0, 45, length=10), 
                    children=c("absent", "present"))

#' ## by hand

#' predictions for the two submodels
p.work     <- predict(mod.work, new, type='response')
p.fulltime <- predict(mod.full, new, type='response')

#' calculate unconditional probs for the three response categories

pred.hand <- data.frame(
  fulltime = p.work * p.fulltime,
  parttime = p.work * (1 - p.fulltime),
  not.work =  1 - p.work
)

#' ## `predict.nested()`
pred.nested <- predict(mod.nested, new)

#' ## Compare predictions
#' 
#' Naive compare fails because of attributes & also predict.nested returns a matrix
#' > all.equal(pred.hand, as.data.frame(pred.nested))
#' [1] "Attributes: < Component “row.names”: Modes: character, numeric >"              
#' [2] "Attributes: < Component “row.names”: target is character, current is numeric >"

all.equal(pred.hand, as.data.frame(pred.nested), 
          check.attributes = FALSE)



