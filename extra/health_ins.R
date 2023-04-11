#' ---
#' title: Choice of health insurance product
#' ---

library(dplyr)
library(nnet)
library(car)
library(dplyr)
library(ggplot2)
library(directlabels)

# health insurance example: Choice of insurance product 
# from https://peopleanalytics-regression-book.org/multinomial-logistic-regression-for-nominal-category-outcomes.html
# This example has been modified to create a 4-level response

url <- "https://peopleanalytics-regression-book.org/data/health_insurance.csv"
HealthInsurance <- read.csv(url)


#' Make a new variable with 4 levels
HealthInsurance$product4 <- HealthInsurance$product
change <- which(runif(nrow(HealthInsurance)) > .75)
HealthInsurance[change, "product4"] <- "D"

# remove gender = non-binary
nb <- which(HealthInsurance$gender == "Non-binary")
HealthInsurance <- HealthInsurance[-nb, ]

# make factors
HealthInsurance <- HealthInsurance |>
  relocate(product4, .after = product) |>
  mutate(gender = as.factor(gender),
         product = as.factor(product),
         product4 = as.factor(product4))

str(HealthInsurance)

table(HealthInsurance$product)
table(HealthInsurance$product4)
table(HealthInsurance$gender)

save(HealthInsurance, file = "extra/HealthInsurance.RData")
prompt(HealthInsurance, file="extra/HealthInsurance.Rd")


health.multi <- multinom(product4 ~ age + gender + household + position_level, 
                         data = HealthInsurance)

Anova(health.multi)

health.multi2 <- multinom(product4 ~ age  + gender * household + position_level,
                         data = HealthInsurance)

Anova(health.multi2)


new <- expand.grid(age=seq(20, 70, by = 5), 
                   gender = c("Female", "Male"),
                   household = mean(HealthInsurance$household))

fit <-data.frame(new,
                 predict(health.multi, newdata=new, type='probs'))

plotdat <- fit |>
  tidyr::gather(key="Level", value="Probability", A:D)

gg <- ggplot(plotdat, aes(x = age, y = Probability, colour= Level)) +
  geom_line(size=1.5) + 
  facet_grid(~ gender, labeller= label_both)

direct.label(gg, list("top.bumptwice", dl.trans(y = y + 0.2)))

#' ## Fit nested logit model 
#' 

health.nested <- nestedLogit()