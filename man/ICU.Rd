\name{ICU}
\alias{ICU}
\docType{data}
\title{
ICU data set
}
\description{
The ICU data set consists of a sample of 200 subjects who were part of
a much larger study on survival of patients following admission to an adult
intensive care unit (ICU), derived from  
Hosmer, Lemeshow and Sturdivant (2013) and Friendly (2000).

The major goal of this study was to develop a
logistic regression model to predict the probability of survival to hospital
discharge of these patients and to study the risk factors associated with 
ICU mortality. The clinical details of the study are described in 
Lemeshow, Teres, Avrunin, and Pastides (1988).

This data set is often used to illustrate model selection methods for
logistic regression.
}
\usage{data(ICU)}
\format{
  A data frame with 200 observations on the following 22 variables.
  \describe{
%    \item{\code{id}}{Patient id code, a numeric vector}
    \item{\code{died}}{Died before discharge?, a factor with levels \code{No} \code{Yes}}
    \item{\code{age}}{Patient age, a numeric vector}
    \item{\code{sex}}{Patient sex, a factor with levels \code{Female} \code{Male}}
    \item{\code{race}}{Patient race, a factor with levels \code{Black} \code{Other} \code{White}.  Also represented here as \code{white}.}
    \item{\code{service}}{Service at ICU Admission, a factor with levels \code{Medical} \code{Surgical}}
    \item{\code{cancer}}{Cancer part of present problem?, a factor with levels \code{No} \code{Yes}}
    \item{\code{renal}}{History of chronic renal failure?, a factor with levels \code{No} \code{Yes}}
    \item{\code{infect}}{Infection probable at ICU admission?, a factor with levels \code{No} \code{Yes}}
    \item{\code{cpr}}{Patient received CPR prior to ICU admission?, a factor with levels \code{No} \code{Yes}}
    \item{\code{systolic}}{Systolic blood pressure at admission (mm Hg), a numeric vector}
    \item{\code{hrtrate}}{Heart rate at ICU Admission (beats/min), a numeric vector}
    \item{\code{previcu}}{Previous admission to an ICU within 6 Months?, a factor with levels \code{No} \code{Yes}}
    \item{\code{admit}}{Type of admission, a factor with levels \code{Elective} \code{Emergency}}
    \item{\code{fracture}}{Admission with a long bone, multiple, neck, single area, or hip fracture?
    a factor with levels \code{No} \code{Yes}}
    \item{\code{po2}}{PO2 from inital blood gases, a factor with levels \code{>60} \code{<=60}}
    \item{\code{ph}}{pH from inital blood gases, a factor with levels \code{>=7.25} \code{<7.25}}
    \item{\code{pco}}{PCO2 from inital blood gases, a factor with levels \code{<=45} \code{>45}}
    \item{\code{bic}}{Bicarbonate (HCO3) level from inital blood gases, a factor with levels \code{>=18} \code{<18}}
    \item{\code{creatin}}{Creatinine, from inital blood gases, a factor with levels \code{<=2} \code{>2}}
    \item{\code{coma}}{Level of unconsciousness at admission to ICU,	a factor with levels \code{None} \code{Stupor} \code{Coma}}
    \item{\code{white}}{a recoding of \code{race},  a factor with levels \code{White} \code{Non-white}}
    \item{\code{uncons}}{a recoding of \code{coma} a factor with levels \code{No} \code{Yes}}
  }
}
\details{
Patient ID numbers are the rownames of the data frame.

Note that the last two variables \code{white} and \code{uncons} are a recoding of
respectively \code{race} and \code{coma} to binary variables.
}

\source{
M. Friendly (2000), \emph{Visualizing Categorical Data}, Appendix B.4. SAS Institute, Cary, NC. 

Hosmer, D. W. Jr., Lemeshow, S. and Sturdivant, R. X. (2013) 
	\emph{Applied Logistic Regression}, NY: Wiley,
	 Third Edition.	
}

\references{

Lemeshow, S., Teres, D., Avrunin, J. S., Pastides, H. (1988). Predicting
    the Outcome of Intensive Care Unit Patients. \emph{Journal of the American
    Statistical Association}, 83, 348-356.
}
\examples{
data(ICU)
# remove redundant variables (race, coma)
ICU1 <- ICU[,-c(4,20)]

# fit full model
icu.full <- glm(died ~ ., data=ICU1, family=binomial)
summary(icu.full)

# simpler model (found from a "best" subsets procedure)
icu.mod1 <- glm(died ~ age + sex + cancer + systolic + admit + uncons, data=ICU1, family=binomial)
summary(icu.mod1)

# even simpler model
icu.mod2 <- glm(died ~ age + cancer  + admit + uncons, data=ICU1, family=binomial)
summary(icu.mod2)

anova(icu.mod2, icu.mod1, icu.full, test="Chisq")

## Reproduce Fig 6.12 from VCD

icu.fit <- data.frame(ICU, prob=predict(icu.mod2, type="response"))

# combine categorical risk factors to a single string
risks <- ICU[, c("cancer", "admit", "uncons")]
risks[,1] <- ifelse(risks[,1]=="Yes", "Cancer", "")
risks[,2] <- ifelse(risks[,2]=="Emergency", "Emerg", "")
risks[,3] <- ifelse(risks[,3]=="Yes", "Uncons", "")
risks <- apply(risks, 1, paste, collapse="")
risks[risks==""] <- "(none)"
icu.fit$risks <- risks

library(ggplot2)
ggplot(icu.fit, aes(x=age, y=prob, color=risks)) +
	geom_point(size=2) +
	geom_line(size=1.25, alpha=0.5) +
	theme_bw() + ylab("Probability of death")

}
\keyword{datasets}
