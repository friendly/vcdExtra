\name{ICU}
\Rdversion{1.1}
\alias{ICU}
\docType{data}
\title{Death in the ICU}
\description{
	The ICU data set consists of a sample of 200 subjects who were part of
a much larger study on survival of patients following admission to an adult
intensive care unit (ICU).  The major goal of this study was to develop a
logistic regression model to predict the probability of survival to hospital
discharge of these patients and to study the risk factors associated with 
ICU mortality.
}
\usage{data(ICU)}
\format{
  A data frame with 200 observations on the following 21 variables.
  \describe{
    \item{\code{id}}{Patient ID}
    \item{\code{died}}{Died before discharge: a factor with levels \code{No} \code{Yes}}
    \item{\code{age}}{Age: a numeric vector}
    \item{\code{sex}}{Sex: a factor with levels \code{Female} \code{Male}}
    \item{\code{race}}{Race: a factor with levels \code{Black} \code{Other} \code{White}}
    \item{\code{service}}{Service at admission: a factor with levels \code{Medical} \code{Surgery}}
    \item{\code{cancer}}{Cancer part of problem?: a factor with levels \code{No} \code{Yes}}
    \item{\code{renal}}{History of chronic renal?: a factor with levels \code{No} \code{Yes}}
    \item{\code{infect}}{Infection probable?: a factor with levels \code{No} \code{Yes}}
    \item{\code{cpr}}{CPR prior to ICU admission?: a factor with levels \code{No} \code{Yes}}
    \item{\code{systolic}}{Systolic blood pressure: a numeric vector}
    \item{\code{hrtrate}}{Heart rate: a numeric vector}
    \item{\code{previcu}}{Previous admit to ICU?: a factor with levels \code{No} \code{Yes}}
    \item{\code{admit}}{Type of admission: a factor with levels \code{Elective} \code{Emergency}}
    \item{\code{fracture}}{Fracture?: a factor with levels \code{No} \code{Yes}}
    \item{\code{po2}}{PO2 inital blood gas: a numeric vector}
    \item{\code{ph}}{pH inital blood gas: a factor with levels \code{<7.25} \code{>=7.25}}
    \item{\code{pco}}{PCO2 inital blood gas: a factor with levels \code{<=45} \code{>45}}
    \item{\code{bic}}{Bicarbonate inital blood: a numeric vector}
    \item{\code{creatin}}{Creatinine inital blood: a factor with levels \code{<=2} \code{>2}}
    \item{\code{coma}}{Consciousness at ICU: an ordered factor with levels \code{None} \code{Stupor} \code{Coma}}
  }
}
\details{
Data were collected at Baystate Medical Center in Springfield,
Massachusetts.
The clinical aspects of this study are described in Lemeshow, Teres, Avrunin,
and Pastides (1988).
}
\source{
Hosmer and Lemeshow, Applied Logistic Regression, Wiley, (1989).

Lemeshow, S., Teres, D., Avrunin, J. S., Pastides, H. (1988). Predicting
    the Outcome of Intensive Care Unit Patients. \emph{Journal of the American
  Statistical Association}, 83, 348-356.
}
\references{
Friendly, M. \emph{Visualizing Categorical Data}, Cary, NC: SAS Institute, 2000, Appendix B.4.

}
\examples{
data(ICU)
## maybe str(ICU) ; plot(ICU) ...
}
\keyword{datasets}
