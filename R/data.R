

#' Abortion Opinion Data
#'
#' Opinions about abortion classified by gender and SES
#'
#' \code{Support_Abortion} is a natural response variable.
#'
#' The combinations of \code{Sex} and \code{Status} represent four independent
#' samples, having fixed \code{Sex}-\code{Status} marginal totals.  There were
#' 500 females and 600 males. Within the female group, 250 of low status and
#' 250 of high status were sampled. Similarly for the males, with 300 in each
#' of the low and hgh status sub-groups.
#'
#' This is an example of a product-multinomial sampling scheme. the
#' \code{Sex:Status} association must be included in any loglinear model where
#' the goal is to determine how attitude toward abortion depends on the others.
#'
#' Alternatively, a logit model for abortion support may provide a simpler
#' analysis.
#'
#' @name Abortion
#' @docType data
#' @format A 3-dimensional array resulting from cross-tabulating 3 variables
#' for 1100 observations. The variable names and their levels are:
#'
#' \tabular{rll}{
#'   No \tab Name                    \tab Levels \cr
#'   1  \tab \code{Sex}              \tab \code{"Female", "Male"}\cr
#'   2  \tab \code{Status}           \tab \code{"Lo", "Hi"}\cr
#'   3  \tab \code{Support_Abortion} \tab \code{"Yes", "No"}\cr
#' }
#'
#' @source
#'
#' Christensen, R. (1990).  \emph{Log-Linear Models}, New York, NY: Springer-Verlag, p. 92, Example 3.5.2.
#'
#' Christensen, R. (1997).  \emph{Log-Linear Models and Logistic Regression},
#' New York, NY: Springer, p. 100, Example 3.5.2.
#' @keywords datasets
#' @examples
#'
#' data(Abortion)
#'
#'
#' ftable(Abortion)
#' mosaic(Abortion, shade=TRUE)
#'
#' # stratified by Sex
#' fourfold(aperm(Abortion, 3:1))
#' # stratified by Status
#' fourfold(aperm(Abortion, c(3,1,2)))
#'
#'
NULL





#' Traffic Accident Victims in France in 1958
#'
#' Bertin (1983) used these data to illustrate the cross-classification of data
#' by numerous variables, each of which could have various types and could be
#' assigned to various visual attributes.
#'
#' For modeling and visualization purposes, the data can be treated as a 4-way
#' table using loglinear models and mosaic displays, or as a frequency-weighted
#' data frame using a binomial response for \code{result} (\code{"Died"} vs.
#' \code{"Injured"}) and plots of predicted probabilities.
#'
#' \code{age} is an ordered factor, but arguably, \code{mode} should be treated
#' as ordered, with levels \code{Pedestrian} < \code{Bicycle} <
#' \code{Motorcycle} < \code{4-Wheeled} as Bertin does.  This affects the
#' parameterization in models, so we don't do this directly in the data frame.
#'
#' @name Accident
#' @docType data
#' @format A data frame in frequency form (comprising a 5 x 2 x 4 x 2 table)
#' with 80 observations on the following 5 variables.
#' \describe{
#'   \item{\code{age}}{an ordered factor with levels \code{0-9} < \code{10-19} <\code{20-29}
#'         < \code{30-49} < \code{50+}}
#'   \item{\code{result}}{a factor with levels \code{Died} \code{Injured}}
#'   \item{\code{mode}}{mode of transportation, a factor with levels \code{4-Wheeled} \code{Bicycle}
#'         \code{Motorcycle} \code{Pedestrian}}
#'   \item{\code{gender}}{a factor with levels \code{Female} \code{Male}}
#'   \item{\code{Freq}}{a numeric vector}
#' }
#'
#' @references Bertin, J. (1983), \emph{Semiology of Graphics}, University of
#' Wisconsin Press.
#' @source Bertin (1983), p. 30; original data from the Ministere des Travaux
#' Publics
#' @keywords datasets
#' @examples
#'
#' # examples
#' data(Accident)
#' head(Accident)
#'
#' # for graphs, reorder mode
#' Accident$mode <- ordered(Accident$mode,
#'    levels=levels(Accident$mode)[c(4,2,3,1)])
#'
#' # Bertin's table
#' accident_tab <- xtabs(Freq ~ gender + mode + age + result, data=Accident)
#' structable(mode + gender ~ age + result, data=accident_tab)
#'
#' ## Loglinear models
#' ## ----------------
#'
#' # mutual independence
#' acc.mod0 <- glm(Freq ~ age + result + mode + gender,
#'                 data=Accident,
#'                 family=poisson)
#' LRstats(acc.mod0)
#'
#' mosaic(acc.mod0, ~mode + age + gender + result)
#'
#' # result as a response
#' acc.mod1 <- glm(Freq ~ age*mode*gender + result,
#'                 data=Accident,
#'                 family=poisson)
#' LRstats(acc.mod1)
#'
#' mosaic(acc.mod1, ~mode + age + gender + result,
#'     labeling_args = list(abbreviate = c(gender=1, result=4)))
#'
#' # allow two-way association of result with each explanatory variable
#' acc.mod2 <- glm(Freq ~ age*mode*gender + result*(age+mode+gender),
#'                 data=Accident,
#'                 family=poisson)
#' LRstats(acc.mod2)
#' mosaic(acc.mod2, ~mode + age + gender + result,
#'     labeling_args = list(abbreviate = c(gender=1, result=4)))
#'
#' acc.mods <- glmlist(acc.mod0, acc.mod1, acc.mod2)
#' LRstats(acc.mods)
#'
#' ## Binomial (logistic regression) models for result
#' ## ------------------------------------------------
#' library(car)  # for Anova()
#' acc.bin1 <- glm(result=='Died' ~ age + mode + gender,
#'     weights=Freq, data=Accident, family=binomial)
#' Anova(acc.bin1)
#'
#' acc.bin2 <- glm(result=='Died' ~ (age + mode + gender)^2,
#'     weights=Freq, data=Accident, family=binomial)
#' Anova(acc.bin2)
#'
#' acc.bin3 <- glm(result=='Died' ~ (age + mode + gender)^3,
#'     weights=Freq, data=Accident, family=binomial)
#' Anova(acc.bin3)
#'
#' # compare models
#' anova(acc.bin1, acc.bin2, acc.bin3, test="Chisq")
#'
#' # visualize probability of death with effect plots
#' \dontrun{
#' library(effects)
#' plot(allEffects(acc.bin1), ylab='Pr (Died)')
#'
#' plot(allEffects(acc.bin2), ylab='Pr (Died)')
#' }
#'
#'
#' #
NULL





#' Air Crash Data
#'
#' Data on all fatal commercial airplane crashes from 1993--2015. Excludes
#' small planes (less than 6 passengers) and non-commercial (cargo, military,
#' private) aircraft.
#'
#' \code{Phase} of the flight was cleaned by combining related variants,
#' spelling, etc.
#'
#' @name AirCrash
#' @docType data
#' @format A data frame with 439 observations on the following 5 variables.
#' \describe{
#'   \item{\code{Phase}}{phase of the flight, a factor with levels \code{en route} \code{landing}
#'         \code{standing} \code{take-off}
#' \code{unknown}}
#'   \item{\code{Cause}}{a factor with levels \code{criminal} \code{human error} \code{mechanical}
#'         \code{unknown} \code{weather}}
#'   \item{\code{date}}{date of crash, a Date}
#'   \item{\code{Fatalities}}{number of fatalities, a numeric vector}
#'   \item{\code{Year}}{year, a numeric vector}
#' }
#' @references Rick Wicklin,
#' \url{http://blogs.sas.com/content/iml/2015/03/30/visualizing-airline-crashes.html}
#' @source Originally from David McCandless,
#' \url{https://informationisbeautiful.net/visualizations/plane-truth-every-single-commercial-plane-crash-visualized/},
#' with the data at
#' \url{https://docs.google.com/spreadsheets/d/1OvDq4_BtbR6nSnnHnjD5hVC3HQ-ulZPGbo0RDGbzM3Q/edit?usp=drive_web},
#' downloaded April 14, 2015.
#' @keywords datasets
#' @examples
#'
#' data(AirCrash)
#' aircrash.tab <- xtabs(~Phase + Cause, data=AirCrash)
#' mosaic(aircrash.tab, shade=TRUE)
#'
#' # fix label overlap
#' mosaic(aircrash.tab, shade=TRUE,
#'        labeling_args=list(rot_labels=c(30, 30, 30, 30)))
#'
#' # reorder by Phase
#' phase.ord <- rev(c(3,4,1,2,5))
#' mosaic(aircrash.tab[phase.ord,], shade=TRUE,
#'        labeling_args=list(rot_labels=c(30, 30, 30, 30)),
#'        offset_varnames=0.5)
#'
#' # reorder by frequency
#' phase.ord <- order(rowSums(aircrash.tab), decreasing=TRUE)
#' cause.ord <- order(colSums(aircrash.tab), decreasing=TRUE)
#' mosaic(aircrash.tab[phase.ord,cause.ord], shade=TRUE,
#'        labeling_args=list(rot_labels=c(30, 30, 30, 30)))
#'
#'
#' library(ca)
#' aircrash.ca <- ca(aircrash.tab)
#' plot(aircrash.ca)
#'
NULL





#' Alligator Food Choice
#'
#' The Alligator data, from Agresti (2002), comes from a study of the primary
#' food choices of alligators in four Florida lakes. Researchers classified the
#' stomach contents of 219 captured alligators into five categories: Fish (the
#' most common primary food choice), Invertebrate (snails, insects, crayfish,
#' etc.), Reptile (turtles, alligators), Bird, and Other (amphibians, plants,
#' household pets, stones, and other debris).
#'
#' The table contains a fair number of 0 counts.
#'
#' \code{food} is the response variable.  \code{fish} is the most frequent
#' choice, and often taken as a baseline category in multinomial response
#' models.
#'
#' @name Alligator
#' @docType data
#' @format A frequency data frame with 80 observations on the following 5
#' variables.
#' \describe{
#'   \item{\code{lake}}{a factor with levels \code{George} \code{Hancock} \code{Oklawaha} \code{Trafford}}
#'   \item{\code{sex}}{a factor with levels \code{female} \code{male}}
#'   \item{\code{size}}{alligator size, a factor with levels \code{large} (>2.3m) \code{small} (<=2.3m)}
#'   \item{\code{food}}{primary food choice, a factor with levels \code{bird} \code{fish}
#'         \code{invert} \code{other} \code{reptile}}
#'   \item{\code{count}}{cell frequency, a numeric vector} }
#' @source Agresti, A. (2002). \emph{Categorical Data Analysis}, New York:
#' Wiley, 2nd Ed., Table 7.1
#' @keywords datasets
#' @examples
#'
#' data(Alligator)
#'
#' # change from frequency data.frame to table
#' allitable <- xtabs(count ~ lake + sex + size + food, data=Alligator)
#' # Agresti's Table 7.1
#' structable(food ~ lake + sex + size, allitable)
#'
#'
#' plot(allitable, shade=TRUE)
#'
#' # mutual independence model
#' mosaic(~ food + lake + size, allitable, shade=TRUE)
#'
#' # food jointly independent of lake and size
#' mosaic(~ food + lake + size, allitable, shade=TRUE,
#'        expected = ~lake:size + food)
#'
#' if (require(nnet)) {
#' 	# multinomial logit model
#' 	mod1 <- multinom(food ~ lake + size + sex, data=Alligator, weights=count)
#' }
#'
#'
NULL





#' Effect of Exposure to Asbestos
#'
#' A two-way contingency table formed from the cross-classification of the
#' number of years of occupational exposure to asbestos and the diagnosed
#' severity of asbestosis of 1117 New York workers. Asbestosis is a chronic
#' lung disease that results in the lung tissue being scared due to contact
#' with the fibers which can lead to severe breathing difficulties.
#'
#' \code{exposure} and \code{grade} should be regarded as ordered factors. Beh
#' and Lombardo (2022) use this data to illustrate a polynomial biplot for
#' ordered categories.
#'
#' The data summarized here was studied by Beh and Smith (2011) and comes from
#' the original data collected and published by Selikoff (1981) who examined
#' the link between asbestos exposure and asbestosis severity in 1963.
#'
#' @name Asbestos
#' @docType data
#' @format The format is:
#' \preformatted{
#'  num [1:5, 1:4] 310 212 21 25 7 36 158 35 102 35 ...
#'  - attr(*, "dimnames")=List of 2
#'   ..$ exposure: chr [1:5] "0-9" "10-19" "20-29" "30-39" ...
#'   ..$ grade   : chr [1:4] "None" "Grade 1" "Grade 2" "Grade 3"#'
#'   }
#' @references Beh, E. J., and D. R. Smith (2011b), Real World Occupational
#' Epidemiology, Part 2: A Visual Interpretation of Statistical Significance,
#' \emph{Archives of Environmental & Occupational Health}, \bold{66}, 245-248.
#'
#' Selikoff, I. J. (1981), Household Risks With Inorganic Fibers,
#' \emph{Bulletin of the New York Academy of Medicine}, \bold{57}, 947-961.
#' @source Beh, E. J. & Lombardo, R. (2022). Features of the Polynomial Biplot
#' for Ordered Contingency Tables, \emph{Journal of Computational and Graphical
#' Statistics}, 31:2, 403-412, DOI: 10.1080/10618600.2021.1990773, Table 1.
#' @keywords datasets
#' @examples
#'
#' data(Asbestos)
#' # mosaic plot
#' vcd::mosaic(Asbestos, shade=TRUE, legend=FALSE)
#'
#' # do the correspondence analysis
#' library(ca)
#' Asbestos.ca <- ca(Asbestos)
#'
#' plot(Asbestos.ca, lines=TRUE)
#'
#'
NULL





#' Bartlett Data on Plum Root Cuttings
#'
#' In an experiment to investigate the effect of cutting length (two levels)
#' and planting time (two levels) on the survival of plum root cuttings, 240
#' cuttings were planted for each of the 2 x 2 combinations of these factors,
#' and their survival was later recorded.
#'
#' Bartlett (1935) used these data to illustrate a method for testing for no
#' three-way interaction in a contingency table.
#'
#'
#' @name Bartlett
#' @docType data
#' @format A 3-dimensional array resulting from cross-tabulating 3 variables
#' for 960 observations. The variable names and their levels are:
#'
#' \tabular{rll}{
#'  dim \tab Name          \tab Levels                \cr
#'    1  \tab \code{Alive}  \tab \code{"Alive", "Dead"}\cr
#'    2  \tab \code{Time}   \tab \code{"Now", "Spring"}\cr
#'    3  \tab \code{Length} \tab \code{"Long", "Short"}\cr
#' }

#'
#' @references
#' Bartlett, M. S. (1935).  Contingency Table Interactions \emph{Journal of the Royal Statistical Society}, Supplement,
#' 1935, 2, 248-252.
#'
#' @source
#'
#' Hand, D. and Daly, F. and Lunn, A. D.and McConway, K. J. and Ostrowski, E. (1994). \emph{A Handbook of Small Data Sets}.
#' London: Chapman & Hall, p. 15, # 19.
#' @keywords datasets
#' @examples
#'
#' data(Bartlett)
#'
#' # measures of association
#' assocstats(Bartlett)
#' oddsratio(Bartlett)
#'
#' # Test models
#'
#' ## Independence
#' MASS::loglm(formula = ~Alive + Time + Length, data = Bartlett)
#'
#' ## No three-way association
#' MASS::loglm(formula = ~(Alive + Time + Length)^2, data = Bartlett)
#'
#' # Use woolf_test() for a formal test of homogeneity of odds ratios
#' vcd::woolf_test(Bartlett)
#'
#'
#' # Plots
#' fourfold(Bartlett, mfrow=c(1,2))
#'
#' mosaic(Bartlett, shade=TRUE)
#' pairs(Bartlett, gp=shading_Friendly)
#'
NULL





#' Burt (1950) Data on Hair, Eyes, Head and Stature
#'
#' Cyril Burt (1950) gave these data, on a sample of 100 people from Liverpool,
#' to illustrate the application of a method of factor analysis (later called
#' multiple correspondence analysis) applied to categorical data.
#'
#' He presented these data initially in the form that has come to be called a
#' "Burt table", giving the univariate and bivariate frequencies for an n-way
#' frequency table.
#'
#' Burt says: "In all, 217 individuals were examined, about two-thirds of them
#' males. But, partly to simplify the calculations and partly because the later
#' observations were rather more trustworthy, I shall here restrict my analysis
#' to the data obtained from the last hundred males in the series."
#'
#' \code{Head} and \code{Stature} reflect a binary coding where people are
#' classified according to whether they are below or above the average for the
#' population.
#'
#' @name Burt
#' @docType data
#' @format A frequency data frame (representing a 3 x 3 x 2 x 2 frequency
#' table) with 36 cells on the following 5 variables.
#' \describe{
#'   \item{\code{Hair}}{hair color, a factor with levels \code{Fair} \code{Red} \code{Dark}}
#'   \item{\code{Eyes}}{eye color, a factor with levels \code{Light} \code{Mixed} \code{Dark}}
#'   \item{\code{Head}}{head shape, a factor with levels \code{Narrow} \code{Wide}}
#'   \item{\code{Stature}}{height, a factor with levels \code{Tall} \code{Short}}
#'   \item{\code{Freq}}{a numeric vector}
#' }
#'
#' @source Burt, C. (1950). The factorial analysis of qualitative data,
#' \emph{British Journal of Statistical Psychology}, \bold{3}(3), 166-185.
#' Table IX.
#' @keywords datasets
#' @examples
#'
#' data(Burt)
#' mosaic(Freq ~ Hair + Eyes + Head + Stature, data=Burt, shade=TRUE)
#'
#' #or
#' burt.tab <- xtabs(Freq ~ Hair + Eyes + Head + Stature, data=Burt)
#' mosaic(burt.tab, shade=TRUE)
#'
NULL





#' Risk Factors for Infection in Caesarian Births
#'
#' Data from infection from birth by Caesarian section, classified by
#' \code{Risk} (two levels), whether \code{Antibiotics} were used (two levels)
#' and whether the Caesarian section was \code{Planned} or not.  The outcome is
#' \code{Infection} (three levels).
#'
#' @details \code{Infection} is regarded as the response variable here.  There are quite
#' a few 0 cells here, particularly when \code{Risk} is absent and the
#' Caesarian section was unplanned. Should these be treated as structural or
#' sampling zeros?
#'
#' @name Caesar
#' @docType data
#' @format A 4-dimensional array resulting from cross-tabulating 4 variables
#' for 251 observations. The variable names and their levels are:
#'
#'   \tabular{rll}{
#'   dim \tab Name \tab Levels \cr
#'     1 \tab \code{Infection}\tab \code{"Type 1", "Type 2", "None"}\cr
#'     2 \tab \code{Risk}\tab \code{"Yes", "No"} (presence of risk factors)\cr
#'     3 \tab \code{Antibiotics}\tab \code{"Yes", "No"} (were antibiotics given?)\cr
#'     4 \tab \code{Planned}\tab \code{"Yes", "No"} (was the C section planned?)\cr
#'}
#'
#'
#' @seealso \code{\link[Fahrmeir]{caesar}} for the same data recorded as a
#' frequency data frame with other variables.
#' @source
#'
#' % \cite{Fahrmeir:94}
#' Fahrmeir, L. & Tutz, G. (1994). Multivariate
#' Statistical Modelling Based on Generalized Linear Models New York: Springer
#' Verlag, Table 1.1.
#' @keywords datasets
#' @examples
#'
#' data(Caesar)
#' #display table;  note that there are quite a few 0 cells
#' structable(Caesar)
#' require(MASS)
#'
#' # baseline model, Infection as response
#' Caesar.mod0 <- loglm(~Infection + (Risk*Antibiotics*Planned),
#'                      data=Caesar)
#'
#' # NB: Pearson chisq cannot be computed due to the 0 cells
#' Caesar.mod0
#'
#' mosaic(Caesar.mod0, main="Baseline model")
#'
#' # Illustrate handling structural zeros
#' zeros <- 0+ (Caesar >0)
#' zeros[1,,1,1] <- 1
#' structable(zeros)
#'
#' # fit model excluding possible structural zeros
#' Caesar.mod0s <- loglm(~Infection + (Risk*Antibiotics*Planned),
#'                       data=Caesar,
#' 	                    start=zeros)
#' Caesar.mod0s
#'
#' anova(Caesar.mod0, Caesar.mod0s, test="Chisq")
#'
#' mosaic (Caesar.mod0s)
#'
#' # what terms to add?
#' add1(Caesar.mod0, ~.^2, test="Chisq")
#'
#' # add Association of Infection:Antibiotics
#' Caesar.mod1 <- update(Caesar.mod0, ~ . + Infection:Antibiotics)
#' anova(Caesar.mod0, Caesar.mod1, test="Chisq")
#'
#' mosaic(Caesar.mod1,
#'        gp=shading_Friendly,
#'        main="Adding Infection:Antibiotics")
#'
#'
NULL





#' Survival of Breast Cancer Patients
#'
#' Three year survival of 474 breast cancer patients according to nuclear grade
#' and diagnostic center.
#'
#'
#' @name Cancer
#' @docType data
#' @format A 3-dimensional array resulting from cross-tabulating 3 variables
#' for 474 observations. The variable names and their levels are:
#'
#' \tabular{rll}{
#'   dim \tab Name \tab Levels \cr
#'     1\tab \code{Survival}\tab  \code{"Died", "Surv"}\cr
#'     2\tab \code{Grade}\tab \code{"Malignant", "Benign"}\cr
#'     3\tab \code{Center}\tab \code{"Boston", "Glamorgan"}\cr
#'     }
#' @source
#'
#' Lindsey, J. K. (1995).
#' Analysis of Frequency and Count Data Oxford, UK: Oxford University Press. p.
#' 38, Table 2.5.
#'
#' Whittaker, J. (1990) Graphical Models in Applied Multivariate Statistics New
#' York: John Wiley and Sons, p. 220.
#' @keywords datasets
#' @examples
#'
#' data(Cancer)
#'
#' MASS::loglm(~Survival + Grade + Center, data = Cancer)
#'
#' vcd::mosaic(Cancer, shade=TRUE)
#'
NULL





#' Advertising Behavior by Males Cormorants
#'
#' Male double-crested cormorants use advertising behavior to attract females
#' for breeding. In this study by Meagan McRae (2015), cormorants were observed
#' two or three times a week at six stations in a tree-nesting colony for an
#' entire season, April 10, 2014-July 10, 2014. The number of advertising birds
#' was counted and these observations were classified by characteristics of the
#' trees and nests.
#'
#' The goal is to determine how this behavior varies temporally over the season
#' and spatially, as well as with characteristics of nesting sites.
#'
#' Observations were made on only 2 days in weeks 3 and 4, but 3 days in all
#' other weeks. One should use log(days) as an offset, so that the response
#' measures rate.
#'
#' \code{Cormorants$days <- ifelse(Cormorants$week \%in\% 3:4, 2, 3)}
#'
#' @name Cormorants
#' @docType data
#' @format A data frame with 343 observations on the following 8 variables.
#'
#' \describe{
#'   \item{\code{category}}{Time of season, divided into 3 categories based on breeding chronology, an ordered factor with levels \code{Pre} < \code{Incubation} < \code{Chicks Present}}
#'   \item{\code{week}}{Week of the season}
#'   \item{\code{station}}{Station of observations on two different peninsulas in a park, a factor with levels \code{B1} \code{B2} \code{C1} \code{C2} \code{C3} \code{C4}}
#'   \item{\code{nest}}{Type of nest, an ordered factor with levels \code{no} < \code{partial} < \code{full}}
#'   \item{\code{height}}{Relative height of bird in the tree, an ordered factor with levels \code{low} < \code{mid} < \code{high}}
#'   \item{\code{density}}{Number of other nests in the tree, an ordered factor with levels \code{zero} < \code{few} < \code{moderate} < \code{high}}
#'   \item{\code{tree_health}}{Health of the tree the bird is advertising in, a factor with levels \code{dead} \code{healthy}}
#'   \item{\code{count}}{Number of birds advertising, a numeric vector}
#' }

#' @source McRae, M. (2015). Spatial, Habitat and Frequency Changes in
#' Double-crested Cormorant Advertising Display in a Tree-nesting Colony.
#' Unpublished MA project, Environmental Studies, York University.
#' @keywords datasets
#' @examples
#'
#' data(Cormorants)
#' str(Cormorants)
#'
#' if (require("ggplot2")) {
#'   print(ggplot(Cormorants, aes(count)) +
#'     geom_histogram(binwidth=0.5) +
#' 	  labs(x="Number of birds advertising"))
#'
#' # Quick look at the data, on the log scale, for plots of `count ~ week`,
#' #   stratified by something else.
#'
#'   print(ggplot(Cormorants, aes(week, count, color=height)) +
#'     geom_jitter() +
#' 	  stat_smooth(method="loess", size=2) +
#' 	  scale_y_log10(breaks=c(1,2,5,10)) +
#' 	  geom_vline(xintercept=c(4.5, 9.5)))
#' }
#'
#' # ### models using week
#' fit1 <-glm(count ~ week + station + nest + height + density + tree_health,
#'            data=Cormorants,
#'            family =  poisson)
#'
#' if (requireNamespace("car"))
#'   car::Anova(fit1)
#'
#' # plot fitted effects
#' if (requireNamespace("effects"))
#'   plot(effects::allEffects(fit1))
#'
#'
NULL





#' London Cycling Deaths
#'
#' A data frame containing the number of deaths of cyclists in London from 2005
#' through 2012 in each fortnightly period.  Aberdein & Spiegelhalter (2013)
#' discuss these data in relation to the observation that six cyclists died in
#' London between Nov. 5 and Nov. 13, 2013.
#'
#'
#' @name CyclingDeaths
#' @docType data
#' @format A data frame with 208 observations on the following 2 variables.
#' \describe{
#'   \item{\code{date}}{a Date}
#'   \item{\code{deaths}}{number of deaths, a numeric vector}
#'   }
#' @references Aberdein, Jody and Spiegelhalter, David (2013). Have London's
#' roads become more dangerous for cyclists? \emph{Significance}, 10(6),
#' 46--48.
#' @source
#' \url{https://www.data.gov.uk/dataset/cb7ae6f0-4be6-4935-9277-47e5ce24a11f/road-accidents-safety-data},
#' STATS 19 data, 2005-2012, using the files \code{Casualty0512.csv} and
#' \code{Accidents0512.csv}
#' @keywords datasets
#' @examples
#'
#' data(CyclingDeaths)
#'
#' plot(deaths ~ date, data=CyclingDeaths,
#'   type="h",
#' 	lwd=3,
#' 	ylab="Number of deaths",
#' 	axes=FALSE)
#' axis(1, at=seq(as.Date('2005-01-01'),
#'                by='years',
#'                length.out=9),
#'      labels=2005:2013)
#' axis(2, at=0:3)
#'
#' # make a one-way frequency table
#' CyclingDeaths.tab <- table(CyclingDeaths$deaths)
#'
#' gf <- goodfit(CyclingDeaths.tab)
#' gf
#' summary(gf)
#'
#' rootogram(gf, xlab="Number of Deaths")
#' distplot(CyclingDeaths.tab)
#'
#' # prob of 6 or more deaths in one fortnight
#' lambda <- gf$par$lambda
#' ppois(5, lambda, lower.tail=FALSE)
#'
NULL





#' Dayton Student Survey on Substance Use
#'
#' This data, from Agresti (2002), Table 9.1, gives the result of a 1992 survey
#' in Dayton Ohio of 2276 high school seniors on whether they had ever used
#' alcohol, cigarettes and marijuana.
#'
#' Agresti uses the letters G (\code{sex}), R (\code{race}), A
#' (\code{alcohol}), C (\code{cigarette}), M (\code{marijuana}) to refer to the
#' table variables, and this usage is followed in the examples below.
#'
#' Background variables include \code{sex} and \code{race} of the respondent
#' (GR), typically treated as explanatory, so that any model for the full table
#' should include the term \code{sex:race}. Models for the reduced table,
#' collapsed over \code{sex} and \code{race} are not entirely unreasonable, but
#' don't permit the estimation of the effects of these variables on the
#' responses.
#'
#' The full 5-way table contains a number of cells with counts of 0 or 1, as
#' well as many cells with large counts, and even the ACM table collapsed over
#' GR has some small cell counts.  Consequently, residuals for these models in
#' mosaic displays are best represented as standardized (adjusted) residuals.
#'
#' @name DaytonSurvey
#' @docType data
#' @format A frequency data frame with 32 observations on the following 6
#' variables.
#' \describe{
#'   \item{\code{cigarette}}{a factor with levels \code{Yes} \code{No}}
#'   \item{\code{alcohol}}{a factor with levels \code{Yes} \code{No}}
#'   \item{\code{marijuana}}{a factor with levels \code{Yes} \code{No}}
#'   \item{\code{sex}}{a factor with levels \code{female} \code{male}}
#'   \item{\code{race}}{a factor with levels \code{white} \code{other}}
#'   \item{\code{Freq}}{a numeric vector}
#' }

#' @references Thompson, L. (2009). \emph{R (and S-PLUS) Manual to Accompany
#' Agresti's Categorical Data},
#' http://www.stat.ufl.edu/~aa/cda/Thompson_manual.pdf
#' @source Agresti, A. (2002). \emph{Categorical Data Analysis}, 2nd Ed., New
#' York: Wiley-Interscience, Table 9.1, p. 362.
#' @keywords datasets
#' @examples
#'
#' data(DaytonSurvey)
#'
#' # mutual independence
#' mod.0  <- glm(Freq ~ ., data=DaytonSurvey, family=poisson)
#'
#' # mutual independence + GR
#' mod.GR <- glm(Freq ~ . + sex*race, data=DaytonSurvey, family=poisson)
#' anova(mod.GR, test = "Chisq")
#'
#' # all two-way terms
#' mod.all2way <- glm(Freq ~ .^2, data=DaytonSurvey, family=poisson)
#' anova(mod.all2way, test = "Chisq")
#'
#' # compare models
#' LRstats(mod.0, mod.GR, mod.all2way)
#'
#' # collapse over sex and race
#' Dayton.ACM <- aggregate(Freq ~ cigarette+alcohol+marijuana,
#'                         data=DaytonSurvey,
#'                         FUN=sum)
#' Dayton.ACM
#'
NULL





#' Dependencies of R Packages
#'
#' This one-way table gives the type-token distribution of the number of
#' dependencies declared in 4983 packages listed on CRAN on January 17, 2014.
#'
#'
#' @name Depends
#' @docType data
#' @format The format is a one-way frequency table of counts of packages with
#' 0, 1, 2, ... dependencies.
#'
#' \preformatted{
#'  table' int [1:15(1d)] 986 1347 993 685 375 298 155 65 32 19 ...
#'  - attr(*, "dimnames")=List of 1
#'  ..$ Depends: chr [1:15] "0" "1" "2" "3" ...
#' }
#' @source Using code from
#' \url{https://blog.revolutionanalytics.com/2013/12/a-look-at-the-distribution-of-r-package-dependencies.html}
#' @keywords datasets
#' @examples
#'
#' data(Depends)
#' plot(Depends,
#'      xlab="Number of Dependencies",
#'      ylab="Number of R Packages",
#'      lwd=8)
#'
#' # what type of distribution?
#' # Ord_plot can't classify this!
#' Ord_plot(Depends)
#'
#' \dontrun{
#' # The code below, from Joseph Rickert, downloads and tabulates the data
#' p <- as.data.frame(available.packages(),stringsAsFactors=FALSE)
#' names(p)
#'
#' pkgs <- data.frame(p[,c(1,4)])                  # Pick out Package names and Depends
#' row.names(pkgs) <- NULL                         # Get rid of row names
#' pkgs <- pkgs[complete.cases(pkgs[,2]),]         # Remove NAs
#'
#' pkgs$Depends2 <-strsplit(pkgs$Depends,",")      # split list of Depends
#' pkgs$numDepends <- as.numeric(lapply(pkgs$Depends2,length)) # Count number of dependencies in list
#' zeros <- c(rep(0,dim(p)[1] - dim(pkgs)[1]))     # Account for packages with no dependencies
#' Deps <- as.vector(c(zeros,pkgs$numDepends))     # Set up to tablate
#' Depends <- table(Deps)
#'
#' }
#'
NULL





#' Detergent Preference Data
#'
#' Cross-classification of a sample of 1008 consumers according to (a) the
#' softness of the laundry water used, (b) previous use of detergent Brand M,
#' (c) the temperature of laundry water used and (d) expressed preference for
#' Brand X or Brand M in a blind trial.
#'
#'
#' @name Detergent
#' @docType data
#' @format A 4-dimensional array resulting from cross-tabulating 4 variables
#' for 1008 observations. The variable names and their levels are:
#'
#'   \tabular{rll}{
#' dim \tab Name \tab Levels \cr
#' 1\tab \code{Temperature}\tab \code{"High", "Low"}\cr
#' 2\tab \code{M_User}\tab \code{"Yes", "No"}\cr
#' 3\tab \code{Preference}\tab \code{"Brand X", "Brand M"}\cr
#' 4\tab \code{Water_softness}\tab \code{"Soft", "Medium", "Hard"}\cr
#' }

#' @references
#' Ries, P. N. & Smith, H. (1963). The use of
#' chi-square for preference testing in multidimensional problems.
#' \emph{Chemical Engineering Progress}, 59, 39-43.
#' @source
#'
#' Fienberg, S. E. (1980). \emph{The Analysis of
#' Cross-Classified Categorical Data} Cambridge, MA: MIT Press, p. 71.
#' @keywords datasets
#' @examples
#'
#' data(Detergent)
#'
#' # basic mosaic plot
#' mosaic(Detergent, shade=TRUE)
#'
#' require(MASS)
#' (det.mod0 <- loglm(~ Preference + Temperature + M_User + Water_softness,
#'                    data=Detergent))
#' # examine addition of two-way terms
#' add1(det.mod0, ~ .^2, test="Chisq")
#'
#' # model for Preference as a response
#' (det.mod1 <- loglm(~ Preference + (Temperature * M_User * Water_softness),
#'                    data=Detergent))
#' mosaic(det.mod0)
#'
#'
#'
NULL





#' Survival in the Donner Party
#'
#' This data frame contains information on the members of the Donner Party, a
#' group of people who attempted to migrate to California in 1846. They were
#' trapped by an early blizzard on the eastern side of the Sierra Nevada
#' mountains, and before they could be rescued, nearly half of the party had
#' died.
#'
#' What factors affected who lived and who died?
#'
#' This data frame uses the person's name as row labels. \code{family} reflects
#' a recoding of the last names of individuals to reduce the number of factor
#' levels. The main families in the Donner party were: Donner, Graves, Breen
#' and Reed. The families of Murphy, Foster and Pike are grouped as
#' \code{'MurFosPik'}, those of Fosdick and Wolfinger are coded as
#' \code{'FosdWolf'}, and all others as \code{'Other'}.
#'
#' \code{survived} is the response variable. What kind of models should be used
#' here?
#'
#' @name Donner
#' @docType data
#' @format A data frame with 90 observations on the following 5 variables.
#'
#' \describe{
#'   \item{\code{family}}{family name, a factor with 10 levels }
#'   \item{\code{age}}{age of person, a numeric vector}
#'   \item{\code{sex}}{a factor with levels \code{Female} \code{Male}}
#'   \item{\code{survived}}{a numeric vector, 0 or 1}
#'   \item{\code{death}}{date of death for those who died before rescue, a POSIXct}
#' }
#'
#' @seealso \code{donner} in \pkg{alr3}, \code{\link[Sleuth2]{case2001}} in
#' \pkg{Sleuth2}(adults only) provide similar data sets.
#'
#' @references Ramsey, F.L. and Schafer, D.W. (2002).  \emph{The Statistical
#' Sleuth: A Course in Methods of Data Analysis}, (2nd ed), Duxbury.
#'
#' Friendly, M. and Meyer, D. (2016).  \emph{Discrete Data Analysis with R:
#' Visualization and Modeling Techniques for Categorical and Count Data}.  Boca
#' Raton, FL: Chapman & Hall/CRC. \url{http://ddar.datavis.ca}.
#' @source D. K. Grayson, 1990, "Donner party deaths: A demographic
#' assessment", \emph{J. Anthropological Research}, \bold{46}, 223-242.
#'
#' Johnson, K. (1996). \emph{Unfortunate Emigrants: Narratives of the Donner
#' Party}.  Logan, UT: Utah State University Press.  Additions, and dates of
#' death from \url{http://user.xmission.com/~octa/DonnerParty/Roster.htm}.
#' @keywords datasets
#' @examples
#'
#' # conditional density plots
#' op <- par(mfrow=c(1,2), cex.lab=1.5)
#' cdplot(factor(survived) ~ age,
#'        subset=sex=='Male',
#'        data=Donner,
#'        main="Donner party: Males",
#'        ylevels=2:1,
#'        ylab="Survived",
#'        yaxlabels=c("yes", "no"))
#' with(Donner, rug(jitter(age[sex=="Male"]),
#'                  col="white", quiet=TRUE))
#'
#' cdplot(factor(survived) ~ age,
#'        subset=sex=='Female',
#'        data=Donner,
#'        main="Donner party: Females",
#'        ylevels=2:1,
#'        ylab="Survived",
#'        yaxlabels=c("yes", "no"))
#' with(Donner, rug(jitter(age[sex=="Female"]),
#'                  col="white", quiet=TRUE))
#' par(op)
#'
#'
#' # fit some models
#' (mod1 <- glm(survived ~ age + sex, data=Donner, family=binomial))
#' (mod2 <- glm(survived ~ age * sex, data=Donner, family=binomial))
#' anova(mod2, test="Chisq")
#'
#' (mod3 <- glm(survived ~ poly(age,2) * sex, data=Donner, family=binomial))
#' anova(mod3, test="Chisq")
#' LRstats(glmlist(mod1, mod2, mod3))
#'
#' # plot fitted probabilities from mod2 and mod3
#' # idea from: http://www.ling.upenn.edu/~joseff/rstudy/summer2010_ggplot2_intro.html
#' library(ggplot2)
#'
#' # separate linear fits on age for M/F
#' ggplot(Donner, aes(age, survived, color = sex)) +
#'   geom_point(position = position_jitter(height = 0.02, width = 0)) +
#'   stat_smooth(method = "glm",
#'               method.args = list(family = binomial),
#'               formula = y ~ x,
#'               alpha = 0.2,
#'               size=2,
#'               aes(fill = sex))
#'
#' # separate quadratics
#' ggplot(Donner, aes(age, survived, color = sex)) +
#'   geom_point(position = position_jitter(height = 0.02, width = 0)) +
#'   stat_smooth(method = "glm",
#'               method.args = list(family = binomial),
#'               formula = y ~ poly(x,2),
#'               alpha = 0.2,
#'               size=2,
#'               aes(fill = sex))
#'
#'
#'
NULL





#' USA 1970 Draft Lottery Data
#'
#' This data set gives the results of the 1970 US draft lottery, in the form of
#' a data frame.
#'
#' The draft lottery was used to determine the order in which eligible men
#' would be called to the Selective Service draft. The days of the year
#' (including February 29) were represented by the numbers 1 through 366
#' written on slips of paper. The slips were placed in separate plastic
#' capsules that were mixed in a shoebox and then dumped into a deep glass jar.
#' Capsules were drawn from the jar one at a time.
#'
#' The first number drawn was 258 (September 14), so all registrants with that
#' birthday were assigned lottery number \code{Rank} 1. The second number drawn
#' corresponded to April 24, and so forth.  All men of draft age (born 1944 to
#' 1950) who shared a birthdate would be called to serve at once. The first 195
#' birthdates drawn were later called to serve in the order they were drawn;
#' the last of these was September 24.
#'
#' @name Draft1970
#' @docType data
#' @format A data frame with 366 observations on the following 3 variables.
#' \describe{
#'   \item{\code{Day}}{day of the year, 1:366}
#'   \item{\code{Rank}}{draft priority rank of people born on that day}
#'   \item{\code{Month}}{an ordered factor with levels \code{Jan} < \code{Feb} \dots < \code{Dec}}
#' }
#'
#' @seealso \code{\link{Draft1970table}}
#' @references Fienberg, S. E. (1971), "Randomization and Social Affairs: The
#' 1970 Draft Lottery," \emph{Science}, 171, 255-261.
#'
#' \url{https://en.wikipedia.org/wiki/Draft_lottery_(1969)}
#' @source Starr, N. (1997). Nonrandom Risk: The 1970 Draft Lottery,
#' \emph{Journal of Statistics Education}, v.5, n.2
#' \url{https://jse.amstat.org/v5n2/datasets.starr.html}
#' @keywords datasets
#' @examples
#'
#' data(Draft1970)
#'
#' # scatterplot
#' plot(Rank ~ Day, data=Draft1970)
#' with(Draft1970, lines(lowess(Day, Rank), col="red", lwd=2))
#' abline(lm(Rank ~ Day, data=Draft1970), col="blue")
#'
#' # boxplots
#' plot(Rank ~ Month, data=Draft1970, col="bisque")
#'
#' lm(Rank ~ Month, data=Draft1970)
#' anova(lm(Rank ~ Month, data=Draft1970))
#'
#' # make the table version
#' Draft1970$Risk <- cut(Draft1970$Rank, breaks=3, labels=c("High", "Med", "Low"))
#' with(Draft1970, table(Month, Risk))
#'
NULL





#' USA 1970 Draft Lottery Table
#'
#' This data set gives the results of the 1970 US draft lottery, in the form of
#' a frequency table. The rows are months of the year, Jan--Dec and columns
#' give the number of days in that month which fall into each of three draft
#' risk categories High, Medium, and Low, corresponding to the chances of being
#' called to serve in the US army.
#'
#' The lottery numbers are divided into three categories of risk of being
#' called for the draft -- High, Medium, and Low -- each representing roughly
#' one third of the days in a year.  Those birthdays having the highest risk
#' have lottery numbers 1-122, medium risk have numbers 123-244, and the lowest
#' risk category contains lottery numbers 245-366.
#'
#' @name Draft1970table
#' @docType data
#' @format The format is:
#'
#' \preformatted{
#' 'table' int [1:12, 1:3] 9 7 5 8 9 11 12 13 10 9 ...
#' - attr(*, "dimnames")=List of 2
#' ..$ Month: chr [1:12] "Jan" "Feb" "Mar" "Apr" ...
#' ..$ Risk : chr [1:3] "High" "Med" "Low"
#' }
#'
#' @seealso \code{\link{Draft1970}}
#' @references Fienberg, S. E. (1971), "Randomization and Social Affairs: The
#' 1970 Draft Lottery," \emph{Science}, 171, 255-261.
#'
#' Starr, N. (1997). Nonrandom Risk: The 1970 Draft Lottery, \emph{Journal of
#' Statistics Education}, v.5, n.2
#' \url{https://jse.amstat.org/v5n2/datasets.starr.html}
#' @source This data is available in several forms, but the table version was
#' obtained from
#'
#' \url{https://sas.uwaterloo.ca/~rwoldfor/software/eikosograms/data/draft-70}
#' @keywords datasets
#' @examples
#'
#' data(Draft1970table)
#' chisq.test(Draft1970table)
#'
#' # plot.table -> graphics:::mosaicplot
#' plot(Draft1970table, shade=TRUE)
#' mosaic(Draft1970table, gp=shading_Friendly)
#'
#' # correspondence analysis
#' if(require(ca)) {
#'   ca(Draft1970table)
#'   plot(ca(Draft1970table))
#' }
#'
#' # convert to a frequency data frame with ordered factors
#' Draft1970df <- as.data.frame(Draft1970table)
#'
#' Draft1970df <- within(Draft1970df, {
#'   Month <- ordered(Month)
#'   Risk <- ordered(Risk, levels=rev(levels(Risk)))
#'   })
#' str(Draft1970df)
#'
#' # similar model, as a Poisson GLM
#' indep <- glm(Freq ~ Month + Risk, family = poisson, data = Draft1970df)
#'
#' mosaic(indep, residuals_type="rstandard", gp=shading_Friendly)
#'
#' # numeric scores for tests of ordinal factors
#' Cscore <- as.numeric(Draft1970df$Risk)
#' Rscore <- as.numeric(Draft1970df$Month)
#'
#' # linear x linear association between Month and Risk
#' linlin <- glm(Freq ~ Month + Risk + Rscore:Cscore, family = poisson, data = Draft1970df)
#'
#' # compare models
#' anova(indep, linlin, test="Chisq")
#' mosaic(linlin, residuals_type="rstandard", gp=shading_Friendly)
#'
#'
#'
NULL





#' Sources of Knowledge of Cancer
#'
#' Observational data on a sample of 1729 individuals, cross-classified in a
#' 2^5 table according to their sources of information (read newspapers, listen
#' to the radio, do 'solid' reading, attend lectures) and whether they have
#' good or poor knowledge regarding cancer.  Knowledge of cancer is often
#' treated as the response.
#'
#'
#' @name Dyke
#' @docType data
#' @format A 5-dimensional array resulting from cross-tabulating 5 variables
#' for 1729 observations. The variable names and their levels are:
#'
#' \tabular{rll}{
#'  dim \tab Name \tab Levels \cr
#'   1\tab \code{Knowledge}\tab \code{"Good", "Poor"}\cr
#'   2\tab \code{Reading}\tab \code{"No", "Yes"}\cr
#'   3\tab \code{Radio}\tab \code{"No", "Yes"}\cr
#'   4\tab \code{Lectures}\tab \code{"No", "Yes"}\cr
#'   5\tab \code{Newspaper}\tab \code{"No", "Yes"}\cr
#' }

#' @references
#' Dyke, G. V. and Patterson, H. D. (1952). Analysis of factorial
#' arrangements when the data are proportions. \emph{Biometrics}, 8, 1-12.
#'
#' Lindsey, J. K. (1993).  \emph{Models for Repeated Measurements} Oxford, UK:
#' Oxford University Press, p. 57.
#'
#' @source Fienberg, S. E. (1980). \emph{The Analysis of Cross-Classified
#' Categorical Data} Cambridge, MA: MIT Press, p. 85, Table 5-6.
#' @keywords datasets
#' @examples
#'
#' data(Dyke)
#'
#' # independence model
#' mosaic(Dyke, shade=TRUE)
#'
#' # null model, Knowledge as response, independent of others
#' require(MASS)
#' dyke.mod0 <- loglm(~ Knowledge + (Reading * Radio * Lectures * Newspaper), data=Dyke)
#' dyke.mod0
#' mosaic(dyke.mod0)
#'
#' # view as doubledecker plot
#' Dyke <- Dyke[2:1,,,,]    # make Good the highlighted value of Knowledge
#' doubledecker(Knowledge ~ ., data=Dyke)
#'
#' # better version, with some options
#' doubledecker(Knowledge ~ Lectures + Reading + Newspaper + Radio,
#'   data=Dyke,
#' 	margins = c(1,6, length(dim(Dyke)) + 1, 1),
#' 	fill_boxes=list(rep(c("white", gray(.90)),4))
#' 	)
#'
#' # separate (conditional) plots for those who attend lectures and those who do not
#' doubledecker(Knowledge ~ Reading + Newspaper + Radio,
#'   data=Dyke[,,,1,],
#' 	main="Do not attend lectures",
#' 	margins = c(1,6, length(dim(Dyke)) + 1, 1),
#' 	fill_boxes=list(rep(c("white", gray(.90)),3))
#' 	)
#' doubledecker(Knowledge ~ Reading + Newspaper + Radio,
#'   data=Dyke[,,,2,],
#' 	main="Attend lectures",
#' 	margins = c(1,6, length(dim(Dyke)) + 1, 1),
#' 	fill_boxes=list(rep(c("white", gray(.90)),3))
#' 	)
#'
#'
#' drop1(dyke.mod0, test="Chisq")
#'
#'
NULL





#' Carcinogenic Effects of a Fungicide
#'
#' Data from Gart (1971) on the carcinogenic effects of a certain fungicide in
#' two strains of mice. Of interest is how the association between \code{group}
#' (Control, Treated) and \code{outcome} (Tumor, No Tumor) varies with
#' \code{sex} and \code{strain} of the mice.
#'
#' Breslow (1976) used this data to illustrate the application of linear models
#' to log odds ratios.
#'
#' All tables have some small cells, so a continuity correction is recommended.
#'
#' @name Fungicide
#' @docType data
#' @format The data comprise a set of four 2 x 2 tables classifying 403 mice,
#' either Control or Treated and whether or not a tumor was later observed.
#' The four groups represent the combinations of sex and strain of mice.
#'
#' The format is:
#' \preformatted{
#' num [1:2, 1:2, 1:2, 1:2] 5 4 74 12 3 2 84 14 10 4 ...
#' - attr(*, "dimnames")=List of 4
#' ..$ group  : chr [1:2] "Control" "Treated"
#' ..$ outcome: chr [1:2] "Tumor" "NoTumor"
#' ..$ sex    : chr [1:2] "M" "F"
#' ..$ strain : chr [1:2] "1" "2"
#' }
#
#' @references Breslow, N. (1976), Regression analysis of the log odds ratio: A
#' method for retrospective studies, \emph{Biometrics}, 32(3), 409-416.
#'
#' @source Gart, J. J. (1971). The comparison of proportions: a review of
#' significance tests, confidence intervals and adjustments for stratification.
#' \emph{International Statistical Review}, 39, 148-169.
#'
#' @keywords datasets
#' @examples
#'
#' data(Fungicide)
#' # loddsratio was moved to vcd; requires vcd_1.3-3+
#' \dontrun{
#' fung.lor <- loddsratio(Fungicide, correct=TRUE)
#' fung.lor
#' confint(fung.lor)
#' }
#'
#' # visualize odds ratios in fourfold plots
#' cotabplot(Fungicide, panel=cotab_fourfold)
#' #  -- fourfold() requires vcd >= 1.2-10
#' fourfold(Fungicide, p_adjust_method="none")
#'
#'
#'
NULL





#' Geissler's Data on the Human Sex Ratio
#'
#' Geissler (1889) published data on the distributions of boys and girls in
#' families in Saxony, collected for the period 1876-1885. The \code{Geissler}
#' data tabulates the family composition of 991,958 families by the number of
#' boys and girls listed in the table supplied by Edwards (1958, Table 1).
#'
#' The data on family composition was available because, on the birth of a
#' child, the parents had to state the sex of all their children on the birth
#' certificate. These family records are not necessarily independent, because a
#' given family may have had several children during this 10 year period,
#' included as multiple records.
#'
#' @name Geissler
#' @docType data
#' @format A data frame with 90 observations on the following 4 variables.  The
#' rows represent the non-NA entries in Edwards' table.
#' \describe{
#'   \item{\code{boys}}{number of boys in the family, \code{0:12}}
#'   \item{\code{girls}}{number of girls in the family, \code{0:12}}
#'   \item{\code{size}}{family size: \code{boys+girls}}
#'   \item{\code{Freq}}{number of families with this sex composition}
#' }

#' @seealso \code{\link[vcd]{Saxony}}, containing the data for families of size
#' 12.
#' @references
#' Friendly, M. and Meyer, D. (2016).  \emph{Discrete Data Analysis
#' with R: Visualization and Modeling Techniques for Categorical and Count
#' Data}.  Boca Raton, FL: Chapman & Hall/CRC. \url{http://ddar.datavis.ca}.
#'
#' Geissler, A. (1889). \emph{Beitrage zur Frage des Geschlechts verhaltnisses
#' der Geborenen} Z. K. Sachsischen Statistischen Bureaus, 35, n.p.
#'
#' Lindsey, J. K. & Altham, P. M. E. (1998).  Analysis of the human sex ratio
#' by using overdispersion models. \emph{Journal of the Royal Statistical
#' Society: Series C (Applied Statistics)}, 47, 149-157.
#'
#' @source
#' Edwards, A. W. F. (1958). An Analysis Of Geissler's Data On The
#' Human Sex Ratio. \emph{Annals of Human Genetics}, 23, 6-15.
#' @keywords datasets
#' @examples
#'
#' data(Geissler)
#' str(Geissler)
#'
#' # reproduce Saxony data, families of size 12
#' Saxony12 <- subset(Geissler, size==12, select=c(boys, Freq))
#' rownames(Saxony12)<-NULL
#'
#' # make a 1-way table
#' xtabs(Freq~boys, Saxony12)
#'
#' # extract data for other family sizes
#' Saxony11 <- subset(Geissler, size==11, select=c(boys, Freq))
#' rownames(Saxony11)<-NULL
#'
#' Saxony10 <- subset(Geissler, size==10, select=c(boys, Freq))
#' rownames(Saxony10)<-NULL
#'
#'
NULL





#' Clothing and Intelligence Rating of Children
#'
#' Schoolboys were classified according to their clothing and to their teachers
#' rating of "dullness" (lack of intelligence), in a 5 x 7 table originally
#' from Gilby (1911). Anscombe (1981) presents a slightly collapsed 4 x 6
#' table, used here, where the last two categories of clothing were pooled as
#' were the first two categories of dullness due to small counts.
#'
#' Both \code{Dullness} and \code{Clothing} are ordered categories, so models
#' and methods that examine their association in terms of ordinal categories
#' are profitable.
#'
#'
#' @name Gilby
#' @docType data
#' @format A 2-dimensional array resulting from cross-tabulating 2 variables
#' for 1725 observations. The variable names and their levels are:
#'
#' \tabular{rll}{
#'   No \tab Name \tab Levels \cr
#'   1\tab \code{Dullness}\tab \code{"Ment. defective", "Slow", "Slow Intell", "Fairly Intell", "Capable", "V.Able"}\cr
#'   2\tab \code{Clothing}\tab \code{"V.Well clad", "Well clad", "Passable", "Insufficient"}\cr
#' }

#' @references
#' Gilby, W. H. (1911).
#' On the significance of the teacher's appreciation of general
#' intelligence.  \emph{Biometrika}, 8, 93-108 (esp. p. 94).  (Quoted by Kendall (1943,..., 1953) Table 13.1, p 320.)
#'
#' @source Anscombe, F. J. (1981). \emph{Computing in Statistical Science Through APL}. New York: Springer-Verlag, p. 302
#' @keywords datasets
#' @examples
#'
#' data(Gilby)
#'
#' # CMH tests treating row/column variables as ordinal
#' CMHtest(Gilby)
#'
#' mosaic(Gilby, shade=TRUE)
#'
#' # correspondence analysis to see relations among categories
#' if(require(ca)){
#' 	ca(Gilby)
#' 	plot(ca(Gilby), lines=TRUE)
#'
#' }
#'
#'
#'
NULL





#' British Social Mobility from Glass(1954)
#'
#' Glass(1954) gave this 5 x 5 table on the occupations of 3500 British fathers
#' and their sons.
#'
#' The occupational categories in order of status are: (1) Professional & High
#' Administrative (2) Managerial, Executive & High Supervisory (3) Low
#' Inspectional & Supervisory (4) Routine Nonmanual & Skilled Manual (5) Semi-
#' & Unskilled Manual
#'
#' However, to make the point that factors are ordered alphabetically by
#' default, Friendly & Meyer (2016) introduce this data set in the form given
#' here.
#'
#' @name Glass
#' @docType data
#' @format A frequency data frame with 25 observations on the following 3
#' variables representing a 5 x 5 table with 3500 cases.
#' \describe{
#'   \item{\code{father}}{a factor with levels \code{Managerial} \code{Professional} \code{Skilled} \code{Supervisory} \code{Unskilled}}
#'   \item{\code{son}}{a factor with levels \code{Managerial} \code{Professional} \code{Skilled} \code{Supervisory} \code{Unskilled}}
#'   \item{\code{Freq}}{a numeric vector}
#' }
#'
#' @references
#' Bishop, Y. M. M. and Fienberg, S. E. and Holland, P. W. (1975).
#' \emph{Discrete Multivariate Analysis: Theory and Practice}, MIT Press.
#'
#' Friendly, M. and Meyer, D. (2016).  \emph{Discrete Data Analysis with R:
#' Visualization and Modeling Techniques for Categorical and Count Data}.  Boca
#' Raton, FL: Chapman & Hall/CRC. \url{http://ddar.datavis.ca}.
#'
#' @source Glass, D. V. (1954), \emph{Social Mobility in Britain}. The Free
#' Press.
#' @keywords datasets
#' @examples
#'
#' data(Glass)
#' glass.tab <- xtabs(Freq ~ father + son, data=Glass)
#'
#' largs <- list(set_varnames=list(father="Father's Occupation",
#'                                 son="Son's Occupation"),
#'               abbreviate=10)
#' gargs <- list(interpolate=c(1,2,4,8))
#'
#' mosaic(glass.tab,
#'   shade=TRUE,
#'   labeling_args=largs,
#'   gp_args=gargs,
#'   main="Alphabetic order",
#'   legend=FALSE,
#'   rot_labels=c(20,90,0,70))
#'
#' # reorder by status
#' ord <- c(2, 1, 4, 3, 5)
#' mosaic(glass.tab[ord, ord],
#'   shade=TRUE,
#'   labeling_args=largs,
#'   gp_args=gargs,
#'   main="Effect order",
#'   legend=FALSE,
#'   rot_labels=c(20,90,0,70))
#'
#'
NULL





#' General Social Survey-- Sex and Party affiliation
#'
#' Data from the General Social Survey, 1991, on the relation between sex and
#' party affiliation.
#'
#'
#' @name GSS
#' @docType data
#' @format A data frame in frequency form with 6 observations on the following
#' 3 variables.
#'
#' \describe{
#'   \item{\code{sex}}{a factor with levels \code{female} \code{male}}
#'   \item{\code{party}}{a factor with levels \code{dem} \code{indep} \code{rep}}
#'   \item{\code{count}}{a numeric vector}
#' }

#' @source Agresti, A. \emph{Categorical Data Analysis}, 2nd E., John Wiley &
#' Sons, 2002, Table 3.11, p. 106.
#' @keywords datasets
#' @examples
#'
#' data(GSS)
#' str(GSS)
#'
#' # use xtabs to show the table in a compact form
#' (GSStab <- xtabs(count ~ sex + party, data=GSS))
#'
#' # fit the independence model
#' (mod.glm <- glm(count ~ sex + party, family = poisson, data = GSS))
#'
#' # display all the residuals in a mosaic plot
#' mosaic(mod.glm,
#'   formula = ~ sex + party,
#'   labeling = labeling_residuals,
#'   suppress=0)
#'
NULL





#' Hair Color and Eye Color in Caithness and Aberdeen
#'
#' A three-way frequency table crossing eye color and hair color in two places,
#' Caithness and Aberdeen, Scotland. These data were of interest to Fisher
#' (1940) and others because there are mixtures of people of Nordic, Celtic and
#' Anglo-Saxon origin.
#'
#' One or both tables have been widely analyzed in conjunction with RC and
#' canonical correlation models for categorical data, e.g., Becker and Clogg
#' (1989).
#'
#' @details
#' The hair and eye colors are ordered as in the original source, suggesting
#' that they form ordered categories.
#'
#' @name HairEyePlace
#' @docType data
#' @format
#' The format is:
#' \preformatted{
#'   num [1:4, 1:5, 1:2] 326 688 343 98 38 116 84 48 241 584 ...
#' - attr(*, "dimnames")=List of 3
#' ..$ Eye  : chr [1:4] "Blue" "Light" "Medium" "Dark"
#' ..$ Hair : chr [1:5] "Fair" "Red" "Medium" "Dark" ...
#' ..$ Place: chr [1:2] "Caithness" "Aberdeen"
#' }
#'
#' @references Becker, M. P., and Clogg, C. C. (1989).  Analysis of Sets of
#' Two-Way Contingency Tables Using Association Models.  \emph{Journal of the
#' American Statistical Association}, 84(405), 142-151.
#'
#' Fisher, R.A. (1940) The precision of discriminant functions.  \emph{Annals
#' of Eugenics}, 10, 422-429.
#' @source This data was taken from the \code{colors} data in \pkg{logmult}.
#' @keywords datasets
#' @examples
#'
#' data(HairEyePlace)
#'
#' # separate mosaics
#' mosaic(HairEyePlace[,,1], shade=TRUE, main="Caithness")
#' mosaic(HairEyePlace[,,2], shade=TRUE, main="Aberdeen")
#'
#' # condition on Place
#' mosaic(~Hair + Eye |Place, data=HairEyePlace, shade=TRUE, legend=FALSE)
#'
#' cotabplot(~Hair+Eye|Place, data=HairEyePlace, shade=TRUE, legend=FALSE)
#'
NULL





#' Hauser (1979) Data on Social Mobility
#'
#' Hauser (1979) presented this two-way frequency table, cross-classifying
#' occupational categories of sons and fathers in the United States.
#'
#' It is a good example for exploring a variety of models for square tables:
#' quasi-independence, quasi-symmetry, row/column effects, uniform association,
#' etc., using the facilities of the \pkg{gnm}.
#'
#' Hauser's data was first presented in 1979, and then published in 1980. The
#' name of the dataset reflects the earliest use.
#'
#' It reflects the "frequencies in a classification of son's first full-time
#' civilian occupation by father's (or other family head's) occupation at son's
#' sixteenth birthday among American men who were aged 20 to 64 in 1973 and
#' were not currently enrolled in school".
#'
#' As noted in Hauser's Table 1, "Counts are based on observations weighted to
#' estimate population counts and compensate for departures of the sampling
#' design from simple random sampling. Broad occupation groups are upper
#' nonmanual: professional and kindred workers, managers and officials, and
#' non-retail sales workers; lower nonmanual: proprietors, clerical and kindred
#' workers, and retail sales workers; upper manual: craftsmen, foremen, and
#' kindred workers; lower manual: service workers, operatives and kindred
#' workers, and laborers (except farm); farm: farmers and farm managers, farm
#' laborers, and foremen. density of mobility or immobility in the cells to
#' which they refer."
#'
#' The table levels for \code{Son} and \code{Father} have been arranged in
#' order of decreasing status as is common for mobility tables.
#'
#' @name Hauser79
#' @docType data
#' @format A frequency data frame with 25 observations on the following 3
#' variables, representing the cross-classification of 19912 individuals by
#' father's occupation and son's first occupation.
#' \describe{
#'   \item{\code{Son}}{a factor with levels \code{UpNM} \code{LoNM} \code{UpM} \code{LoM} \code{Farm}}
#'   \item{\code{Father}}{a factor with levels \code{UpNM} \code{LoNM} \code{UpM} \code{LoM} \code{Farm}}
#'   \item{\code{Freq}}{a numeric vector}
#' }
#'
#' @references
#' Powers, D.A. and Xie, Y. (2008). \emph{Statistical Methods for
#' Categorical Data Analysis}, Bingley, UK: Emerald.
#' @source
#' R.M. Hauser (1979), Some exploratory methods for modeling mobility
#' tables and other cross-classified data.  In: K.F. Schuessler (Ed.),
#' \emph{Sociological Methodology}, 1980, Jossey-Bass, San Francisco, pp.
#' 413-458. Table 1.
#' @keywords datasets
#' @examples
#'
#' data(Hauser79)
#' str(Hauser79)
#'
#' # display table
#' structable(~Father+Son, data=Hauser79)
#'
#' #Examples from Powers & Xie, Table 4.15
#' # independence model
#' mosaic(Freq ~ Father + Son, data=Hauser79, shade=TRUE)
#'
#' hauser.indep <- gnm(Freq ~ Father + Son,
#'   data=Hauser79,
#'   family=poisson)
#'
#' mosaic(hauser.indep, ~Father+Son,
#'        main="Independence model",
#'        gp=shading_Friendly)
#'
#' # Quasi-independence
#' hauser.quasi <-  update(hauser.indep,
#'                         ~ . + Diag(Father,Son))
#' mosaic(hauser.quasi, ~Father+Son,
#'        main="Quasi-independence model",
#'        gp=shading_Friendly)
#'
#' # Quasi-symmetry
#' hauser.qsymm <-  update(hauser.indep,
#'                         ~ . + Diag(Father,Son) + Symm(Father,Son))
#'
#' mosaic(hauser.qsymm, ~Father+Son,
#'        main="Quasi-symmetry model",
#'        gp=shading_Friendly)
#'
#'
#' # numeric scores for row/column effects
#' Sscore <- as.numeric(Hauser79$Son)
#' Fscore <- as.numeric(Hauser79$Father)
#'
#' # row effects model
#' hauser.roweff <- update(hauser.indep, ~ . + Father*Sscore)
#' LRstats(hauser.roweff)
#'
#' # uniform association
#' hauser.UA <- update(hauser.indep, ~ . + Fscore*Sscore)
#' LRstats(hauser.UA)
#'
#' # uniform association, omitting diagonals
#' hauser.UAdiag <- update(hauser.indep, ~ . + Fscore*Sscore + Diag(Father,Son))
#' LRstats(hauser.UAdiag)
#'
#' # Levels for Hauser 5-level model
#' levels <- matrix(c(
#'   2,  4,  5,  5,  5,
#'   3,  4,  5,  5,  5,
#'   5,  5,  5,  5,  5,
#'   5,  5,  5,  4,  4,
#'   5,  5,  5,  4,  1
#'   ), 5, 5, byrow=TRUE)
#'
#' hauser.topo <- update(hauser.indep,
#'                       ~ . + Topo(Father, Son, spec=levels))
#'
#' mosaic(hauser.topo, ~Father+Son,
#'        main="Topological model", gp=shading_Friendly)
#'
#' # RC model
#' hauser.RC <- update(hauser.indep, ~ . + Mult(Father, Son), verbose=FALSE)
#' mosaic(hauser.RC, ~Father+Son, main="RC model", gp=shading_Friendly)
#' LRstats(hauser.RC)
#'
#' # crossings models
#' hauser.CR <- update(hauser.indep, ~ . + Crossings(Father,Son))
#' mosaic(hauser.topo, ~Father+Son, main="Crossings model", gp=shading_Friendly)
#' LRstats(hauser.CR)
#'
#' hauser.CRdiag <- update(hauser.indep, ~ . + Crossings(Father,Son) + Diag(Father,Son))
#' LRstats(hauser.CRdiag)
#'
#'
#' # compare model fit statistics
#' modlist <- glmlist(hauser.indep, hauser.roweff, hauser.UA, hauser.UAdiag,
#'                    hauser.quasi, hauser.qsymm,  hauser.topo,
#'                    hauser.RC, hauser.CR, hauser.CRdiag)
#' sumry <- LRstats(modlist)
#' sumry[order(sumry$AIC, decreasing=TRUE),]
#' # or, more simply
#' LRstats(modlist, sortby="AIC")
#'
#' mods <- substring(rownames(sumry),8)
#' with(sumry,
#' 	{plot(Df, AIC, cex=1.3, pch=19, xlab='Degrees of freedom', ylab='AIC')
#' 	text(Df, AIC, mods, adj=c(0.5,-.5), col='red', xpd=TRUE)
#' 	})
#'
#'
#'
NULL





#' Sex, Occupation and Heart Disease
#'
#' Classification of individuals by gender, occupational category and
#' occurrence of heart disease
#'
#'
#' @name Heart
#' @docType data
#' @format A 3-dimensional array resulting from cross-tabulating 3 variables
#' for 21522 observations. The variable names and their levels are:
#'
#' \tabular{rll}{
#'   No \tab Name \tab Levels \cr
#'   1\tab \code{Disease}\tab \code{"Disease", "None"}\cr
#'   2\tab \code{Gender}\tab \code{"Male", "Female"}\cr
#'   3\tab \code{Occup}\tab \code{"Unempl", "WhiteCol", "BlueCol"}\cr
#' }

#' @source
#'
#' % \cite{Karger, 1980}
#' Karger, (1980).
#' @keywords datasets
#' @examples
#'
#' data(Heart)
#' str(Heart)
#'
#' # Display the frequencies for occupational categories.
#' # Each row is a 2 x 2 table
#' vcd::structable(Disease + Gender ~ Occup, data=Heart)
#'
#' # display as fourfold plots
#' vcd::cotabplot(~ Disease + Gender | Occup, data=Heart, panel = cotab_fourfold)
#'
NULL





#' Labour Force Participation of Married Women 1967-1971
#'
#' 1583 married women were surveyed over the years 1967-1971, recording whether
#' or not they were employed in the labor force.
#'
#' The data, originally from Heckman & Willis (1977) provide an example of
#' modeling longitudinal categorical data, e.g., with markov chain models for
#' dependence over time.
#'
#' Lindsey (1993) fits an initial set of logistic regression models examining
#' the dependence of employment in 1971 (\code{e1971}) on successive subsets of
#' the previous years, \code{e1970}, \code{e1969}, \dots{} \code{e1967}.
#'
#' Alternatively, one can examine markov chain models of first-order
#' (dependence on previous year), second-order (dependence on previous two
#' years), etc.
#'
#' @name Heckman
#' @docType data
#' @format A 5-dimensional \eqn{2^5} array resulting from cross-tabulating 5
#' binary variables for 1583 observations. The variable names and their levels
#' are:
#'
#' \tabular{rll}{
#'   No \tab Name \tab Levels \cr
#'   1\tab \code{e1971}\tab \code{"71Yes", "No"}\cr
#'   2\tab \code{e1970}\tab \code{"70Yes", "No"}\cr
#'   3\tab \code{e1969}\tab \code{"69Yes", "No"}\cr
#'   4\tab \code{e1968}\tab \code{"68Yes", "No"}\cr
#'   5\tab \code{e1967}\tab \code{"67Yes", "No"}\cr
#' }

#' @references
#' % \cite{HeckmanWillis:77}
#' Heckman, J.J. & Willis, R.J. (1977).
#' "A beta-logistic model for the analysis of sequential labor force
#' participation by married women."  \emph{Journal of Political Economy}, 85:
#' 27-58
#' @source
#'
#' Lindsey, J. K. (1993).  \emph{Models for
#' Repeated Measurements} Oxford, UK: Oxford University Press, p. 185.
#' @keywords datasets
#' @examples
#'
#' data(Heckman)
#'
#' # independence model
#' mosaic(Heckman, shade=TRUE)
#' # same, as a loglm()
#' require(MASS)
#' (heckman.mod0 <- loglm(~ e1971+e1970+e1969+e1968+e1967, data=Heckman))
#' mosaic(heckman.mod0, main="Independence model")
#'
#' # first-order markov chain: bad fit
#' (heckman.mod1 <- loglm(~ e1971*e1970 + e1970*e1969 +e1969*e1968 + e1968*e1967, data=Heckman))
#' mosaic(heckman.mod1, main="1st order markov chain model")
#'
#' # second-order markov chain: bad fit
#' (heckman.mod2 <- loglm(~ e1971*e1970*e1969 + e1970*e1969*e1968 +e1969*e1968*e1967, data=Heckman))
#' mosaic(heckman.mod2, main="2nd order markov chain model")
#'
#' # third-order markov chain: fits OK
#' (heckman.mod3 <- loglm(~ e1971*e1970*e1969*e1968 + e1970*e1969*e1968*e1967, data=Heckman))
#' mosaic(heckman.mod2, main="3rd order markov chain model")
#'
#'
NULL





#' Hospital Visits Data
#'
#' Length of stay in hospital for 132 schizophrenic patients, classified by
#' visiting patterns, originally from Wing (1962).
#'
#' Both table variables can be considered ordinal. The variable \code{visit}
#' refers to visiting patterns recorded hospital.  The category labels are
#' abbreviations of those given by Goodman (1983); e.g., \code{"Regular"} is
#' short for \dQuote{received visitors regularly or patient went home}. The
#' variable \code{stay} refers to length of stay in hospital, in year groups.
#'
#' @name HospVisits
#' @docType data
#' @format A 3 by 3 frequency table, with format:
#' \preformatted{
#' table [1:3, 1:3] 43 6 9 16 11 18 3 10 16
#' - attr(*, "dimnames")=List of 2
#' ..$ visit: chr [1:3] "Regular" "Infrequent" "Never"
#' ..$ stay : chr [1:3] "2-9" "10-19" "20+"
#' }
#
#' @seealso \code{\link[ca]{ca}}
#' @references
#' Wing, J. K. (1962). Institutionalism in Mental Hospitals,
#' \emph{British Journal of Social and Clinical Psychology}, 1 (1), 38-51.
#'
#' @source
#' Goodman, L. A. (1983) The analysis of dependence in
#' cross-classifications having ordered categories, using log-linear models for
#' frequencies and log-linear models for odds.  \emph{Biometrics}, 39, 149-160.
#'
#' @keywords datasets
#' @examples
#'
#' data(HospVisits)
#' mosaic(HospVisits, gp=shading_Friendly)
#'
#' if(require(ca)){
#'   ca(HospVisits)
#'   # surprisingly 1D !
#'   plot(ca(HospVisits))
#'   }
#'
NULL





#' Household Tasks Performed by Husbands and Wives
#'
#' A 13 x 4 table of frequencies of household tasks performed by couples,
#' either by the \code{Husband}, \code{Wife}, \code{Alternating} or \code{Jointly}.
#'
#'
#' @name HouseTasks
#' @docType data
#' @format The format is:
#' \preformatted{
#'   'table' int [1:13, 1:4] 36 11 24 51 13 1 1 14 20 46 ...
#' - attr(*, "dimnames")=List of 2
#' ..$ Task: chr [1:13] "Breakfast" "Dinner" "Dishes" "Driving" ...
#' ..$ Who : chr [1:4] "Alternating" "Husband" "Jointly" "Wife"
#' }
#'
#' @source This data set was taken from \code{\link[factoextra]{housetasks}}, a
#' 13 x 4 data.frame. In this table version, the rows and columns were sorted
#' alphabetically (and a typo was corrected).
#' @keywords datasets
#' @examples
#'
#' data(HouseTasks)
#' str(HouseTasks)
#'
#' chisq.test(HouseTasks)
#'
#' # mosaic plot, illustrating some tweaks to handle overlapping labels
#' require(vcd)
#' mosaic(HouseTasks, shade = TRUE,
#'        labeling = labeling_border(rot_labels = c(45,0, 0, 0),
#'                                   offset_label =c(.5,5,0, 0),
#'                                   varnames = c(FALSE, TRUE),
#'                                   just_labels=c("center","right"),
#'                                   tl_varnames = FALSE),
#'        legend = FALSE)
#'
#' # use seriation package to permute rows & cols using correspondence analysis
#' if(require(seriation)) {
#' order <- seriate(HouseTasks, method = "CA")
#' # the permuted row and column labels
#' rownames(HouseTasks)[order[[1]]]
#' colnames(HouseTasks)[order[[2]]]
#'
#' # do the permutation
#' HT_perm <- permute(HouseTasks, order, margin=1)
#'
#' mosaic(HT_perm, shade = TRUE,
#'        labeling = labeling_border(rot_labels = c(45,0, 0, 0),
#'                                   offset_label =c(.5,5,0, 0),
#'                                   varnames = c(FALSE, TRUE),
#'                                   just_labels=c("center","right"),
#'                                   tl_varnames = FALSE),
#'        legend = FALSE)
#' }
#'
NULL





#' Minnesota High School Graduates
#'
#' Minnesota high school graduates of June 1930 were classified with respect to
#' (a) \code{Rank} by thirds in their graduating class, (b) post-high school
#' \code{Status} in April 1939 (4 levels), (c) \code{Sex}, (d) father's
#' \code{Occupation}al status (7 levels, from 1=High to 7=Low).
#'
#' The data were first presented by Hoyt et al. (1959) and have been analyzed
#' by Fienberg(1980), Plackett(1974) and others.
#'
#' Post high-school \code{Status} is natural to consider as the response.
#' \code{Rank} and father's \code{Occupation} are ordinal variables.
#'
#' @name Hoyt
#' @docType data
#' @format A 4-dimensional array resulting from cross-tabulating 4 variables
#' for 13968 observations. The variable names and their levels are:
#'
#' \tabular{rll}{
#'   No \tab Name \tab Levels \cr
#'   1\tab \code{Status}\tab \code{"College", "School", "Job", "Other"}\cr
#'   2\tab \code{Rank}\tab \code{"Low", "Middle", "High"}\cr
#'   3\tab \code{Occupation}\tab \code{"1", "2", "3", "4", "5", "6", "7"}\cr
#'   4\tab \code{Sex}\tab \code{"Male", "Female"}\cr
#' }
#
#' @seealso \code{\link[MASS]{minn38}} provides the same data as a data frame.
#' @references
#' Hoyt, C. J., Krishnaiah, P. R. and Torrance, E. P. (1959)
#' Analysis of complex contingency tables, \emph{Journal of Experimental
#' Education} 27, 187-194.
#' @source
#'
#' Fienberg, S. E. (1980). \emph{The Analysis of Cross-Classified Categorical
#' Data}. Cambridge, MA: MIT Press, p. 91-92.
#'
#' R. L. Plackett, (1974). \emph{The Analysis of Categorical Data}. London: Griffin.
#' @keywords datasets
#' @examples
#'
#' data(Hoyt)
#'
#' # display the table
#' structable(Status + Sex ~ Rank + Occupation, data=Hoyt)
#'
#' # mosaic for independence model
#' plot(Hoyt, shade=TRUE)
#'
#' # examine all pairwise mosaics
#' pairs(Hoyt, shade=TRUE)
#'
#' # collapse Status to College vs. Non-College
#' Hoyt1 <- collapse.table(Hoyt, Status=c("College", rep("Non-College",3)))
#' plot(Hoyt1, shade=TRUE)
#'
#' #################################################
#' # fitting models with loglm, plotting with mosaic
#' #################################################
#'
#' # fit baseline log-linear model for Status as response
#' require(MASS)
#' hoyt.mod0 <- loglm(~ Status + (Sex*Rank*Occupation),
#'   data=Hoyt1)
#' hoyt.mod0
#'
#' mosaic(hoyt.mod0,
#'   gp=shading_Friendly,
#'   main="Baseline model: Status + (Sex*Rank*Occ)")
#'
#' # add one-way association of Status with factors
#' hoyt.mod1 <- loglm(~ Status * (Sex + Rank + Occupation) + (Sex*Rank*Occupation),
#'   data=Hoyt1)
#' hoyt.mod1
#'
#' mosaic(hoyt.mod1,
#'   gp=shading_Friendly,
#'   main="Status * (Sex + Rank + Occ)")
#'
#' # can we drop any terms?
#' drop1(hoyt.mod1, test="Chisq")
#'
#' # assess model fit
#' anova(hoyt.mod0, hoyt.mod1)
#'
#' # what terms to add?
#' add1(hoyt.mod1, ~.^2, test="Chisq")
#'
#' # add interaction of Sex:Occupation on Status
#' hoyt.mod2 <- update(hoyt.mod1, ~ . + Status:Sex:Occupation)
#'
#' mosaic(hoyt.mod2,
#'   gp=shading_Friendly,
#'   main="Adding Status:Sex:Occupation")
#'
#' # compare model fits
#' anova(hoyt.mod0, hoyt.mod1, hoyt.mod2)
#'
#' # Alternatively, try stepwise analysis, heading toward the saturated model
#' steps <- step(hoyt.mod0,
#'   direction="forward",
#'   scope=~Status*Sex*Rank*Occupation)
#'
#' # display anova
#' steps$anova
#'
#'
NULL





#' ICU data set
#'
#' The ICU data set consists of a sample of 200 subjects who were part of a
#' much larger study on survival of patients following admission to an adult
#' intensive care unit (ICU), derived from Hosmer, Lemeshow and Sturdivant
#' (2013) and Friendly (2000).
#'
#' The major goal of this study was to develop a logistic regression model to
#' predict the probability of survival to hospital discharge of these patients
#' and to study the risk factors associated with ICU mortality. The clinical
#' details of the study are described in Lemeshow, Teres, Avrunin, and Pastides
#' (1988).
#'
#' This data set is often used to illustrate model selection methods for
#' logistic regression.
#'
#' Patient ID numbers are the rownames of the data frame.
#'
#' Note that the last two variables \code{white} and \code{uncons} are a
#' recoding of respectively \code{race} and \code{coma} to binary variables.
#'
#' @name ICU
#' @docType data
#' @format A data frame with 200 observations on the following 22 variables.
#' \describe{
#'   \item{\code{died}}{Died before discharge?, a factor with levels \code{No} \code{Yes}}
#'   \item{\code{age}}{Patient age, a numeric vector}
#'   \item{\code{sex}}{Patient sex, a factor with levels \code{Female} \code{Male}}
#'   \item{\code{race}}{Patient race, a factor with levels \code{Black} \code{Other} \code{White}.  Also represented here as \code{white}.}
#'   \item{\code{service}}{Service at ICU Admission, a factor with levels \code{Medical} \code{Surgical}}
#'   \item{\code{cancer}}{Cancer part of present problem?, a factor with levels \code{No} \code{Yes}}
#'   \item{\code{renal}}{History of chronic renal failure?, a factor with levels \code{No} \code{Yes}}
#'   \item{\code{infect}}{Infection probable at ICU admission?, a factor with levels \code{No} \code{Yes}}
#'   \item{\code{cpr}}{Patient received CPR prior to ICU admission?, a factor with levels \code{No} \code{Yes}}
#'   \item{\code{systolic}}{Systolic blood pressure at admission (mm Hg), a numeric vector}
#'   \item{\code{hrtrate}}{Heart rate at ICU Admission (beats/min), a numeric vector}
#'   \item{\code{previcu}}{Previous admission to an ICU within 6 Months?, a factor with levels \code{No} \code{Yes}}
#'   \item{\code{admit}}{Type of admission, a factor with levels \code{Elective} \code{Emergency}}
#'   \item{\code{fracture}}{Admission with a long bone, multiple, neck, single area, or hip fracture?
#'         a factor with levels \code{No} \code{Yes}}
#'   \item{\code{po2}}{PO2 from initial blood gases, a factor with levels \code{>60} \code{<=60}}
#'   \item{\code{ph}}{pH from initial blood gases, a factor with levels \code{>=7.25} \code{<7.25}}
#'   \item{\code{pco}}{PCO2 from initial blood gases, a factor with levels \code{<=45} \code{>45}}
#'   \item{\code{bic}}{Bicarbonate (HCO3) level from initial blood gases, a factor with levels \code{>=18} \code{<18}}
#'   \item{\code{creatin}}{Creatinine, from initial blood gases, a factor with levels \code{<=2} \code{>2}}
#'   \item{\code{coma}}{Level of unconsciousness at admission to ICU,	a factor with levels \code{None} \code{Stupor} \code{Coma}}
#'   \item{\code{white}}{a recoding of \code{race},  a factor with levels \code{White} \code{Non-white}}
#'   \item{\code{uncons}}{a recoding of \code{coma} a factor with levels \code{No} \code{Yes}}
#' }
#'
#' @references
#'
#' Lemeshow, S., Teres, D., Avrunin, J. S., Pastides, H. (1988). Predicting the
#' Outcome of Intensive Care Unit Patients. \emph{Journal of the American
#' Statistical Association}, 83, 348-356.
#' @source M. Friendly (2000), \emph{Visualizing Categorical Data}, Appendix
#' B.4. SAS Institute, Cary, NC.
#'
#' Hosmer, D. W. Jr., Lemeshow, S. and Sturdivant, R. X. (2013) \emph{Applied
#' Logistic Regression}, NY: Wiley, Third Edition.
#' @keywords datasets
#' @examples
#'
#' data(ICU)
#' # remove redundant variables (race, coma)
#' ICU1 <- ICU[,-c(4,20)]
#'
#' # fit full model
#' icu.full <- glm(died ~ ., data=ICU1, family=binomial)
#' summary(icu.full)
#'
#' # simpler model (found from a "best" subsets procedure)
#' icu.mod1 <- glm(died ~ age + sex + cancer + systolic + admit + uncons,
#'   data=ICU1,
#'   family=binomial)
#' summary(icu.mod1)
#'
#' # even simpler model
#' icu.mod2 <- glm(died ~ age + cancer  + admit + uncons,
#'   data=ICU1,
#'   family=binomial)
#' summary(icu.mod2)
#'
#' anova(icu.mod2, icu.mod1, icu.full, test="Chisq")
#'
#' ## Reproduce Fig 6.12 from VCD
#'
#' icu.fit <- data.frame(ICU, prob=predict(icu.mod2, type="response"))
#'
#' # combine categorical risk factors to a single string
#' risks <- ICU[, c("cancer", "admit", "uncons")]
#' risks[,1] <- ifelse(risks[,1]=="Yes", "Cancer", "")
#' risks[,2] <- ifelse(risks[,2]=="Emergency", "Emerg", "")
#' risks[,3] <- ifelse(risks[,3]=="Yes", "Uncons", "")
#' risks <- apply(risks, 1, paste, collapse="")
#' risks[risks==""] <- "(none)"
#' icu.fit$risks <- risks
#'
#' library(ggplot2)
#' ggplot(icu.fit, aes(x=age, y=prob, color=risks)) +
#' 	geom_point(size=2) +
#' 	geom_line(size=1.25, alpha=0.5) +
#' 	theme_bw() + ylab("Probability of death")
#'
#'
NULL





#' Cross-classification of job satisfaction by income
#'
#' This data set is a contingency table of job satisfaction by income for a
#' small sample of black males from the 1996 General Social Survey, as used by
#' Agresti (2002) for an example.
#'
#' Both \code{income} and \code{satisfaction} are ordinal variables, and are so
#' ordered in the table.  Measures of association, visualizations, and models
#' should take ordinality into account.
#'
#' @name JobSat
#' @docType data
#' @format A 4 x 4 contingency table of \code{income} by \code{satisfaction},
#' with the following structure:
#' \preformatted{
#'   table [1:4, 1:4] 1 2 1 0 3 3 6 1 10 10 ...
#'   - attr(*, "dimnames")=List of 2
#'   ..$ income      : chr [1:4] "< 15k" "15-25k" "25-40k" "> 40k"
#'   ..$ satisfaction: chr [1:4] "VeryD" "LittleD" "ModerateS" "VeryS"
#' }
#'
#' @source Agresti, A. Categorical Data Analysis John Wiley & Sons, 2002, Table
#' 2.8, p. 57.
#' @keywords datasets
#' @examples
#'
#' data(JobSat)
#' assocstats(JobSat)
#' GKgamma(JobSat)
#'
NULL





# Loglinear Model Utilities
#
#
# @aliases loglin-utilities conditional joint loglin2formula loglin2string
#          markov mutual saturated
# @param nf number of factors for which to generate the model
# @param table a contingency table used only for factor names in the model,
# typically the output from \code{\link[base]{table}} and possibly permuted
# with \code{aperm}
# @param factors names of factors used in the model formula when \code{table}
# is not specified
# @param with For \code{joint} and \code{conditional} models, \code{with}
# gives the indices of the factors against which all others are considered
# jointly or conditionally independent
# @param order For \code{markov}, this gives the order of the Markov chain
# model for the factors.  An \code{order=1} Markov chain allows associations
# among sequential pairs of factors, e.g., \code{[A,B], [B,C], [C,D]} \dots{}.
# An \code{order=2} Markov chain allows associations among sequential triples.
# @param x For the \code{loglin2*} functions, a list of terms in a loglinear
# model, such as returned by \code{conditional}, \code{joint}, \dots{}
# @param env For \code{loglin2formula}, environment in which to evaluate the
# formula
# @param brackets For \code{loglin2string}, characters to use to surround
# model terms.  Either a single character string containing two characters
# (e.g., \code{'[]'} or a character vector of length two.
# @param sep For \code{loglin2string}, the separator character string used for
# factor names within a given model term
# @param collapse For \code{loglin2string}, the character string used between
# terms in the the model string
# @param abbrev For \code{loglin2string}, whether and how to abbreviate the
# terms in the string representation. This has not yet been implemented.
# @return For the main model specification functions, \code{conditional},
# \code{joint}, \code{markov}, \dots{}, the result is a list of vectors
# (terms), where the elements in each vector are the names of the factors. The
# elements of the list are given names \code{term1, term2, \dots{}}.
# @author Michael Friendly
# @seealso \code{\link[stats]{loglin}}, \code{\link[MASS]{loglm}}
# @references These functions were inspired by the original SAS implementation
# of mosaic displays, described in the \emph{User's Guide},
# \url{http://www.datavis.ca/mosaics/mosaics.pdf}
# @keywords models
# EXAMPLES NOT COPIED
# @examples
#
# joint(3, table=HairEyeColor)
# # as a formula or string
# loglin2formula(joint(3, table=HairEyeColor))
# loglin2string(joint(3, table=HairEyeColor))
#
# joint(2, HairEyeColor)  # marginal model for [Hair] [Eye]
#
# # other possibilities
# joint(4, factors=letters, with=1)
# joint(5, factors=LETTERS)
# joint(5, factors=LETTERS, with=4:5)
#
# conditional(4)
# conditional(4, with=3:4)
#
# # use in mosaic displays or other strucplots
# mosaic(HairEyeColor, expected=joint(3))
# mosaic(HairEyeColor, expected=conditional(3))
#
# # use with MASS::loglm
# cond3 <- loglin2formula(conditional(3, table=HairEyeColor))
# cond3 <- loglin2formula(conditional(3))  # same, with factors 1,2,3
# require(MASS)
# loglm(cond3, data=HairEyeColor)
#
# saturated(3, HairEyeColor)
# loglin2formula(saturated(3, HairEyeColor))
# loglin2string(saturated(3, HairEyeColor))
# loglin2string(saturated(3, HairEyeColor), brackets='{}', sep=', ')
#
#
#NULL





# The Logarithmic Series Distribution
#
# The logarithmic series distribution is a long-tailed distribution introduced
# by Fisher etal. (1943) in connection with data on the abundance of
# individuals classified by species.
#
# These functions provide the density, distribution function, quantile
# function and random generation for the logarithmic series distribution with
# parameter \code{prob}.
#
# The logarithmic series distribution with \code{prob} = \eqn{p} has density
# \deqn{ p ( x ) = \alpha p^x / x } for \eqn{x = 1, 2, \dots}, where
# \eqn{\alpha= -1 / \log(1 - p)} and \eqn{0 < p <1}.  Note that counts
# \code{x==2} cannot occur.
#
# @aliases Logseries dlogseries plogseries qlogseries rlogseries
# @param x,q vector of quantiles representing the number of events.
# @param prob parameter for the distribution, \code{0 < prob < 1}
# @param log,log.p logical; if TRUE, probabilities \code{p} are given as
# \code{log(p)}
# @param lower.tail logical; if TRUE (default), probabilities are \eqn{P[X \le
# x]}{P[X <= x]}, otherwise, \eqn{P[X > x]}{P[X > x]}.
# @param p vector of probabilities
# @param max.value maximum value returned by \code{qlogseries}
# @param n number of observations for \code{rlogseries}
# @return \code{dlogseries} gives the density, \code{plogseries} gives the
# distribution function, \code{qlogseries} gives the quantile function, and
# \code{rlogseries} generates random deviates.
#
# %% ~Describe the value returned %% If it is a LIST, use %% \item{comp1
# }{Description of 'comp1'} %% \item{comp2 }{Description of 'comp2'} %% ...
# @author Michael Friendly, using original code modified from the
# \code{gmlss.dist} package by Mikis Stasinopoulos.
# @seealso \code{\link[stats]{Distributions}}, ~~~
# @references \url{https://en.wikipedia.org/wiki/Logarithmic_distribution}
#
# Fisher, R. A. and Corbet, A. S. and Williams, C. B. (1943). The relation
# between the number of species and the number of individuals \emph{Journal of
# Animal Ecology}, 12, 42-58.
# @keywords distribution
# @examples
#
# XL <-expand.grid(x=1:5, p=c(0.33, 0.66, 0.99))
# lgs.df <- data.frame(XL, prob=dlogseries(XL[,"x"], XL[,"p"]))
# lgs.df$p = factor(lgs.df$p)
# str(lgs.df)
#
# require(lattice)
# mycol <- palette()[2:4]
# xyplot( prob ~ x, data=lgs.df, groups=p,
# 	xlab=list('Number of events (k)', cex=1.25),
# 	ylab=list('Probability',  cex=1.25),
# 	type='b', pch=15:17, lwd=2, cex=1.25, col=mycol,
# 	key = list(
# 					title = 'p',
# 					points = list(pch=15:17, col=mycol, cex=1.25),
# 					lines = list(lwd=2, col=mycol),
# 					text = list(levels(lgs.df$p)),
# 					x=0.9, y=0.98, corner=c(x=1, y=1)
# 					)
# 	)
#
#
# # random numbers
# hist(rlogseries(200, prob=.4), xlab='x')
# hist(rlogseries(200, prob=.8), xlab='x')
#
#
# NULL





#' Mammogram Ratings
#'
#' Kundel & Polansky (2003) give (possibly contrived) data on a set of 110
#' mammograms rated by two readers.
#'
#'
#' @name Mammograms
#' @docType data
#' @format
#' A frequency table in matrix form.  The format is:
#' \preformatted{
#'   num [1:4, 1:4] 34 6 2 0 10 8 5 1 2 8 ...
#' - attr(*, "dimnames")=List of 2
#' ..$ Reader2: chr [1:4] "Absent" "Minimal" "Moderate" "Severe"
#' ..$ Reader1: chr [1:4] "Absent" "Minimal" "Moderate" "Severe"
#' }
#'
#' @source
#' Kundel, H. L. & Polansky, M. (2003), "Measurement of Observer
#' Agreement", \emph{Radiology}, \bold{228}, 303-308, Table A1
#' @keywords datasets
#' @examples
#'
#' data(Mammograms)
#' B <- agreementplot(Mammograms, main="Mammogram ratings")
#' # agreement measures
#' B
#' Kappa(Mammograms)
#'
#' ## other displays
#' mosaic(Mammograms, shade=TRUE)
#'
#' sieve(Mammograms, pop = FALSE, shade = TRUE)
#' labeling_cells(text = Mammograms,
#'   gp_text = gpar(fontface = 2, cex=1.75))(as.table(Mammograms))
#'
NULL





#' Mental Impairment and Parents SES
#'
#' A 6 x 4 contingency table representing the cross-classification of mental
#' health status (\code{mental}) of 1660 young New York residents by their
#' parents' socioeconomic status (\code{ses}).
#'
#' @details
#' Both \code{ses} and \code{mental} can be treated as ordered factors or
#' integer scores.  For \code{ses}, 1="High" and 6="Low".
#'
#' @name Mental
#' @docType data
#' @format A data frame frequency table with 24 observations on the following 3
#' variables.
#' \describe{
#'   \item{\code{ses}}{an ordered factor with levels \code{1} < \code{2} < \code{3} < \code{4} < \code{5} < \code{6}}
#'   \item{\code{mental}}{an ordered factor with levels \code{Well} < \code{Mild} < \code{Moderate} < \code{Impaired}}
#'   \item{\code{Freq}}{cell frequency: a numeric vector}
#' }
#'
#' @references
#' Friendly, M. \emph{Visualizing Categorical Data}, Cary, NC: SAS
#' Institute, 2000, Appendix B.7.
#' @source
#' Haberman, S. J.  \emph{The Analysis of Qualitative Data: New
#' Developments}, Academic Press, 1979, Vol. II, p. 375.
#'
#' Srole, L.; Langner, T. S.; Michael, S. T.; Kirkpatrick, P.; Opler, M. K. &
#' Rennie, T. A. C.  \emph{Mental Health in the Metropolis: The Midtown
#' Manhattan Study}, NYU Press, 1978, p. 289
#'
#' @keywords datasets
#' @examples
#'
#' data(Mental)
#' str(Mental)
#' (Mental.tab <- xtabs(Freq ~ ses + mental, data=Mental))
#'
#' # mosaic and sieve plots
#' mosaic(Mental.tab, gp=shading_Friendly)
#' sieve(Mental.tab, gp=shading_Friendly)
#'
#' if(require(ca)){
#'   plot(ca(Mental.tab), main="Mental impairment & SES", lines=TRUE)
#' }
#'
#'
NULL





#' Mice Depletion Data
#'
#' Data from Kastenbaum and Lamphiear (1959). The table gives the number of
#' depletions (deaths) in 657 litters of mice, classified by litter size and
#' treatment.  This data set has become a classic in the analysis of
#' contingency tables, yet unfortunately little information on the details of
#' the experiment has been published.
#'
#'
#' @name Mice
#' @docType data
#' @format A frequency data frame with 30 observations on the following 4
#' variables, representing a 5 x 2 x 3 contingency table.
#' \describe{
#'   \item{\code{litter}}{litter size, a numeric vector}
#'   \item{\code{treatment}}{treatment, a factor with levels \code{A} \code{B}}
#'   \item{\code{deaths}}{number of depletions, a factor with levels \code{0} \code{1} \code{2+}}
#'   \item{\code{Freq}}{cell frequency, a numeric vector}
#' }
#'
#' @references
#' Kastenbaum, M. A. & Lamphiear, D. E. (1959) Calculation of
#' chi-square to calculate the no three-factor interaction hypothesis.
#' \emph{Biometrics}, 15, 107-115.
#' @source
#' Goodman, L. A. (1983) The analysis of dependence in
#' cross-classifications having ordered categories, using log-linear models for
#' frequencies and log-linear models for odds. \emph{Biometrics}, 39, 149-160.
#' @keywords datasets
#' @examples
#'
#' data(Mice)
#' # make a table
#' ftable(mice.tab <- xtabs(Freq ~ litter + treatment + deaths, data=Mice))
#'
#' #library(vcd)
#' vcd::mosaic(mice.tab, shade=TRUE)
#'
#'
NULL





#' Social Mobility data
#'
#' Data on social mobility, recording the occupational category of fathers and
#' their sons.
#'
#'
#' @name Mobility
#' @docType data
#' @format A 2-dimensional array resulting from cross-tabulating 2 variables
#' for 19912 observations. The variable names and their levels are:
#'
#' \tabular{rll}{
#'   No \tab Name \tab Levels \cr
#'   1\tab \code{Son's_Occupation}\tab \code{"UpNonMan", "LoNonMan", "UpManual", "LoManual", "Farm"}\cr
#'     2\tab \code{Father's_Occupation}\tab \code{"UpNonMan", "LoNonMan", "UpManual", "LoManual", "Farm"}\cr
#' }
#'
#' @seealso \code{\link{Glass}}, \code{\link{Hauser79}},
#'      \code{\link{Yamaguchi87}} for other examples of mobility data.
#' @source
#'
#' Falguerolles, A. de and Mathieu, J. R. (1988).  \emph{Proceedings of
#' COMPSTAT 88}, Copenhagen, Denmark, Springer-Verlag.
#'
#' % \cite{FeathermanHauser:78}
#'
#' Featherman, D. L. and Hauser, R. M. Occupations and social mobility in the
#' United States.  \emph{Sociological Microjournal}, 12, Fiche 62. Copenhagen:
#' Sociological Institute.
#' @keywords datasets
#' @examples
#'
#' data(Mobility)
#' Mobility
#'
#' # independence model
#' MASS::loglm(~Father_Occupation + Son_Occupation, data = Mobility)
#'
#' vcd::mosaic(Mobility, shade=TRUE, legend = FALSE)
#'
#'
#'
NULL





#' Publications of PhD Candidates
#'
#' A data set giving the number of publications by doctoral candidates in
#' biochemistry in relation to various predictors, originally from Long (1997).
#'
#' There is a large number of zero counts. Is there evidence for a separate
#' group of non-publishers?
#'
#' In this version of the data set, \code{phdprestige} had been rounded to the
#' nearest integer. A Stata version with the continuous values was subsequently
#' found at \url{https://www.stata-press.com/data/lf2/couart2.dta}
#'
#' @name PhdPubs
#' @docType data
#' @format A data frame with 915 observations on the following 6 variables.
#' \describe{
#'   \item{\code{articles}}{number of articles published in the final three years of PhD studies}
#'   \item{\code{female}}{dummy variable for gender, coded \code{1} for female}
#'   \item{\code{married}}{dummy variable for marital status, coded \code{1} for married}
#'   \item{\code{kid5}}{number of young children, age 5 and under}
#'   \item{\code{phdprestige}}{prestige of the PhD department.  The higher the number the more prestigious the program.}
#'   \item{\code{mentor}}{number of publications by the mentor in the preceeding three years}
#' }
#'
#' @source
#' Long, J. S. (1997). \emph{Regression Models for Categorical and
#' Limited Dependent Variables}, Sage.
#'
#' Long, J. S. & Freese, J. (2006). \emph{Regression Models for Categorical
#' Dependent Variables Using Stata}, 2nd Ed., Stata Press.
#' @keywords datasets
#' @examples
#'
#' data(PhdPubs)
#' # very uninformative
#' hist(PhdPubs$articles,
#'      breaks=0:19, col="pink", xlim=c(0,20),
#'      xlab="Number of Articles")
#'
#' library(vcd)
#' rootogram(goodfit(PhdPubs$articles), xlab="Number of Articles")
#'
#' # compare with negative binomial
#' rootogram(goodfit(PhdPubs$articles, type="nbinomial"),
#' 	xlab="Number of Articles", main="Negative binomial")
#'
#'
#'
NULL





#' Shakespeare's Word Type Frequencies
#'
#' This data set, from Efron and Thisted (1976), gives the number of distinct
#' words types (\code{Freq}) of words that appeared exactly once, twice, etc.
#' up to 100 times (\code{count}) in the complete works of Shakespeare.  In
#' these works, Shakespeare used 31,534 distinct words (types), comprising
#' 884,647 words in total.
#'
#' Efron & Thisted used this data to ask the question, "How many words did
#' Shakespeare know?"  Put another way, suppose another new corpus of works
#' Shakespeare were discovered, also with 884,647 words. How many new word
#' types would appear? The answer to the main question involves contemplating
#' an infinite number of such new corpora.
#'
#' In addition to the words that appear \code{1:100} times, there are 846 words
#' that appear more than 100 times, not listed in this data set.
#'
#' @name ShakeWords
#' @docType data
#' @format A data frame with 100 observations on the following 2 variables.
#' \describe{
#'    \item{\code{count}}{the number of times a word type appeared in Shakespeare's written works}
#'    \item{\code{Freq}}{the number of different words (types) appearing with this count.}
#'   }
#
#' @source
#' Bradley Efron and Ronald Thisted (1976). Estimating the Number of
#' Unseen Species: How Many Words Did Shakespeare Know? \emph{Biometrika}, Vol.
#' 63, No. 3, pp. 435-447,
#' %\url{http://www.jstor.org/stable/2335721}
#' @keywords datasets
#' @examples
#'
#' data(ShakeWords)
#' str(ShakeWords)
#'
#' plot(sqrt(Freq) ~ count, data=ShakeWords)
#'
NULL





#' Passengers on the Titanic
#'
#' Data on passengers on the RMS Titanic, excluding the Crew and some
#' individual identifier variables.
#'
#' There are a number of related versions of the Titanic data, in various
#' formats. This version was derived from \code{ptitanic} in the
#' \pkg{rpart.plot} package, modifying it to remove the \code{Class 'labelled'}
#' attributes for some variables (inherited from Frank Harrell's
#' \code{titanic3} version) which caused problems with some applications,
#' notably \code{ggplot2}.
#'
#' Other versions:
#'
#' \code{\link[datasets]{Titanic}} is the 4-way frequency table of all 2201
#' people aboard the Titanic, including passengers and crew.
#'
#' @name Titanicp
#' @docType data
#' @format A data frame with 1309 observations on the following 6 variables.
#' \describe{
#'   \item{\code{pclass}}{a factor with levels \code{1st} \code{2nd} \code{3rd}}
#'   \item{\code{survived}}{a factor with levels \code{died} \code{survived}}
#'   \item{\code{sex}}{a factor with levels \code{female} \code{male}}
#'   \item{\code{age}}{passenger age in years (or fractions of a year, for children), a numeric vector; age is missing for 263 of the passengers}
#'   \item{\code{sibsp}}{number of siblings or spouses aboard, integer: \code{0:8}}
#'   \item{\code{parch}}{number of parents or children aboard, integer: \code{0:6}}
#' }
#
#' @source
#'
#' The original R source for this dataset was compiled by Frank Harrell and
#' Robert Dawson:
#' \url{https://hbiostat.org/data/repo/titanic.txt},
#' described in more detail in
#' \url{https://hbiostat.org/data/repo/titanic}
#'
#' For this version of the Titanic data, passenger details were deleted,
#' survived was cast as a factor, and the name changed to \code{Titanicp} to
#' minimize confusion with other versions.
#' @keywords datasets
#' @examples
#'
#' data(Titanicp)
#' ## maybe str(Titanicp) ; plot(Titanicp) ...
#'
NULL





#' Toxaemia Symptoms in Pregnancy
#'
#' Brown et al (1983) gave these data on two signs of toxaemia, an abnormal
#' condition during pregnancy characterized by high blood pressure
#' (hypertension) and high levels of protein in the urine.  If untreated, both
#' the mother and baby are at risk of complications or death.
#'
#' The data frame \code{Toxaemia} represents 13384 expectant mothers in
#' Bradford, England in their first pregnancy, who were also classified
#' according to social class and the number of cigarettes smoked per day.
#'
#'
#' @name Toxaemia
#' @docType data
#' @format A data frame in frequency form representing a 5 x 3 x 2 x 2
#' contingency table, with 60 observations on the following 5 variables.
#' \describe{
#'   \item{\code{class}}{Social class of mother, a factor with levels \code{1} \code{2} \code{3} \code{4} \code{5}}
#'   \item{\code{smoke}}{Cigarettes smoked per day during pregnancy, a factor with levels \code{0} \code{1-19} \code{20+}}
#'   \item{\code{hyper}}{Hypertension level, a factor with levels \code{Low} \code{High}}
#'   \item{\code{urea}}{Protein urea level, a factor with levels \code{Low} \code{High}}
#'   \item{\code{Freq}}{frequency in each cell, a numeric vector}
#' }
#'
#' @references Friendly, M.  (2000), \emph{Visualizing Categorical Data}, SAS
#' Institute, Cary, NC, Example 7.15.
#'
#' Friendly, M. and Meyer, D. (2016).  \emph{Discrete Data Analysis with R:
#' Visualization and Modeling Techniques for Categorical and Count Data}.  Boca
#' Raton, FL: Chapman & Hall/CRC. \url{http://ddar.datavis.ca}. Example 10.10.
#' @source
#' Brown, P. J., Stone, J. and Ord-Smith, C. (1983), Toxaemic signs
#' during pregnancy. \emph{JRSS, Series C, Applied Statistics}, 32, 69-72
#' @keywords datasets
#' @examples
#'
#' data(Toxaemia)
#'
#' tox.tab <- xtabs(Freq ~ class + smoke + hyper + urea, Toxaemia)
#' ftable(tox.tab, row.vars=1)
#'
#'
#' # symptoms by smoking
#' mosaic(~smoke + hyper + urea, data=tox.tab, shade=TRUE)
#'
#' # symptoms by social class
#' mosaic(~class + hyper + urea, data=tox.tab, shade=TRUE)
#'
#' # predictors
#' mosaic(~smoke + class, data=tox.tab, shade=TRUE)
#'
#' # responses
#' mosaic(~hyper + urea, data=tox.tab, shade=TRUE)
#'
#' # log odds ratios for urea and hypertension, by class and smoke
#' \dontrun{
#' LOR <-loddsratio(aperm(tox.tab))
#' LOR
#' }
#'
#'
NULL





#' TV Viewing Data
#'
#' This data set \code{TV} comprises a 5 x 11 x 3 contingency table based on
#' audience viewing data from Neilsen Media Research for the week starting
#' November 6, 1995.
#'
#' The original data, \code{tv.dat}, contains two additional networks: "Fox"
#' and "Other", with small frequencies. These levels were removed in the
#' current version. There is also a fourth factor, transition State transition
#' (turn the television Off, Switch channels, or Persist in viewing the current
#' channel). The \code{TV} data here includes only the Persist observations.
#'
#' @name TV
#' @docType data
#' @format A 5 x 11 x 3 array of cell frequencies with the following structure:
#' \preformatted{
#'   int [1:5, 1:11, 1:3] 146 244 233 174 294 151 181 161 183 281 ...
#'   - attr(*, "dimnames")=List of 3
#'   ..$ Day    : chr [1:5] "Monday" "Tuesday" "Wednesday" "Thursday" ...
#'   ..$ Time   : chr [1:11] "8:00" "8:15" "8:30" "8:45" ...
#'   ..$ Network: chr [1:3] "ABC" "CBS" "NBC"
#' }
#'
#' @references
#' Friendly, M. and Meyer, D. (2016).  \emph{Discrete Data Analysis
#' with R: Visualization and Modeling Techniques for Categorical and Count
#' Data}.  Boca Raton, FL: Chapman & Hall/CRC. \url{http://ddar.datavis.ca}.
#'
#' Emerson, John W. Mosaic Displays in S-PLUS: A General Implementation and a
#' Case Study. \emph{Statistical Graphics and Computing Newsletter}, 1998,
#' 9(1), 17--23, \url{http://www.stat.yale.edu/~jay/R/mosaic/v91.pdf}
#'
#' Hartigan, J. A. & Kleiner, B. A Mosaic of Television Ratings. \emph{The
#' American Statistician}, 1984, 38, 32-35.
#'
#' @source The original data, \code{tv.dat}, came from the initial
#' implementation of mosaic displays in R by Jay Emerson (1998). Similar data
#' had been used by Hartigan and Kleiner (1984) as an illustration.
#' @keywords datasets
#' @examples
#'
#' data(TV)
#' structable(TV)
#' doubledecker(TV)
#'
#' # reduce number of levels of Time
#' TV.df <- as.data.frame.table(TV)
#' levels(TV.df$Time) <- rep(c("8:00-8:59", "9:00-9:59", "10:00-10:44"),
#'                           c(4, 4, 3))
#'
#' TV2 <- xtabs(Freq ~ Day + Time + Network, TV.df)
#'
#' # re-label for mosaic display
#' levels(TV.df$Time) <- c("8", "9", "10")
#' # fit mode of joint independence, showing association of Network with Day*Time
#' mosaic(~ Day + Network + Time,
#'   data = TV.df,
#'   expected = ~ Day:Time + Network,
#'   legend = FALSE)
#'
#'
#' # with doubledecker arrangement
#' mosaic(~ Day + Network + Time,
#'   data = TV.df,
#'   expected = ~ Day:Time + Network,
#'   split = c(TRUE, TRUE, FALSE),
#'   spacing = spacing_highlighting,
#'   legend = FALSE)
#'
NULL



#' Student Opinion about the Vietnam War
#'
#' A survey of student opinion on the Vietnam War was taken at the University
#' of North Carolina at Chapel Hill in May 1967 and published in the student
#' newspaper. Students were asked to fill in ballot papers stating which policy
#' out of A,B,C or D they supported. Responses were cross-classified by
#' gender/year.
#'
#' The response categories were:
#' \describe{
#'   \item{\code{A}}{Defeat North Vietnam by widespread bombing and land invasion}
#'   \item{\code{B}}{Maintain the present policy}
#'   \item{\code{C}}{De-escalate military activity, stop bombing and begin negotiations}
#'   \item{\code{D}}{Withdraw military forces Immediately}
#' }
#'
#'
#' For some analyses, it is useful to treat \code{year} as numeric, and
#' possibly assign grad students a value \code{year=7}.
#'
#' @name Vietnam
#' @docType data
#' @format A frequency data frame with 40 observations representing a 2 x 5 x 4
#' contingency table on the following 4 variables.
#' \describe{
#'   \item{\code{sex}}{a factor with levels \code{Female} \code{Male}}
#'   \item{\code{year}}{year of study, an ordered factor with levels
#'     \code{Freshmen}, \code{Sophomore}, \code{Junior}, \code{Senior}, \code{Grad student}}
#'   \item{\code{response}}{a factor with levels \code{A} \code{B} \code{C} \code{D}}
#'   \item{\code{Freq}}{cell frequency, a numeric vector}
#' }
#
#' @references Friendly, M.  (2000), \emph{Visualizing Categorical Data}, SAS
#' Institute, Cary, NC, Example 7.9.
#' @source Aitken, M. etal, 1989, \emph{Statistical Modelling in GLIM}
#' @keywords datasets
#' @examples
#'
#' data(Vietnam)
#' ## maybe str(Vietnam) ; plot(Vietnam) ...
#'
NULL





#' Race and Politics in the 1980 Presidential Vote
#'
#' Data from the 1982 General Social Survey on votes in the 1980 U.S.
#' presidential election in relation to race and political conservatism.
#'
#' The data contains a number of sampling zeros in the frequencies of NonWhites
#' voting for Ronald Reagan.
#'
#' @name Vote1980
#' @docType data
#' @format A frequency data frame representing a 2 x 7 x 2 table, with 28
#' observations on the following 4 variables.
#' \describe{
#'   \item{\code{race}}{a factor with levels \code{NonWhite} \code{White}}
#'   \item{\code{conservatism}}{a factor with levels \code{1} \code{2} \code{3} \code{4} \code{5} \code{6} \code{7},
#'        \code{1}=most liberal, \code{7}=most conservative}
#'   \item{\code{votefor}}{a factor with levels \code{Carter} \code{Reagan}; \code{Carter}represents Jimmy Carter or other.}
#'   \item{\code{Freq}}{a numeric vector}
#' }
#'
#' @references
#' Agresti, A. (1990) \emph{Categorical Data Analysis}, Table 4.12
#' New York: Wiley-Interscience.
#'
#' Friendly, M. (2000) \emph{Visualizing Categorical Data}, Example 7.5 Cary,
#' NC: SAS Institute.
#' @source
#' Clogg, C. & Shockey, J. W. (1988). In Nesselroade, J. R. & Cattell,
#' R. B. (ed.)  Multivariate Analysis of Discrete Data, \emph{Handbook of
#' Multivariate Experimental Psychology}, New York: Plenum Press.
#' @keywords datasets
#' @examples
#'
#' data(Vote1980)
#' fourfold(xtabs(Freq ~ race + votefor + conservatism,
#'   data=Vote1980),
#'   mfrow=c(2,4))
#'
#'
NULL





#' Worker Satisfaction Data
#'
#' Blue collar workers job satisfaction from large scale investigation in
#' Denmark in 1968 (Andersen, 1991).
#'
#'
#' @name WorkerSat
#' @docType data
#' @format A frequency data frame with 8 observations on the following 4
#' variables, representing the 2 x 2 x 2 classification of 715 cases.
#' \describe{
#'   \item{\code{Manage}}{Quality of management, an ordered factor with levels \code{bad} < \code{good}}
#'   \item{\code{Super}}{Supervisor satisfaction, an ordered factor with levels \code{low} < \code{high}}
#'   \item{\code{Worker}}{Worker job satisfaction, an ordered factor with levels \code{low} < \code{high}}
#'   \item{\code{Freq}}{a numeric vector}
#' }
#' @references Andersen, E. B. (1991) Statistical Analysis of Categorical Data,
#' 2nd Ed., Springer-Verlag.
#' @source
#' Originally from \url{https://online.stat.psu.edu/stat504/lesson/10/}
#' @keywords datasets
#' @examples
#'
#' data(WorkerSat)
#'
#' worker.tab <- xtabs(Freq ~ Worker + Super + Manage, data=WorkerSat)
#' fourfold(worker.tab)
#' mosaic(worker.tab, shade=TRUE)
#'
#'
NULL





#' Occupational Mobility in Three Countries
#'
#' Yamaguchi (1987) presented this three-way frequency table, cross-classifying
#' occupational categories of sons and fathers in the United States, United
#' Kingdom and Japan.  This data set has become a classic for models comparing
#' two-way mobility tables across layers corresponding to countries, groups or
#' time (e.g., Goodman and Hout, 1998; Xie, 1992).
#'
#' The US data were derived from the 1973 OCG-II survey; those for the UK from
#' the 1972 Oxford Social Mobility Survey; those for Japan came from the 1975
#' Social Stratification and Mobility survey. They pertain to men aged 20-64.
#'
#' Five status categories -- upper and lower nonmanuals (\code{UpNM},
#' \code{LoNM}), upper and lower manuals (\code{UpM}, \code{LoM}), and
#' \code{Farm}) are used for both fathers' occupations and sons' occupations.
#'
#' Upper nonmanuals are professionals, managers, and officials; lower
#' nonmanuals are proprietors, sales workers, and clerical workers; upper
#' manuals are skilled workers; lower manuals are semi-skilled and unskilled
#' nonfarm workers; and farm workers are farmers and farm laborers.
#'
#' Some of the models from Xie (1992), Table 1, are fit in
#' \code{demo(yamaguchi-xie)}.
#'
#' @name Yamaguchi87
#' @docType data
#' @format A frequency data frame with 75 observations on the following 4
#' variables. The total sample size is 28887.
#'
#' \describe{
#'   \item{\code{Son}}{a factor with levels \code{UpNM} \code{LoNM} \code{UpM} \code{LoM} \code{Farm}}
#'   \item{\code{Father}}{a factor with levels \code{UpNM} \code{LoNM} \code{UpM} \code{LoM} \code{Farm}}
#'   \item{\code{Country}}{a factor with levels \code{US} \code{UK} \code{Japan}}
#'   \item{\code{Freq}}{a numeric vector}
#' }
#'
#' @references Goodman, L. A. and Hout, M. (1998). Statistical Methods and
#' Graphical Displays for Analyzing How the Association Between Two Qualitative
#' Variables Differs Among Countries, Among Groups, Or Over Time: A Modified
#' Regression-Type Approach. \emph{Sociological Methodology}, 28 (1), 175-230.
#'
#' Xie, Yu (1992). The log-multiplicative layer effect model for comparing
#' mobility tables. \emph{American Sociological Review}, 57 (June), 380-395.
#' @source Yamaguchi, K. (1987).  Models for comparing mobility tables: toward
#' parsimony and substance, \emph{American Sociological Review}, vol. 52
#' (Aug.), 482-494, Table 1
#' @keywords datasets
#' @examples
#'
#' data(Yamaguchi87)
#' # reproduce Table 1
#' structable(~ Father + Son + Country, Yamaguchi87)
#' # create table form
#' Yama.tab <- xtabs(Freq ~ Son + Father + Country, data=Yamaguchi87)
#'
#' # define mosaic labeling_args for convenient reuse in 3-way displays
#' largs <- list(rot_labels=c(right=0), offset_varnames = c(right = 0.6),
#'               offset_labels = c(right = 0.2),
#'               set_varnames = c(Son="Son's status", Father="Father's status")
#'              )
#'
#' ###################################
#' # Fit some models & display mosaics
#'
#' # Mutual independence
#' yama.indep <- glm(Freq ~ Son + Father + Country,
#'   data=Yamaguchi87,
#'   family=poisson)
#' anova(yama.indep)
#'
#' mosaic(yama.indep, ~Son+Father, main="[S][F] ignoring country")
#'
#' mosaic(yama.indep, ~Country + Son + Father, condvars="Country",
#'        labeling_args=largs,
#'        main='[S][F][C] Mutual independence')
#'
#' # no association between S and F given country ('perfect mobility')
#' # asserts same associations for all countries
#' yama.noRC <- glm(Freq ~ (Son + Father) * Country,
#'   data=Yamaguchi87,
#'   family=poisson)
#' anova(yama.noRC)
#'
#' mosaic(yama.noRC, ~~Country + Son + Father, condvars="Country",
#'        labeling_args=largs,
#'        main="[SC][FC] No [SF] (perfect mobility)")
#'
#' # ignore diagonal cells
#' yama.quasi <- update(yama.noRC, ~ . + Diag(Son,Father):Country)
#' anova(yama.quasi)
#'
#' mosaic(yama.quasi, ~Son + Father, main="Quasi [S][F]")
#'
#' ## see also:
#' # demo(yamaguchi-xie)
#' ##
#'
NULL



