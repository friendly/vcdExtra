#' Horseshoe Crab Mating
#'
#' @description
#' Determinants for male satellites to nesting horseshoe crabs.
#'
#' @name Accident
#' @docType data
#'
#' @format
#' A data frame containing 173 observations on 5 variables.
#' \describe{
#' }
color
Ordered factor indicating color (light medium, medium, dark medium, dark).

spine
Ordered factor indicating spine condition (both good, one worn or broken, both worn or broken).

width
Carapace width (cm).

weight
Weight (kg).

satellites
Number of satellites.

#' @details
#' Brockmann (1996) investigates horshoe crab mating. The crabs arrive on the beach in pairs to spawn. Furthermore, unattached males
#' also come to the beach, crowd around the nesting couples and compete with attached males for fertilizations. These so-called
#' satellite males form large groups around some couples while ignoring others. Brockmann (1996) shows that the groupings are
#' not driven by environmental factors but by properties of the nesting female crabs. Larger females that are in better condition
#' attract more satellites.
#'
#' Agresti (2002, 2013) reanalyzes the number of satellites using count models. Explanatory variables are the female crab's
#' color, spine condition, weight, and carapace width. Color and spine condition are ordered factors but are treated as numeric in some analyses.

#' @source
Table 4.3 in Agresti (2002). This dataset was taken from the \pkg{countreg}, which is not on CRAN

References
Agresti A (2002). Categorical Data Analysis, 2nd ed., John Wiley & Sons, Hoboken.

Agresti A (2013). Categorical Data Analysis, 3rd ed., John Wiley & Sons, Hoboken.

Brockmann HJ (1996). “Satellite Male Groups in Horseshoe Crabs, Limulus polyphemus”, Ethology, 102(1), 1–21.
