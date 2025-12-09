#' Horseshoe Crab Mating
#'
#' @description
#' Determinants of mating for male satellites to nesting horseshoe crabs.
#' The number of `satellites` is a natural outcome variable. This dataset is useful for exploring various count data models.
#'
#' @name CrabSatellites
#' @docType data
#' @md
#'
#' @format
#' A data frame containing 173 observations on 5 variables.
#' \describe{
#'   \item{`color`}{Ordered factor indicating color (light medium, medium, dark medium, dark).}
#'   \item{`spine`}{Ordered factor indicating spine condition (both good, one worn or broken, both worn or broken).}
#'   \item{`width`}{Carapace width (cm).}
#'   \item{`weight`}{Weight (kg).}
#'   \item{`satellites`}{Number of satellites.}
#' }
#'
#' @details
#' Brockmann (1996) investigates horseshoe crab mating. The crabs arrive on the beach in pairs to spawn. Furthermore, unattached males
#' also come to the beach, crowd around the nesting couples and compete with attached males for fertilizations. These so-called
#' satellite males form large groups around some couples while ignoring others. Brockmann (1996) shows that the groupings are
#' not driven by environmental factors but by properties of the nesting female crabs. Larger females that are in better condition
#' attract more satellites.
#'
#' Agresti (2002, 2013) reanalyzes the number of satellites using count models. Explanatory variables are the female crab's
#' color, spine condition, weight, and carapace width. Color and spine condition are ordered factors but are treated as numeric in some analyses.

#' @source
#' Table 4.3 in Agresti (2002). This dataset was taken from the \pkg{countreg}, which is not on CRAN
#'
#' @references
#' Agresti A (2002). Categorical Data Analysis, 2nd ed., John Wiley & Sons, Hoboken.
#'
#' Agresti A (2013). Categorical Data Analysis, 3rd ed., John Wiley & Sons, Hoboken.

#' Brockmann HJ (1996). “Satellite Male Groups in Horseshoe Crabs, Limulus polyphemus”, Ethology, 102(1), 1–21.
#'
#' @examples
#' # example code
#' ## load data, use ordered factors as numeric, and
#' ## grouped factor version of width
#' data("CrabSatellites", package = "vcdExtra")
#' CrabSatellites <- transform(CrabSatellites,
#'   color = as.numeric(color),
#'   spine = as.numeric(spine),
#'   cwidth = cut(width, c(-Inf, seq(23.25, 29.25), Inf))
#' )
#'
#' ## Agresti, Table 4.4
#' aggregate(CrabSatellites$satellites,
#'           list(CrabSatellites$cwidth), function(x)
#'   round(c(Number = length(x), Sum = sum(x), Mean = mean(x), Var = var(x)), digits = 2))
#'
#' ## Agresti, Figure 4.4
#' plot(tapply(satellites, cwidth, mean) ~ tapply(width, cwidth, mean),
#'   data = CrabSatellites,
#'   ylim = c(0, 6), pch = 19, cex = 1.5,
#'   xlab = "Mean carapace width (cm)",
#'   ylab = "Mean number of satellites")
#'
#' ## More examples: ?countreg::CrabSatellites` has examples of other plots and count data models
NULL
