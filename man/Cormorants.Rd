\name{Cormorants}
\alias{Cormorants}
\docType{data}
\title{
Advertising Behavior by Males Cormorants
}
\description{
Male double-crested cormorants use advertising behavior to attract females for breeding.
In this study by Meagan Mc Rae (2015), cormorants were observed two or three times a week
at six stations in a tree-nesting colony
for an entire season, April 10, 2014-July 10, 2014.
The number of advertising birds was counted and these observations were classified
by characteristics of the trees and nests.

The goal is to determine how this behavior varies temporally over the season and spatially,
as well as with characteristics of nesting sites.
}
\usage{data("Cormorants")}
\format{
  A data frame with 343 observations on the following 8 variables.
  \describe{
    \item{\code{category}}{Time of season, divided into 3 categories based on breeding chronology, an ordered factor with levels \code{Pre} < \code{Incubation} < \code{Chicks Present}}
    \item{\code{week}}{Week of the season}
    \item{\code{station}}{Station of observations on two different peninsulas in a park, a factor with levels \code{B1} \code{B2} \code{C1} \code{C2} \code{C3} \code{C4}}
    \item{\code{nest}}{Type of nest, an ordered factor with levels \code{no} < \code{partial} < \code{full}}
    \item{\code{height}}{Relative height of bird in the tree, an ordered factor with levels \code{low} < \code{mid} < \code{high}}
    \item{\code{density}}{Number of other nests in the tree, an ordered factor with levels \code{zero} < \code{few} < \code{moderate} < \code{high}}
    \item{\code{tree_health}}{Health of the tree the bird is advertising in, a factor with levels \code{dead} \code{healthy}}
    \item{\code{count}}{Number of birds advertising, a numeric vector}
  }
}
%\details{
%%%  ~~ If necessary, more details than the __description__ above ~~
%}
\source{
Mc Rae, M. (2015)
Spatial and Temporal Changes in Double-Crested Cormorant (Phalacrocorax auritus) Advertising Display through the Nesting Season.
Unpublished MA thesis, York University.
}
%\references{
%%%  ~~ possibly secondary sources and usages ~~
%}
\examples{
data(Cormorants)
str(Cormorants)

library(ggplot2)
ggplot(Cormorants, aes(count)) + geom_histogram(binwidth=0.5) + 
	labs(x="Number of birds advertising")

# Quick look at the data, on the log scale, for plots of `count ~ week`, stratified by something else.
library(ggplot2)
ggplot(Cormorants, aes(week, count, color=height)) + geom_jitter() +
	stat_smooth(method="loess", size=2) + scale_y_log10(breaks=c(1,2,5,10)) +
	geom_vline(xintercept=c(4.5, 9.5))

# ### models using week 
fit1 <-glm(count ~ week + station + nest + height + density + tree_health, data=Cormorants,
    family =  poisson)

library(car)
Anova(fit1)
# plot fitted effects
library(effects)
plot(allEffects(fit1))

}
\keyword{datasets}
