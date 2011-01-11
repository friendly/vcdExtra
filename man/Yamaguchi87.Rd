\name{Yamaguchi87}
\alias{Yamaguchi87}
\docType{data}
\title{
Occupational Mobility in Three Countries
}
\description{
Yamaguchi (1987) presented this three-way frequency table,
cross-classifying occupational categories of sons and fathers
in the United States, United Kingdom and Japan.  This data set has
become a classic for models comparing two-way mobility tables across
layers corresponding to countries, groups or time (e.g.,
Goodman and Hout, 1998; Xie, 1992).

The US data were derived from the 1973 OCG-II survey; those
for the UK from the 1972 Oxford Social Mobility Survey; those
for Japan came from the 1975 Social Stratification and Mobility
survey. They pertain to men aged 20-64.
}
\usage{data(Yamaguchi87)}
\format{
  A frequency data frame with 75 observations on the following 4 variables. The total sample size is 28887.
  \describe{
    \item{\code{Son}}{a factor with levels \code{UpNM} \code{LoNM} \code{UpM} \code{LoM} \code{Farm}}
    \item{\code{Father}}{a factor with levels \code{UpNM} \code{LoNM} \code{UpM} \code{LoM} \code{Farm}}
    \item{\code{Country}}{a factor with levels \code{US} \code{UK} \code{Japan}}
    \item{\code{Freq}}{a numeric vector}
  }
}
\details{
Five status categories -- upper and lower
nonmanuals (\code{UpNM}, \code{LoNM}), 
upper and lower manuals (\code{UpM}, \code{LoM}), 
and \code{Farm}) are used for both fathers' occupations and
sons' occupations. 

Upper nonmanuals are professionals,
managers, and officials; lower
nonmanuals are proprietors, sales workers, and
clerical workers; upper manuals are skilled
workers; lower manuals are semi-skilled and
unskilled nonfarm workers; and farm workers
are farmers and farm laborers.

Some of the models from Xie (1992), Table 1, are fit in \code{demo(yamaguchi-xie)}.
}
\source{
Yamaguchi, K. (1987). 
Models for comparing mobility tables: toward parsimony and substance, 
\emph{American Sociological Review}, vol. 52 (Aug.), 482-494, Table 1
}
\references{
Goodman, L. A. and Hout, M. (1998).
Statistical Methods and Graphical Displays for Analyzing How the Association Between Two Qualitative Variables Differs Among Countries, Among Groups, Or Over Time: 
A Modified Regression-Type Approach.
\emph{Sociological Methodology}, 28 (1), 175-230.

Xie, Yu (1992).
The log-multiplicative layer effect model for comparing mobility tables.
\emph{American Sociological Review}, 57 (June), 380-395.
}
\examples{
data(Yamaguchi87)
# reproduce Table 1
structable(~ Father + Son + Country, Yamaguchi87)
# create table form
Yama.tab <- xtabs(Freq ~ Son + Father + Country, data=Yamaguchi87)

# define mosaic labeling_args for convenient reuse in 3-way displays
largs <- list(rot_labels=c(right=0), offset_varnames = c(right = 0.6), offset_labels = c(right = 0.2),
              set_varnames = c(Son="Son's status", Father="Father's status") 
             )

###################################
# Fit some models & display mosaics
  
# Mutual independence
yama.indep <- glm(Freq ~ Son + Father + Country, data=Yamaguchi87, family=poisson)
anova(yama.indep)

mosaic(yama.indep, ~Son+Father, main="[S][F] ignoring country")
mosaic(yama.indep, ~Country + Son + Father, condvars="Country",
       labeling_args=largs, 
       main='[S][F][C] Mutual independence') 

# no association between S and F given country ('perfect mobility')
# asserts same associations for all countries
yama.noRC <- glm(Freq ~ (Son + Father) * Country, data=Yamaguchi87, family=poisson)
anova(yama.noRC)
mosaic(yama.noRC, ~~Country + Son + Father, condvars="Country", 
       labeling_args=largs, 
       main="[SC][FC] No [SF] (perfect mobility)")

# ignore diagonal cells
yama.quasi <- update(yama.noRC, ~ . + Diag(Son,Father):Country)
anova(yama.quasi)
mosaic(yama.quasi, ~Son+Father, main="Quasi [S][F]")

## see also:
# demo(yamaguchi-xie)
##
}
\keyword{datasets}
