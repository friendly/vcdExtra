# Plotting logistic regressions with marginal distributions of Y
#
# GOAL: Define a new function, `logist_plot()` for ggplot() of logistic regression model for a single predictor with a binary response.
#       It plots the predicted values
#       from a `glm(y ~ x, family = binomial)` against `x`, assumed here to be quantitative, and adds a representation of the distribution of the cases
#       where `y==0` vs. `y==1` as marginal histograms, points, or even rug plots or histograms.
#       * It can take a `formula =` argument, defaulting to `y ~x`
#       * It can allow various representations of the marginal distributions of the cases for which `y==0` vs. `y==1`: histogram, density plot, jittered points,
#         dot points, ala ggdist::geom_dots(), ...

# Suggested in: Smart et al. (2004), A New Means of Presenting the Results of Logistic Regression
# Bulletin of the Ecological Society of America, 85(3),
# https://esapubs.org/bulletin/backissues/085-3/bulletinjuly2004_2column.htm#tools1
#
# See: How I did this in DDAR Ch. 7 was much nicer for the Donner and other examples. Note that the use of facets or color in such examples
#    give what I call `conditional plots`, where `geom_smooth(family=binomial)` gives predicted values and CIs for the data _within_ each group,
#    rather than for an overall model fitted to all the data.
#    Code for all examples is in: http://ddar.datavis.ca/pages/Rcode/ch07.R
#    Sample plots: Fig 7.7, 7.8 Arthritis data;
#
# See: "C:\Dropbox\Documents\VCDR\ch07\R\donner1.R" for all the Donner examples
#
# See also: vcd::binreg_plot() does something very similar using {grid} graphics. Main arg is a fitted model.
#     "C:\Dropbox\Documents\VCDR\ch07\R\binregplot-MF.R" contains a bunch of examples
#
# See also: popbio::logi.hist.plot(), https://www.rdocumentation.org/packages/popbio/versions/2.8/topics/logi.hist.plot
#     MF improved code in "C:\Dropbox\Documents\VCDR\functions\logi.hist.plot.R"
#
# Implemented below by Scott Chamberlain  in https://recology.info/2012/01/logistic-regression-barplot-fig/
# This is really just a sketch of what I'm after, for one particular version of how to do this.
#
# TODO: Make into a proper, general function so that it takes args x=, y=, data=
# TODO: Get variable labels from data (if labeled) or args xlab=, ylab=
# TODO: Combine these functions in a more general way. An argument, `marginal = c("hist", "points")`

# Define the function
loghistplot  <- function(data) {

  require(ggplot2); require(gridExtra) # load packages

  names(data) <- c('x','y') # rename columns

  # get min and max axis values
  min_x <- min(data$x)
  max_x <- max(data$x)
  min_y <- min(data$y)
  max_y <- max(data$y)

  # get bin numbers
  bin_no <- max(hist(data$x)$counts) + 5

  # create plots
  a <- ggplot(data, aes(x = x, y = y)) +
    theme_bw(base_size=16) +
    geom_smooth(method = "glm", family = binomial, se = TRUE,
                colour='black', linewidth=1.5, alpha = 0.3) +
    #     scale_y_continuous(limits=c(0,1), breaks=c(0,1)) +
    scale_x_continuous(limits=c(min_x,max_x)) +
    opts(panel.grid.major = theme_blank(),
         panel.grid.minor=theme_blank(),
         panel.background = theme_blank()) +
    labs(y = "Probability\n", x = "\nYour X Variable")

  b <- ggplot(data[data$y == unique(data$y)[1], ], aes(x = x)) +
    theme_bw(base_size=16) +
    geom_histogram(fill = "grey") +
    scale_y_continuous(limits=c(0,bin_no)) +
    scale_x_continuous(limits=c(min_x,max_x)) +
    opts(panel.grid.major = theme_blank(),
         panel.grid.minor=theme_blank(),
         axis.text.y = theme_blank(),
         axis.text.x = theme_blank(),
         axis.ticks = theme_blank(),
         panel.border = theme_blank(),
         panel.background = theme_blank()) +
    labs(y='\n', x='\n')

  c <- ggplot(data[data$y == unique(data$y)[2], ], aes(x = x)) +
    theme_bw(base_size=16) +
    geom_histogram(fill = "grey") +
    scale_y_continuous(trans='reverse') +
    scale_y_continuous(trans='reverse', limits=c(bin_no,0)) +
    scale_x_continuous(limits=c(min_x,max_x)) +
    opts(panel.grid.major = theme_blank(),panel.grid.minor=theme_blank(),
         axis.text.y = theme_blank(), axis.text.x = theme_blank(),
         axis.ticks = theme_blank(),
         panel.border = theme_blank(),
         panel.background = theme_blank()) +
    labs(y='\n', x='\n')

  grid.newpage()
  pushViewport(viewport(layout = grid.layout(1,1)))

  vpa_ <- viewport(width = 1, height = 1, x = 0.5, y = 0.5)
  vpb_ <- viewport(width = 1, height = 1, x = 0.5, y = 0.5)
  vpc_ <- viewport(width = 1, height = 1, x = 0.5, y = 0.5)

  print(b, vp = vpb_)
  print(c, vp = vpc_)
  print(a, vp = vpa_)
}


logpointplot  <- function(data) {

  require(ggplot2); require(gridExtra) # load packages

  names(data) <- c('x','y') # rename columns

  # get min and max axis values
  min_x <- min(data$x)
  max_x <- max(data$x)
  min_y <- min(data$y)
  max_y <- max(data$y)

  # create plots
  ggplot(data, aes(x = x, y = y)) +
    theme_bw(base_size=16) +
    geom_point(alpha = 0.5, position = position_jitter(w=0, h=0.02)) +
    geom_smooth(method = "glm", family = "binomial", se = TRUE,
                colour='black', size=1.5, alpha = 0.3) +
    scale_x_continuous(limits=c(min_x,max_x)) +
    opts(panel.grid.major = theme_blank(),
         panel.grid.minor=theme_blank(),
         panel.background = theme_blank()) +
    labs(y = "Probability\n", x = "\nYour X Variable")

}

if (FALSE) {
 # Examples
# loghistplot(mtcars[,c("mpg","vs")])
# loghistplot(movies[,c("rating","Action")])
# logpointplot(mtcars[,c("mpg","vs")])
# logpointplot(movies[,c("rating","Action")])

  data(Donner, package = "vcdExtra")
  loghistplot(Donner[,c("age","survived")])
  logpointplot(Donner[,c("age","survived")])


}
