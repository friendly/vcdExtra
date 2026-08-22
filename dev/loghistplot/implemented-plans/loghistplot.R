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
# [GK: DONE] TODO: Combine these functions in a more general way. An argument, `marginal = c("hist", "points")`

# Define the function
logist_plot <- function(data, marginal, bins=30) {
  
  require(ggplot2); require(gridExtra); require(grid) # load packages
  
  names(data) <- c('x','y') # rename columns
  
  # get min and max axis values
  min_x <- min(data$x)
  max_x <- max(data$x)
  min_y <- min(data$y)
  max_y <- max(data$y)
  
  if (marginal == "hist") {
    
    if (length(bins) != 1L || !is.numeric(bins) || is.na(bins) ||
        !is.finite(bins) || bins < 1 || bins != floor(bins)) {
      stop("`bins` must be one positive whole number.", call. = FALSE)
    }
    
    # get bin numbers
    bin_width <- (max(data$x) - min(data$x)) / bins
    hist_breaks <- seq(min(data$x), max(data$x), length.out = bins + 1)
    hist_counts <- lapply(unique(data$y), function(y) {
      hist(data$x[data$y == y], breaks = hist_breaks, right = FALSE,
           include.lowest = TRUE, plot = FALSE)$counts
    })
    max_count <- max(unlist(hist_counts))
    bin_no <- 4 * max_count
    
    count_ticks <- pretty(c(0, max_count))
    count_ticks <- count_ticks[count_ticks >= 0 & count_ticks <= max_count]
    count_positions <- sort(c(count_ticks / bin_no,
                              1 - count_ticks / bin_no))
    count_labels <- round(bin_no * pmin(count_positions,
                                        1 - count_positions))
    
    # create plots
    a <- ggplot(data, aes(x = x, y = y)) +
      theme_bw(base_size=16) +
      geom_smooth(method = "glm", method.args = list(family = "binomial"), 
                  se = TRUE, colour = 'black', linewidth = 1.5, alpha = 0.3) +
      scale_y_continuous(
        limits = c(0, 1),
        breaks = seq(0, 1, by = 0.2),
        expand = expansion(mult = 0),
        sec.axis = dup_axis(
          breaks = count_positions,
          labels = count_labels,
          name = "Count"
        )
      ) +
      coord_cartesian(xlim = c(min_x, max_x)) +
      theme(panel.grid.major = element_blank(),
            panel.grid.minor = element_blank(),
            panel.background = element_blank(),
            plot.background = element_blank()) +
      labs(y = "Probability\n", x = "\nYour X Variable")
    
    b <- ggplot(data[data$y == unique(data$y)[1], ], aes(x = x)) +
      theme_bw(base_size=16) +
      geom_histogram(fill = "grey", binwidth = bin_width,
                     boundary = min(data$x), closed = "left") +
      scale_y_continuous(
        limits = c(0, bin_no),
        labels = function(z) rep("0.0", length(z)),
        expand = expansion(mult = 0),
        sec.axis = dup_axis(
          breaks = count_ticks,
          labels = count_ticks,
          name = "Count"
        )
      ) +
      coord_cartesian(xlim = c(min_x, max_x)) +
      theme(panel.grid.major = element_blank(),
            panel.grid.minor = element_blank(),
            axis.text = element_text(colour = "transparent"),
            axis.ticks = element_line(colour = "transparent"),
            axis.title = element_text(colour = "transparent"),
            panel.border = element_blank(),
            panel.background = element_blank(),
            plot.background = element_blank()) +
      labs(y = "Probability\n", x = "\nYour X Variable")
    
    c <- ggplot(data[data$y == unique(data$y)[2], ], aes(x = x)) +
      theme_bw(base_size=16) +
      geom_histogram(fill = "grey", binwidth = bin_width,
                     boundary = min(data$x), closed = "left") +
      scale_y_reverse(
        limits = c(bin_no, 0),
        labels = function(z) rep("0.0", length(z)),
        expand = expansion(mult = 0),
        sec.axis = dup_axis(
          breaks = count_ticks,
          labels = count_ticks,
          name = "Count"
        )
      ) +
      coord_cartesian(xlim = c(min_x, max_x)) +
      theme(panel.grid.major = element_blank(), 
            panel.grid.minor = element_blank(),
            axis.text = element_text(colour = "transparent"),
            axis.ticks = element_line(colour = "transparent"),
            axis.title = element_text(colour = "transparent"),
            panel.border = element_blank(),
            panel.background = element_blank(),
            plot.background = element_blank()) +
      labs(y = "Probability\n", x = "\nYour X Variable")
    
    grid.newpage()
    pushViewport(viewport(layout = grid.layout(1,1)))
    
    vpa_ <- viewport(width = 1, height = 1, x = 0.5, y = 0.5)
    vpb_ <- viewport(width = 1, height = 1, x = 0.5, y = 0.5)
    vpc_ <- viewport(width = 1, height = 1, x = 0.5, y = 0.5)
    
    print(b, vp = vpb_)
    print(c, vp = vpc_)
    print(a, vp = vpa_)
  }
  
  else if (marginal == "points") {
    
    # create plots
    ggplot(data, aes(x = x, y = y)) +
      theme_bw(base_size=16) +
      geom_point(alpha = 0.5, position = position_jitter(w=0, h=0.02)) +
      geom_smooth(method = "glm", method.args = list(family = "binomial"), 
                  se = TRUE, colour='black', size=1.5, alpha = 0.3) +
      scale_x_continuous(limits=c(min_x,max_x)) +
      theme(panel.grid.major = element_blank(),
            panel.grid.minor = element_blank(),
            panel.background = element_blank(),
            plot.background = element_blank()) +
      labs(y = "Probability\n", x = "\nYour X Variable")
  }
  
  else {
    stop('`marginal` must be either "hist" or "points"')
  }
}

loghistplot  <- function(data, bins = 30) {

  require(ggplot2); require(gridExtra); require(grid) # load packages

  if (length(bins) != 1L || !is.numeric(bins) || is.na(bins) ||
      !is.finite(bins) || bins < 1 || bins != floor(bins)) {
    stop("`bins` must be one positive whole number.", call. = FALSE)
  }

  names(data) <- c('x','y') # rename columns

  # get min and max axis values
  min_x <- min(data$x)
  max_x <- max(data$x)
  min_y <- min(data$y)
  max_y <- max(data$y)

  # get bin numbers
  bin_width <- (max(data$x) - min(data$x)) / bins
  hist_breaks <- seq(min(data$x), max(data$x), length.out = bins + 1)
  hist_counts <- lapply(unique(data$y), function(y) {
    hist(data$x[data$y == y], breaks = hist_breaks, right = FALSE,
         include.lowest = TRUE, plot = FALSE)$counts
  })
  max_count <- max(unlist(hist_counts))
  bin_no <- 4 * max_count

  count_ticks <- pretty(c(0, max_count))
  count_ticks <- count_ticks[count_ticks >= 0 & count_ticks <= max_count]
  count_positions <- sort(c(count_ticks / bin_no,
                            1 - count_ticks / bin_no))
  count_labels <- round(bin_no * pmin(count_positions,
                                      1 - count_positions))

  # create plots
  a <- ggplot(data, aes(x = x, y = y)) +
    theme_bw(base_size=16) +
    geom_smooth(method = "glm", method.args = list(family = "binomial"), 
                se = TRUE, colour = 'black', linewidth = 1.5, alpha = 0.3) +
    scale_y_continuous(
      limits = c(0, 1),
      breaks = seq(0, 1, by = 0.2),
      expand = expansion(mult = 0),
      sec.axis = dup_axis(
        breaks = count_positions,
        labels = count_labels,
        name = "Count"
      )
    ) +
    coord_cartesian(xlim = c(min_x, max_x)) +
    theme(panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          panel.background = element_blank(),
          plot.background = element_blank()) +
    labs(y = "Probability\n", x = "\nYour X Variable")

  b <- ggplot(data[data$y == unique(data$y)[1], ], aes(x = x)) +
    theme_bw(base_size=16) +
    geom_histogram(fill = "grey", binwidth = bin_width,
                   boundary = min(data$x), closed = "left") +
    scale_y_continuous(
      limits = c(0, bin_no),
      labels = function(z) rep("0.0", length(z)),
      expand = expansion(mult = 0),
      sec.axis = dup_axis(
        breaks = count_ticks,
        labels = count_ticks,
        name = "Count"
      )
    ) +
    coord_cartesian(xlim = c(min_x, max_x)) +
    theme(panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          axis.text = element_text(colour = "transparent"),
          axis.ticks = element_line(colour = "transparent"),
          axis.title = element_text(colour = "transparent"),
          panel.border = element_blank(),
          panel.background = element_blank(),
          plot.background = element_blank()) +
    labs(y = "Probability\n", x = "\nYour X Variable")

  c <- ggplot(data[data$y == unique(data$y)[2], ], aes(x = x)) +
    theme_bw(base_size=16) +
    geom_histogram(fill = "grey", binwidth = bin_width,
                   boundary = min(data$x), closed = "left") +
    scale_y_reverse(
      limits = c(bin_no, 0),
      labels = function(z) rep("0.0", length(z)),
      expand = expansion(mult = 0),
      sec.axis = dup_axis(
        breaks = count_ticks,
        labels = count_ticks,
        name = "Count"
      )
    ) +
    coord_cartesian(xlim = c(min_x, max_x)) +
    theme(panel.grid.major = element_blank(), 
          panel.grid.minor = element_blank(),
          axis.text = element_text(colour = "transparent"),
          axis.ticks = element_line(colour = "transparent"),
          axis.title = element_text(colour = "transparent"),
          panel.border = element_blank(),
          panel.background = element_blank(),
          plot.background = element_blank()) +
    labs(y = "Probability\n", x = "\nYour X Variable")

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
    geom_smooth(method = "glm", method.args = list(family = "binomial"), 
                se = TRUE, colour='black', size=1.5, alpha = 0.3) +
    scale_x_continuous(limits=c(min_x,max_x)) +
    theme(panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          panel.background = element_blank(),
          plot.background = element_blank()) +
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

  logist_plot(Donner[,c("age","survived")], marginal = "hist")
  logist_plot(Donner[,c("age","survived")], marginal = "points")
  
}
