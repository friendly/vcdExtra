# From: Beh, 2022 Features of the Polynomial Biplot for Ordered Contingency Tables

desc <- "
Table 1 is the twoway
contingency table formed from the cross-classification of
the number of years of occupational exposure to asbestos and
the diagnosed severity of asbestosis of 1117 New York workers;
asbestosis is a chronic lung disease that results in the lung
tissue being scared due to contact with the fibers which can
lead to severe breathing difficulties. 

The data summarized in
Table 1 was originally studied by Beh and Smith (2011b) and
comes fromthe original data collected and published by Selikoff
(1981) who examined the link between asbestos exposure and
asbestosis severity in 1963.

Beh, E. J. & Lombardo, R. (2022) Features of the Polynomial Biplot for
Ordered Contingency Tables, Journal of Computational and Graphical Statistics, 31:2, 403-412,
DOI: 10.1080/10618600.2021.1990773

Beh, E. J., and D. R. Smith (2011a), Real World Occupational Epidemiology,
Part 1: Odds Ratios, Relative Risk, and Asbestosis, Archives of
Environmental & Occupational Health, 66, 119?123.

Beh, E. J., and D. R. Smith (2011b), “Real World Occupational Epidemiology, Part 2: A Visual
Interpretation of Statistical Significance,” Archives of Environmental &
Occupational Health, 66, 245–248.


Selikoff, I. J. (1981), ?Household Risks With Inorganic Fibers,? Bulletin of
the New York Academy of Medicine, 57, 947?961. [

Other references:

Beh, E. J. (1997), ?Simple Correspondence Analysis of Ordinal Cross-
Classifications Using Orthogonal Polynomials,? Biometrical Journal, 39,
589?613. 

Beh, E. J. (1998), ?A Comparative Study of Scores for Correspondence
Analysis With Ordered Categories,? Biometrical Journal, 40, 413?429.

"

Asbestos <- matrix(c(310, 212, 21, 25, 7, 
                      36, 158, 35, 102, 35, 
                      0, 9, 17, 49, 51, 
                      0, 0, 4, 18, 28), nrow = 5)
dimnames(Asbestos) <- list(exposure = c("0-9", "10-19", "20-29", "30-39", "40+"), 
                           grade = paste(c("None", "Grade 1", "Grade 2", "Grade 3")))

Asbestos

save(Asbestos, file = "data/Asbestos.RData")

prompt(Asbestos, filename = "man/Asbestos.Rd")

# --------------------------

# examples

vcd::mosaic(Asbestos, shade=TRUE, legend=FALSE)

library(ca)
Asbestos.ca <- ca(Asbestos) 

plot(Asbestos.ca, lines=TRUE) 

# convert to data.frame
Asbestos.df <- as.data.frame(as.table(Asbestos))

library(dplyr)
Asbestos.df <- Asbestos.df |>
  mutate(exposure = ordered(exposure),
         grade = ordered(grade),
         Escore <- as.numeric(exposure),
         Gscore <- as.numeric(grade))
str(Asbestos.df)

        