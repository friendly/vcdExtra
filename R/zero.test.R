# Score test for zero inflation in Poisson data
#https://stats.stackexchange.com/questions/118322/how-to-test-for-zero-inflation-in-a-dataset

# References: 
# Broek, Jan van den. 1995. ?A Score Test for Zero Inflation in a Poisson Distribution.? Biometrics 51 (2): 738?43. doi:10.2307/2532959. 
# Yang, Zhao, James W. Hardin, and Cheryl L. Addy. 2010. ?Score Tests for Zero-Inflation in Overdispersed Count Data.? Communications in Statistics - Theory and Methods 39 (11): 2008?30. doi:10.1080/03610920902948228

# Van den Broek, J. (1995). A Score Test for Zero Inflation in a Poisson Distribution. Biometrics, 51(2), 738-743. doi:10.2307/2532959
 


zero.test <- function(x) {

    if(is.table(x)) { # expand to vector of values
        if(length(dim(x)) > 1) stop ("x must be a 1-way table")
        x <- rep(as.numeric(names(x)), unname(c(x)))
    }
		lambda <- mean(x)
		p0_tilde <- exp(-lambda)
		n0 <- sum(1*(!(x >0)))
		n <- length(x)
		numerator <- (n0 - n*p0_tilde)^2
		denominator <- n*p0_tilde*(1-p0_tilde) - n*lambda*(p0_tilde^2)
		stat <- numerator/denominator
		pvalue <- pchisq(stat,df=1, ncp=0, lower.tail=FALSE)
		result <- list(statistic=stat, df=1, prob=pvalue)
		cat(paste("Score test for zero inflation\n\n",
		          "\tChi-square =", round(stat,5), "\n",
		          "\tdf = 1\n",
		          "\tpvalue:", format.pval(pvalue), "\n"))
		invisible(result)
}
