# Cochran-Mantel-Haenszel tests for ordinal factors in contingency tables

# The code below follows Stokes, Davis & Koch, (2000). 
#   "Categorical Data Analysis using the SAS System", 2nd Ed.,
#   pp 74--75, 92--101, 124--129.

# Ref: Landis, R. J., Heyman, E. R., and Koch, G. G. (1978), 
#		Average Partial Association in Three-way Contingency Tables: 
#		A Review and Discussion of Alternative Tests, 
#		International Statistical Review, 46, 237–254. 

# See: https://onlinecourses.science.psu.edu/stat504/book/export/html/90
# http://support.sas.com/documentation/cdl/en/statug/63033/HTML/default/viewer.htm#statug_freq_a0000000648.htm

# DONE: this should be the main function, handling 2-way & higher-way tables
#  With strata, use apply() or recursion over strata

CMHtest <- function(x, strata = NULL, rscores=1:R, cscores=1:C, 
	types=c("cor", "cmeans", "rmeans", "general"),
	overall=FALSE, details=overall,  ...)
{

	snames <- function(x, strata) {
	  	sn <- dimnames(x)[strata]
	  	dn <- names(sn)
	  	apply(expand.grid(sn), 1, function(x) paste(dn, x, sep=":", collapse = "|"))
	}

  ## check dimensions
  L <- length(d <- dim(x))
  if(any(d < 2L)) stop("All table dimensions must be 2 or greater")
  if(L > 2L & is.null(strata)) strata <- 3L:L
  if(is.character(strata)) strata <- which(names(dimnames(x)) == strata)
  if(L - length(strata) != 2L) stop("All but 2 dimensions must be specified as strata.")

  ## rearrange table to put primary dimensions first
  x <- aperm(x, c(setdiff(1:L, strata), strata))

	d <- dim(x)
	R <- d[1]
	C <- d[2]

  # handle strata
  if (!is.null(strata)) {
  	sn <- snames(x, strata)
  	
  	res <- c(apply(x, strata, CMHtest2, rscores=rscores, cscores=cscores, 
			types=types,details=details, ...))
		# DONE: fix names if there are 2+ strata
		names(res) <- sn
		# TODO: Calculate generalized CMH, controlling for strata
		if (overall) {message("overall CMH test not yet implemented")}
		return(res)
	}
	else CMHtest2(x, rscores=rscores, cscores=cscores, 
		types=types,details=details, ...)	
}

# handle two-way case
#  DONE:  now allow rscores/cscores == 'midrank' for midrank scores
#  DONE:  allow rscores/cscores=NULL for unordered factors, where ordinal
#     scores don't make sense
CMHtest2 <- function(x, stratum=NULL, rscores=1:R, cscores=1:C, 
	types=c("cor", "cmeans", "rmeans", "general"),
	details=FALSE, ...) {

	# left kronecker product
	lkronecker <- function(x, y, make.dimnames=TRUE, ...)
		kronecker(y, x, make.dimnames=make.dimnames, ...)
		
	# basic CMH calculation
	cmh <- function(A, V, df) {
		AVA <- A %*% V %*% t(A)
		Q <- t(n-m) %*% t(A) %*% solve(AVA) %*% A %*% (n-m)
		pvalue <- pchisq(Q, df, lower.tail=FALSE)
		c(Q, df, pvalue)
	}
	 
	# midrank scores (modified ridits) based on row/column totals
	midrank <- function (n) {
		cs <- cumsum(n)
		(2*cs - n +1) / (2*(cs[length(cs)]+1))
	}

  L <- length(d <- dim(x))
	R <- d[1]
	C <- d[2]
	
	if (is.character(rscores) && rscores=="midrank") rscores <- midrank(rowSums(x))
	if (is.character(cscores) && cscores=="midrank") cscores <- midrank(colSums(x))

	nt <- sum(x)
	pr <- rowSums(x) / nt
	pc <- colSums(x) / nt
	
	m <- as.vector(nt * outer(pr,pc))  # expected values under independence
	n <- as.vector(x)                  # cell frequencies
	
	V1 <- (diag(pr) - pr %*% t(pr))
	V2 <- (diag(pc) - pc %*% t(pc))
	V <- (nt^2/(nt-1)) * lkronecker(V1, V2, make.dimnames=TRUE)
	
	if (length(types)==1 && types=="ALL") types <- c("general", "rmeans", "cmeans", "cor" )
	types <- match.arg(types, several.ok=TRUE)
	# handle is.null(rscores) etc here
	if (is.null(rscores)) types <- setdiff(types, c("cmeans", "cor"))
	if (is.null(cscores)) types <- setdiff(types, c("rmeans", "cor"))

	table <- NULL
	if("cor" %in% types) {
		A <- lkronecker( t(rscores), t(cscores) )
		df <- 1
		table <- rbind(table, cmh(A, V, df))
		}
	if("rmeans" %in% types) {
		A <- lkronecker( cbind(diag(R-1), rep(0, R-1)), t(cscores))
		df <- R-1
		table <- rbind(table, cmh(A, V, df))
		}
	if("cmeans" %in% types) {
		A <- lkronecker( t(rscores), cbind(diag(C-1), rep(0, C-1)))
		df <- C-1
		table <- rbind(table, cmh(A, V, df))
		}
	if ("general" %in% types) {
		A <- lkronecker( cbind(diag(R-1), rep(0, R-1)), cbind(diag(C-1), rep(0, C-1)))
		df <- (R-1)*(C-1)
		table <- rbind(table, cmh(A, V, df))
		}


	colnames(table) <- c("Chisq", "Df", "Prob")
	rownames(table) <- types
	xnames <- names(dimnames(x))
	result <- list(table=table, names=xnames, rscores=rscores, cscores=cscores, stratum=stratum )
	if (details) result <- c(result, list(A=A, V=V, n=n, m=m))
	class(result) <- "CMHtest"
	result
}


print.CMHtest <- function(x, digits = max(getOption("digits") - 2, 3), ...) {
	heading <- "Cochran-Mantel-Haenszel Statistics"
	if (!is.null(x$names)) heading <- paste(heading, "for", paste(x$names, collapse=" by "))
	# TODO: determine score types (integer, midrank) for heading

	df <- x$table
	types <- rownames(df)
	labels <- list(cor="Nonzero correlation", rmeans="Row mean scores differ",
			cmeans="Col mean scores differ", general="General association")
	df <- data.frame("AltHypothesis"=as.character(unlist(labels[types])), df)
	cat(heading,"\n\n")
	print(df, digits=digits, ...)

#	# TODO: use print.anova() method, but this screws up the AltHyp
#	attr(df, "heading") <- paste(heading,"\n")
#	class(df) <- c("anova", "data.frame")
#	print(df)
	invisible(x)
}

