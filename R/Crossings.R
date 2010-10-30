# crossings model (Goodman, 1972)
# Ref:
#Goodman, L. (1972).  Some multiplicative models for the analysis of cross-classified data.
#In: Proceedings of the Sixth Berkeley Symposium on Mathematical Statistics and Probability,
#Berkeley, CA: University of California Press, pp. 649-696.

crossings <- function (n) {
	result <- matrix(0, n*n, n-1)
	for(i in 1:(n-1)) {
	 M11 <- matrix(0, i, i)
	 M12 <- matrix(1, i, n-i)
	 M22 <- matrix(0, n-i, n-i)
	 M <- rbind( cbind(M11, M12),
	             cbind(t(M12), M22))
	 result[,i] <- M
	}
	colnames(result) <- paste('C', 1:(n-1), sep='')
	result
}

Crossings <- function(...) {
    dots <- list(...)
    if (length(dots) !=2) stop("Crossings() is defined for only two factor")
    if (any(diff(sapply(dots, length)) != 0)) 
        stop("arguments to Crossings() must all have same length")
    if (any(diff(sapply(dots, nlevels)) != 0)) 
        stop("arguments to Crossings() must all have same number of levels")
    dots <- lapply(dots, as.factor)
    facMatrix <- sapply(dots, as.character)
    n <- nlevels(dots[[1]])
    result <- crossings(n)
    rownames(result) <- apply(sapply(dots, as.character), 1, paste, collapse='')
    result
}




	