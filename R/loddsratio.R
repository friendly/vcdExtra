# Modifications:
# -- return a dnames component, containing dimnames for the array version of coef
# -- added dim methods: dim.loddsratio, dimnames.loddsratio
# -- added print.loddsratio
# -- handle strata:  result computed correctly, but structure of coef() loses names
#    and confint doesn't work in the 2x2xk or RxCxk case

# -- Fixed problem with strata by setting rownames and colnames for contrast matrix
# DONE: handle multiple strata (|foo:bar)
# -- print.loddsratio now uses drop() for dimensions of length 1
# -- made generic, anticipating a formula method, maybe structable or ftable methods
# DONE: decide which methods should allow a log=FALSE argument to provide exp(lor)

# -- Now handle any number of strata
# -- Added log= argument to print, coef methods, and added confint.loddsratio method,
#    allowing log=FALSE

# -- Incorporated Z code additions, fixing some <FIXME>s
# -- Added as.matrix and as.array methods
# -- Added header to print method
# -- Added as.data.frame method (for use in plots)

loddsratio <- function(x, ...)
  UseMethod("loddsratio")


loddsratio.default <- function(x, strata = NULL, log = TRUE,
  ref = NULL, correct = any(x == 0), ...)
{
  ## check dimensions
  L <- length(d <- dim(x))
  if(any(d < 2L)) stop("All table dimensions must be 2 or greater")
  if(L > 2L & is.null(strata)) strata <- 3L:L
  if(is.character(strata)) strata <- which(names(dimnames(x)) == strata)
  if(L - length(strata) != 2L) stop("All but 2 dimensions must be specified as strata.")

  ## dimensions of primary R x C table
  ## <FIXME>
  ## Z: This seems to ignore strata settings different from the defaults
  ## or am I missing something?
  ## M: OK, fixed, though not perhaps elegantly
  ## </FIXME>
  dp <- if (length(strata)) d[-strata] else d
  dn <- if (length(strata)) dimnames(x)[-strata] else dimnames(x)
  R <- dp[1]
  C <- dp[2]
  ## <FIXME>
  ## Z: Computing the sum over a couple of margins should be fast but the computation
  ## is really unnecessary, isn't it? taking a suitable subset, or setting up an
  ## zero matrix with the right dimensions and dimnames should be sufficient.
  ## </FIXME>
#  X <- apply(x, (1:L)[-strata], sum)
	X <- matrix(0, R, C, dimnames=dn)
  
  ## process reference categories (always return list of length
  ## two with reference for rows/cols, respectively)
  if(is.null(ref)) {
    ref <- list(NULL, NULL)
  } else if(is.character(ref)) {
    if(length(ref) != 2L) stop("'ref' must specify both reference categories")
    ref <- list(match(ref[1L], rownames(x)), match(ref[2L], colnames(x)))
  } else if(is.numeric(ref)) {
    ref <- as.integer(rep(ref, length.out = 2L))
    ref <- list(ref[1L], ref[2L])
  }
  
  ## compute corresponding indices
  compute_index <- function(n, ref) {
    if(is.null(ref)) return(cbind(1:(n-1), 2:n))
    rval <- cbind(ref, 1:n)
    d <- rval[,2L] - rval[,1L]
    rval <- rbind(
      rval[d > 0, 1:2],
      rval[d < 0, 2:1]
    )
    return(rval[order(rval[,1L]),])
  }
  Rix <- compute_index(R, ref[[1L]])
  Cix <- compute_index(C, ref[[2L]])
  
  ## set up contrast matrix for the primary R x C table
  contr <- matrix(0L, nrow = (R-1) * (C-1), ncol = R * C)
  colnames(contr) <- paste(rownames(X)[as.vector(row(X))], colnames(X)[as.vector(col(X))], sep = ":")
  rownames(contr) <- rep("", (R-1) * (C-1))
  for(i in 1:(R-1)) for(j in 1:(C-1)) {
    rix <- (j-1) * (R-1) + i
    cix <- rep(Rix[i,], 2L) + R * (rep(Cix[j,], each = 2L) - 1L)
    contr[rix, cix] <- c(1L, -1L, -1L, 1L)
    rownames(contr)[rix] <- sprintf("%s/%s",
      paste(rownames(X)[Rix[i,]], collapse = ":"),
      paste(colnames(X)[Cix[j,]], collapse = ":"))
  }

	# handle strata
  if (!is.null(strata)) {
  	if (length(strata)==1) {
    	sn <- dimnames(x)[[strata]]  
		}
		else {
			sn <- apply(expand.grid(dimnames(x)[strata]), 1, paste, collapse = ":")
		}
    rn <- as.vector(outer( dimnames(contr)[[1]], sn, paste, sep='|'))
    cn <- as.vector(outer( dimnames(contr)[[2]], sn, paste, sep='|'))
    contr <- kronecker(diag(prod(dim(x)[strata])), contr) 
		rownames(contr) <- rn
		colnames(contr) <- cn
		}

  ## dimnames for array version
  dn <- list(rep("", R-1), rep("", C-1))
  for(i in 1:(R-1)) dn[[1]][i] <- paste(rownames(x)[Rix[i,]], collapse = ":")
  for(j in 1:(C-1)) dn[[2]][j] <- paste(colnames(x)[Cix[j,]], collapse = ":")
  if (!is.null(strata)) dn <- c(dn, dimnames(x)[strata])
  if (!is.null(names(dimnames(x)))) names(dn) <- names(dimnames(x))

  ## point estimates
  add <- if(correct) 0.5 else 0
  coef <- drop(contr %*% log(as.vector(x) + add))
  
  ## covariances
  vcov <- crossprod(diag(sqrt(1/(as.vector(x) + add))) %*% t(contr))

  rval <- structure(list(
    coefficients = coef,
    dimnames = dn,
    dim = as.integer(sapply(dn, length)),
    vcov = vcov,
    contrasts = contr,
    log = log
  ), class = "loddsratio")
  rval
}

## dim methods
dimnames.loddsratio <- function(x, ...) x$dimnames
dim.loddsratio <- function(x, ...) x$dim


## straightforward methods
coef.loddsratio <- function(object, log = object$log, ...)
  if(log) object$coefficients else exp(object$coefficients)

vcov.loddsratio <- function(object, log = object$log, ...)
  if(log) object$vcov else diag(exp(object$coefficients)) %*% object$vcov %*% diag(exp(object$coefficients))


confint.loddsratio <- function(object, parm, level=0.95, log=TRUE, ...) {
	if (log) confint.default(object, parm=parm, level=level, ... ) 
	else exp(confint.default(object, parm=parm, level=level, ... ))
}


## print method
print.loddsratio <- function(x, log = x$log, ...) {
	vn <- names(dimnames(x))
	header <- c(if(log) "log" else "",
			"odds ratios for", vn[1], "and", vn[2],
			if (length(vn)>2) c("by", paste(vn[-(1:2)], collapse=', ')), "\n\n")
	cat(paste(header, sep=" "))
	print(drop(array(coef(x, log = log), dim = dim(x), dimnames = dimnames(x)), ...))
	invisible(x)
}


## reshape coef() methods
as.matrix.loddsratio <- function (x, log=x$log, ...) {
	if (length(dim(x))==2) matrix(coef(x, log = log), ncol = dim(x)[2], dimnames=dimnames(x))
	else {
		matrix(coef(x, log = log), ncol = prod(dim(x)[-1]), 
			dimnames=list(dimnames(x)[1], apply(expand.grid(dimnames(x)[-1]), 1, paste, collapse = ":"))) 
	}
}

as.array.loddsratio <- function (x, log=x$log, ...) 
	drop(array(coef(x, log = log), dim = dim(x), dimnames=dimnames(x)))

# <FIXME>
# The LOR column should be OR if log=FALSE
# Should row.names=NULL be supplied as default?
# </FIXME>
#as.data.frame.loddsratio <- function(x, row.names = NULL, optional = FALSE, log=x$log, ...)
as.data.frame.loddsratio <- function(x, ..., log=x$log)
		data.frame(expand.grid(dimnames(x)), 
			LOR = coef(x, log=log),
			ASE = diag(vcov(x, log=log)),  ...
	)
