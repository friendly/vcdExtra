## extend aperm to handle table objects

aperm <- function(a, ...)
	UseMethod("aperm")

aperm.default <- function (a, perm, resize = TRUE, ...) 
{
	if (missing(perm)) 
		perm <- integer(0L)
	.Internal(aperm(a, perm, resize))
}

aperm.table <- function(a, perm, resize=TRUE, keep.class=TRUE, ...)
{
	result <- aperm.default(a, perm, resize=resize)
	if(keep.class) class(result) <- class(a)
	result
}

