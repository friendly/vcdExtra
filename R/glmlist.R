# glmlist - make a glmlist object containing a list of fitted glm objects with their names

# borrowing code from Hmisc::llist
glmlist <- function(...) {
    args  <- list(...);
    lname <- names(args)
    name <- vname <- as.character(sys.call())[-1]
    for (i in 1:length(args)) {
        vname[i] <- if (length(lname) && lname[i] != "") 
            lname[i]
        else name[i]
    }
    names(args) <- vname[1:length(args)]
    is.glm <- unlist(lapply(args, function(x) inherits(x, "glm")))
    if (!all(is.glm)) {
    	warning("Objects ", paste(vname[!is.glm], collapse=', '), 
              " removed because they are not glm objects")
    	args <- args[is.glm]
    }
    class(args) <- "glmlist"
    return(args);
}

