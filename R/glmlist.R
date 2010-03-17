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

# loglmlist - do the same for loglm objects

loglmlist <- function(...) {
    args  <- list(...);
    lname <- names(args)
    name <- vname <- as.character(sys.call())[-1]
    for (i in 1:length(args)) {
        vname[i] <- if (length(lname) && lname[i] != "") 
            lname[i]
        else name[i]
    }
    names(args) <- vname[1:length(args)]
    is.loglm <- unlist(lapply(args, function(x) inherits(x, "loglm")))
    if (!all(is.loglm)) {
    	warning("Objects ", paste(vname[!is.loglm], collapse=', '), 
              " removed because they are not loglm objects")
    	args <- args[is.loglm]
    }
    class(args) <- "loglmlist"
    return(args);
}

# generic version: named list

nlist <- function(...) {
    args  <- list(...);
    lname <- names(args)
    name <- vname <- as.character(sys.call())[-1]
    for (i in 1:length(args)) {
        vname[i] <- if (length(lname) && lname[i] != "") 
            lname[i]
        else name[i]
    }
    names(args) <- vname[1:length(args)]
    return(args);
}
