# collapse a contingency table or ftable by re-assigning levels of table variables
# revised to accept an array also



#' Collapse Levels of a Table
#' 
#' Collapse (or re-label) variables in a a contingency table, array or
#' `ftable` object by re-assigning levels of the table variables.
#' 
#' Each of the \code{\dots{}} arguments must be of the form `variable =
#' levels`, where `variable` is the name of one of the table dimensions,
#' and `levels` is a character or numeric vector of length equal to the
#' corresponding dimension of the table.
#' 
#' @param table A \code{\link[base]{table}}, \code{\link[base]{array}} or
#' \code{\link[stats]{ftable}} object
#' @param \dots A collection of one or more assignments of factors of the table
#' to a list of levels
#' @return A `xtabs` and `table` object, representing the original
#' table with one or more of its factors collapsed or rearranged into other
#' levels.
#' @author Michael Friendly
#' @seealso \code{\link{expand.dft}} expands a frequency data frame to case
#' form.
#' 
#' \code{\link[base]{margin.table}} "collapses" a table in a different way, by
#' summing over table dimensions.
#' @keywords manip attribute
#' @examples
#' 
#' # create some sample data in table form
#' sex <- c("Male", "Female")
#' age <- letters[1:6]
#' education <- c("low", 'med', 'high')
#' data <- expand.grid(sex=sex, age=age, education=education)
#' counts <- rpois(36, 100) 
#' data <- cbind(data, counts)
#' t1 <- xtabs(counts ~ sex + age + education, data=data)
#' structable(t1)
#' 
#' ##                  age   a   b   c   d   e   f
#' ## sex    education                            
#' ## Male   low           119 101 109  85  99  93
#' ##        med            94  98 103 108  84  84
#' ##        high           81  88  96 110 100  92
#' ## Female low           107 104  95  86 103  96
#' ##        med           104  98  94  95 110 106
#' ##        high           93  85  90 109  99  86
#' 
#' 
#' # collapse age to 3 levels
#' t2 <- collapse.table(t1, age=c("A", "A", "B", "B", "C", "C"))
#' structable(t2)
#' 
#' ##                  age   A   B   C
#' ## sex    education                
#' ## Male   low           220 194 192
#' ##        med           192 211 168
#' ##        high          169 206 192
#' ## Female low           211 181 199
#' ##        med           202 189 216
#' ##        high          178 199 185
#' 
#' 
#' # collapse age to 3 levels and pool education: "low" and "med" to "low"
#' t3 <- collapse.table(t1, age=c("A", "A", "B", "B", "C", "C"), 
#'     education=c("low", "low", "high"))
#' structable(t3)
#' 
#' ##                  age   A   B   C
#' ## sex    education                
#' ## Male   low           412 405 360
#' ##        high          169 206 192
#' ## Female low           413 370 415
#' ##        high          178 199 185
#' 
#' 
#' 
#' # change labels for levels of education to 1:3
#' t4 <- collapse.table(t1,  education=1:3)
#' structable(t4)
#' 
#' structable(t4)
#' ##                  age   a   b   c   d   e   f
#' ## sex    education                            
#' ## Male   1             119 101 109  85  99  93
#' ##        2              94  98 103 108  84  84
#' ##        3              81  88  96 110 100  92
#' ## Female 1             107 104  95  86 103  96
#' ##        2             104  98  94  95 110 106
#' ##        3              93  85  90 109  99  86
#' 
#' 
#' 
#' 
#' @export collapse.table
collapse.table <- function(table, ...) {
	nargs <- length(args <- list(...))
	if (!nargs) 
		return(table)
	if (inherits(table, "ftable"))
		table <- as.table(table)
	if (inherits(table, "array"))
		table <- as.table(table)
	if (inherits(table, "table")) {
		tvars <- names(dimnames(table))
		table <- as.data.frame.table(table)
		freq <- table[,"Freq"]
	}
	else stop("Argument must be a table, array or ftable object")
	
	names <- names(args)
	for (i in 1:nargs) {
		vals <- args[[i]]
		nm <- names[[i]]
		if(any(nm==tvars)) levels(table[[nm]]) <- vals
		else warning(nm, " is not among the table variables.")
	}
#    term <- paste(tvars, collapse = '+')
#    form <- as.formula(paste("freq ~", term))
#    cat("term: ", term, "\n")
	xtabs(as.formula(paste("freq ~", paste(tvars, collapse = '+'))), data=table)
}
