#' Convert frequency or case form data into table form
#' 
#' Converts object (`obj`) in frequency or case form into table form. The 
#' column containing the frequencies (`freq`) must be supplied if `obj` is in 
#' frequency form.
#' 
#' @param obj object to be converted to table form
#' @param freq If `obj` is in frequency form, this is the name of the frequency column. Leave as `NULL` if `obj` is in any other form.
#' @param dims A character vector of dimensions. If not specified, all variables apart from `freq` will be used as dimensions
#' @return object in table form
#' 
#' @details
#' If `obj` was in table form to begin with, it is returned to the user as-is
#' unless dimensions were specified (in which case it returns a table with
#' entries summed over excluded dimensions).
#' 
#' @examples
#' \dontrun{
#' data("HairEyeColor")
#' freqForm <- as.data.frame(HairEyeColor) # Generate frequency form data
#' tidy_freqForm <- as_tibble(HairEyeColor) # Generate tidy frequency form data
#' caseForm <- expand.dft(freqForm) # Generate case form data
#' 
#' as_table(freqForm, freq = "Freq") # frequency -> table form
#' as_table(freqForm) # Warned if forgot freq
#' as_table(tidy_freqForm, freq = "n") # frequency (tibble) -> table form
#' as_table(caseForm) # case form -> table form
#' 
#' # For specific dimensions
#' as_table(tidy_freqForm, freq = "n", dims = c("Hair", "Eye"))
#' }
#' 
#' @importFrom stats reformulate xtabs
#' @export

as_table <- function(obj, freq = NULL, dims = NULL){
  
  # If user supplied a table or array, return it back to them
  if (length(intersect(c("table", "array"), class(obj))) > 0){
    
    tab <- as.table(obj) # Handle arrays
    
    # To include dimensions if specified
    if (!is.null(dims)){
      tab <- margin.table(tab, margin = dims)
    }
    
    return(tab)
  }
  
  
  # If obj is a tibble, convert to data frame
  else if (length(intersect("tbl", class(obj))) > 0){
    obj <- as.data.frame(obj)
  }
  
  if (!is.null(dims)){ # If dims supplied by user, use those
    cols <- dims
  }
  else { # If dims NOT supplied by user, use everything else
    cols <- colnames(obj)
  }
  
  if (!is.null(freq)){  # If freq supplied by user, then... (freq form)
    cols <- cols[cols != freq] # Remove freq column
    tab <- xtabs(reformulate(cols, response = freq), data = obj) # freq ~ cols
  }
  else { # If freq NOT supplied by user, and not array, then... (case form)
    tab <- xtabs(reformulate(cols), data = obj)
    
    # Check if user forgot to supply freq, warn if they potentially forgot
    common <- c("n", "freq", "frequency", "count")
    if (length(intersect(tolower(colnames(obj)), common)) > 0){
      warning("Ensure a value for 'freq' was supplied if your data was in frequency form.")
    }
  }
  return(tab)
}