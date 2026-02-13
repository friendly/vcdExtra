#' Convert any form (case or table form) into frequency form.
#' 
#' A wrapper for \code{as.data.frame()} that is able to properly handle arrays.
#' Converts object (`obj`) in case or table form into frequency form. The 
#' column containing the frequencies (`freq`) must be supplied if `obj` is 
#' already in frequency form (and you are using this function to select
#' dimensions). Returns a tibble if `tidy` is set to `TRUE`.
#' 
#' @param obj object to be converted to frequency form
#' @param freq If `obj` is already in frequency form, this is the name of the frequency column. If `obj` is in any other form, do not supply an argument (see "Details")
#' @param dims A character vector of dimensions. If not specified, all variables apart from `freq` will be used as dimensions
#' @param tidy returns a tibble if set to TRUE
#' @return object in frequency form.
#' 
#' @details
#' Converts `obj` to a table using \code{as_table()} before converting to
#' frequency form.
#' 
#' @examples
#' \dontrun{
#' data("HairEyeColor")
#' freqForm <- as.data.frame(HairEyeColor) # Generate frequency form data
#' tableForm <- as_table(HairEyeColor) # Generate table form data
#' arrayDat <- as_array(HairEyeColor) # Generate an array
#' caseForm <- as_caseform(HairEyeColor) # Generate case form data
#' 
#' as_freqform(arrayDat) # array -> frequency form
#' as_freqform(tableForm) # table -> frequency form
#' as_freqform(caseForm) # case -> frequency form
#' 
#' # Selecting dimensions (optional)
#' as_freqform(freqForm, freq = "Freq", dims = c("Hair", "Eye"))
#' as_freqform(tableForm, dims = c("Hair", "Eye"))
#' }
#' 
#' @importFrom dplyr as_tibble
#' 
#' @export

as_freqform <- function(obj, freq = NULL, dims = NULL, tidy = TRUE){
  
  tab <- as.data.frame(as_table(obj, freq = freq, dims = dims))
  
  if (tidy){
    tab <- as_tibble(tab)
  }
  
  return(tab)
}