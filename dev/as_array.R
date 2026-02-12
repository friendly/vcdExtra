#' Convert frequency or case form data into array form
#' 
#' Converts object (`obj`) in frequency or case form into array form. `freq`
#' must be supplied if `obj` is in frequency form.
#' 
#' @param obj object to be converted to array form
#' @param freq If `obj` is in frequency form, this is the name of the frequency column. Leave as `NULL` if `obj` is in case form.
#' @param dims A character vector of table dimensions. If not specified, all variables apart from `freq` will be used as dimensions
#' @return object in array form
#' 
#' @details
#' Unclasses the \code{as_table()} function to return an object in array form.
#' 
#' @examples
#' \dontrun{
#' freqForm <- as.data.frame(HairEyeColor) # Generate frequency form data
#' tidy_freqForm <- as_tibble(HairEyeColor) # Generate tidy frequency form data
#' caseForm <- expand.dft(freqForm) # Generate case form data
#' 
#' as_array(freqForm, freq = "Freq") # frequency -> array form
#' as_array(freqForm) # Warned if forgot freq
#' as_array(caseForm) # case form -> array form
#' 
#' # For specific dimensions
#' as_array(tidy_freqForm, freq = "n", dims = c("Hair", "Eye"))
#' }
#' @export

as_array <- function(obj, freq = NULL, dims = NULL){
  return(unclass(as_table(obj, freq, dims))) # Unclass as_table output
}