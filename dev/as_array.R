#' Convert frequency, case, or table form data into an array
#' 
#' Converts object (`obj`) in frequency, case or table form into an array. The 
#' column containing the frequencies (`freq`) must be supplied if `obj` is in 
#' frequency form.
#' 
#' @param obj object to be converted to an array
#' @param freq If `obj` is in frequency form, this is the name of the frequency column. Leave as `NULL` if `obj` is in any other form.
#' @param dims A character vector of dimensions. If not specified, all variables apart from `freq` will be used as dimensions
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
#' as_array(freqForm, freq = "Freq") # frequency form -> array
#' as_array(freqForm) # Warned if forgot freq
#' as_array(caseForm) # case form -> array
#' as_table(tidy_freqForm, freq = "n") # frequency (tibble) form -> array
#' 
#' # For specific dimensions
#' as_array(tidy_freqForm, freq = "n", dims = c("Hair", "Eye"))
#' }
#' @export

as_array <- function(obj, freq = NULL, dims = NULL){
  return(unclass(as_table(obj, freq, dims))) # Unclass as_table output
}