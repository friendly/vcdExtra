#' Convert frequency or table form into case form.
#' 
#' Converts object (`obj`) in frequency or table form into case form. The 
#' column containing the frequencies (`freq`) must be supplied if `obj` is in 
#' frequency form. Returns a tibble if `tidy` is set to `TRUE`.
#' 
#' @param obj object to be converted to case form
#' @param freq If `obj` is in frequency form, this is the name of the frequency column. If `obj` is in any other form, do not supply an argument (see "Details")
#' @param dims A character vector of dimensions. If not specified, all variables apart from `freq` will be used as dimensions
#' @param tidy returns a tibble if set to TRUE
#' @return object in case form.
#' 
#' @details
#' A wrapper for \code{expand.dft()} that is able to handle arrays.
#' 
#' If a frequency column is not supplied, this function defaults to "Freq"
#' just like \code{expand.dft()}. Converts `obj` to a table using 
#' \code{as_table()} before converting to case form.
#' 
#' @author Gavin M. Klorfine
#' 
#' @importFrom dplyr as_tibble
#' 
#' @examples
#' library(vcdExtra)
#' 
#' data("HairEyeColor", package = "vcdExtra")
#' 
#' freqForm <- as.data.frame(HairEyeColor) # Generate frequency form data
#' tidy_freqForm <- dplyr::as_tibble(HairEyeColor) # Generate tidy frequency form data
#' tableForm <- as_table(HairEyeColor) # Generate table form data
#' arrayDat <- as_array(HairEyeColor) # Generate an array
#' 
#' # Frequency form -> case form
#' as_caseform(freqForm) |> str()
#' 
#' # Frequency form (tibble) -> case form
#' as_caseform(tidy_freqForm, freq = "n") |> str()
#' 
#' # Array -> case form
#' as_caseform(arrayDat) |> str()
#' 
#' # Optionally specify dims
#' as_caseform(tableForm, dims = c("Hair", "Eye")) |> str()
#'
#' 
#' @export

as_caseform <- function(obj, freq = "Freq", dims = NULL, tidy = TRUE){
  
  tab <- expand.dft(as_table(obj, freq = freq, dims = dims), freq = freq)
  
  if (tidy){
    tab <- dplyr::as_tibble(tab)
  }
  
  return(tab)
}