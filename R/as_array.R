#' Convert frequency, case, or table form data into an array
#' 
#' Converts object (`obj`) in frequency, case or table form into an array. The 
#' column containing the frequencies (`freq`) must be supplied if `obj` is in 
#' frequency form.
#' 
#' @param obj 
#'  Object to be converted to an array.
#' @param freq 
#'  If `obj` is in frequency form, this is the name of the frequency column. 
#'  Leave as `NULL` if `obj` is in any other form.
#' @param dims 
#'  A character vector of dimensions. If not specified, all variables apart from 
#'  `freq` will be used as dimensions.
#' @param prop 
#'  If set to `TRUE`, returns an array of proportions (that sum to 1). May also 
#'  be set to a character or numeric vector of dimensions to be used as margins 
#'  from which proportions will be computed.
#' @return object in array form
#' 
#' @details
#' Unclasses the \code{\link{as_table}} function to return an object in array form.
#' 
#' @author Gavin M. Klorfine
#' 
#' @seealso
#' \code{\link{as_table}}, \code{\link{as_freqform}}, \code{\link{as_caseform}},
#' \code{\link{as_matrix}}
#' 
#' @examples
#' library(vcdExtra)
#' 
#' data("HairEyeColor")
#' 
#' freqForm <- as.data.frame(HairEyeColor) # Generate frequency form data
#' tidy_freqForm <- dplyr::as_tibble(HairEyeColor) # Generate tidy frequency form data
#' caseForm <- expand.dft(freqForm) # Generate case form data
#' 
#' # Frequency form -> array form
#' as_array(freqForm, freq = "Freq") |> str()
#' 
#' # Warned if forgot to specify freq
#' as_array(freqForm) |> str()
#' 
#' # Case form -> array form
#' as_array(caseForm) |> str()
#' 
#' # Frequency (tibble) form -> array form
#' as_array(tidy_freqForm, freq = "n") |> str()
#' 
#' # For specific dimensions
#' as_array(tidy_freqForm, freq = "n", dims = c("Hair", "Eye")) |> str()
#' 
#' #-----For proportions-----#
#' 
#' as_array(freqForm, freq = "Freq", prop = TRUE) |> # proportions relative to grand total
#'   head(c(4,4,1)) 
#' 
#' # Marginalize proportions along "Sex" (i.e., male proportions sum to 1, female proportions sum to 1)
#' as_array(freqForm, freq = "Freq", prop = "Sex") |> head(c(4,4,1))
#' 
#' as_array(freqForm, freq = "Freq", prop = 3) |> head(c(4,4,1)) # Same as above
#' 
#' # Marginalize proportions along multiple variables
#' as_array(freqForm, freq = "Freq", prop = c("Hair", "Sex")) |> head(c(4,4,1))
#' 
#' as_array(freqForm, freq = "Freq", prop = c(1, 3)) |> head(c(4,4,1)) # Same as above
#' 
#' # Using dims and prop arguments in tandem
#' as_array(freqForm, freq = "Freq", dims = c("Hair", "Eye"), prop = TRUE)
#' 
#' 
#' @export

as_array <- function(obj, freq = NULL, dims = NULL, prop = NULL){
  return(unclass(as_table(obj, freq, dims, prop))) # Unclass as_table output
}