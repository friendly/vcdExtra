#' Convert frequency, case, or table form data into a matrix.
#' 
#' Converts object (`obj`) in frequency, case or table form into a matrix of
#' specified dimensions (`dims`). The column containing the frequencies (`freq`) 
#' must be supplied if `obj` is in frequency form.
#' 
#' @param obj 
#'  Object to be converted into a matrix.
#' @param freq 
#'  If `obj` is in frequency form, this is the name of the frequency column. 
#'  Leave as `NULL` if `obj` is in any other form.
#' @param dims 
#'  A character vector of dimensions. If not specified, all variables apart from 
#'  `freq` will be used as dimensions.
#' @param prop 
#'  If set to `TRUE`, returns a matrix of proportions (that sum to 1). May also 
#'  be set to a character or numeric vector of dimensions to be used as margins 
#'  from which proportions will be computed.
#' @return Object in matrix form.
#' 
#' @details
#' First converts `obj` into an array using \code{\link{as_array}}. Then a
#' check is made to ensure the user inputted a 2D `obj`. If `obj` is not 2D, an 
#' error is returned. If `obj` is 2D, \code{\link[base]{as.matrix}} is applied.
#' 
#' @author Gavin M. Klorfine
#' 
#' @seealso
#' \code{\link{as_array}}, \code{\link{as_table}}, \code{\link{as_freqform}},
#' \code{\link{as_caseform}}
#' 
#' @examples
#' library(vcdExtra)
#' 
#' data("HairEyeColor")
#' 
#' freqForm <- as.data.frame(HairEyeColor) # Generate frequency form data
#' tidy_freqForm <- dplyr::as_tibble(HairEyeColor) # Generate tidy frequency form data
#' caseForm <- expand.dft(freqForm) # Generate case form data
#' arrayDat <- as_array(HairEyeColor) # Generate an array
#' 
#' # Table form -> matrix
#' as_matrix(HairEyeColor, dims = c("Hair", "Sex")) |> str()
#' 
#' # Frequency form -> matrix
#' as_matrix(freqForm, freq = "Freq", dims = c("Hair", "Sex")) |> str()
#' 
#' # Case form -> matrix form
#' as_matrix(caseForm, dims = c("Hair", "Sex")) |> str()
#' 
#' # Frequency (tibble) form -> matrix form
#' as_matrix(tidy_freqForm, freq = "n", dims = c("Hair", "Sex")) |> str()
#' 
#' #-----For proportions-----#
#' 
#' # Proportions relative to grand total
#' as_matrix(HairEyeColor, dims = c("Hair", "Sex"), prop = TRUE)
#' 
#' # Marginalize proportions along "Sex" (i.e., male proportions sum to 1, 
#' # female proportions sum to 1)
#' as_matrix(HairEyeColor, dims = c("Hair", "Sex"), prop = "Sex")
#' 
#' as_matrix(HairEyeColor, dims = c("Hair", "Sex"), prop = 2) # Same as above
#' 
#' 
#' @export

as_matrix <- function(obj, freq = NULL, dims = NULL, prop = NULL){
  
  tab <- as_array(obj, freq = freq, dims = dims, prop = prop)
  
  if (length(dim(tab)) == 2){ # If number of dimensions equal 2
    return(as.matrix(tab))
  }
  else{
    stop("Please supply an object with two dimensions. You may use the `dims` argument.")
  }
}