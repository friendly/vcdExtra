#' Convert any form (case or table form) into frequency form.
#' 
#' A wrapper for \code{\link[base]{as.data.frame}} that is able to properly handle arrays.
#' Converts object (`obj`) in case or table form into frequency form. The 
#' column containing the frequencies (`freq`) must be supplied if `obj` is 
#' already in frequency form (and you are using this function to select
#' dimensions). Returns a tibble if `tidy` is set to `TRUE`.
#' 
#' @param obj Object to be converted to frequency form.
#' @param freq If `obj` is already in frequency form, this is the name of the frequency column. If `obj` is in any other form, do not supply an argument (see "Details").
#' @param dims A character vector of dimensions. If not specified, all variables apart from `freq` will be used as dimensions.
#' @param prop If set to `TRUE`, the resulting "frequency" column will contain proportions (that sum to 1). May also be set to a character or numeric vector of dimensions to be used as margins from which proportions will be computed. The resulting "frequency" column is renamed to "Prop."
#' @param tidy Returns a tibble if set to `TRUE.`
#' @return Object in frequency form.
#' 
#' @details
#' Converts `obj` to a table using \code{\link{as_table}} before converting to
#' frequency form
#' 
#' @author Gavin M. Klorfine
#' 
#' @seealso
#' \code{\link{as_table}}, \code{\link{as_caseform}}, \code{\link{as_array}},
#' \code{\link{as_matrix}}
#' 
#' @importFrom dplyr as_tibble rename all_of
#' 
#' @examples
#' library(vcdExtra)
#' 
#' data("HairEyeColor")
#' 
#' freqForm <- as.data.frame(HairEyeColor) # Generate frequency form data
#' tableForm <- as_table(HairEyeColor) # Generate table form data
#' arrayDat <- as_array(HairEyeColor) # Generate an array
#' caseForm <- as_caseform(HairEyeColor) # Generate case form data
#' 
#' # array -> frequency form
#' as_freqform(arrayDat) |> str()
#' 
#' # table -> frequency form
#' as_freqform(tableForm) |> str()
#' 
#' # case -> frequency form
#' as_freqform(caseForm) |> str()
#' 
#' # Selecting dimensions (optional)
#' as_freqform(freqForm, freq = "Freq", dims = c("Hair", "Eye")) |> str()
#' 
#' as_freqform(tableForm, dims = c("Hair", "Eye")) |> str()
#' 
#' #-----For proportions-----#
#' 
#' as_freqform(tableForm, prop = TRUE) |> head() # print only Sex == Male rows
#' 
#' # Marginalize proportions along "Sex" (i.e., male proportions sum to 1, female proportions sum to 1)
#' as_freqform(tableForm, prop = "Sex") |> head()
#' 
#' as_freqform(tableForm, prop = 3) |> head() # Same as above
#' 
#' # Marginalize proportions along multiple variables
#' as_freqform(tableForm, prop = c("Hair", "Sex")) |> head()
#' 
#' as_freqform(tableForm, prop = c(1, 3)) |> head() # Same as above
#' 
#' # Using dims and prop arguments in tandem
#' as_freqform(tableForm, dims = c("Hair", "Eye"), prop = TRUE)
#' 
#' @export

as_freqform <- function(obj, freq = NULL, dims = NULL, prop = NULL, tidy = TRUE){
  
  tab <- as.data.frame(as_table(obj, freq = freq, dims = dims, prop = prop))
  
  if (tidy)
    tab <- dplyr::as_tibble(tab)
  
  # Account for new column named "Freq" if freq was NULL
  if (is.null(freq))
    freq <- "Freq"
  
  if (!is.null(prop))
    tab <- tab |> dplyr::rename("Prop" = dplyr::all_of(freq))
  
  return(tab)
}