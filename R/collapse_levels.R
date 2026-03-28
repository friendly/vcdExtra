#' Collapse the levels of a dataset
#' 
#' Collapses the levels of a dataset (of any form) into those specified. May
#' also be used to re-name levels. Ensure argument \code{freq} is supplied
#' should your data be in frequency form (and the frequency column differs in
#' name from default, "Freq").
#'
#' @param x The dataset to be collapsed.
#' @param freq Supply only if your data is in frequency form AND your frequency 
#'  column differs in name from the default ("Freq").
#' @param \dots A collection of one or more assignments of dataset variables to
#'  a list of levels in the format 
#'  \code{new_level = c("old_level_1", "old_level_2", ..., "old_level_n")}.
#' @return The collapsed dataset in its original form (i.e., the initial form of
#'  \code{x}).
#'  
#' @details
#' First converts the object \code{x} into a frequency form data frame. Then, 
#' \code{\link[forcats]{fct_collapse}} is used to collapse variable levels.
#' Next, duplicate rows (an artefact of collapsing) are aggregated via
#' \code{\link[dplyr]{summarise}}. Last, the frequency form data frame is
#' converted back into the initial form of object \code{x}.
#'
#' @author Gavin M. Klorfine
#'
#' @seealso
#' \code{\link[forcats]{fct_collapse}}, \code{\link{collapse.table}}
#' 
#' Tidy conversion functions:
#' \code{link{as_table}}, \code{link{as_freqform}}, \code{link{as_caseform}},
#' \code{link{as_matrix}}, \code{link{as_array}},
#'
#' @importFrom methods is
#' @importFrom rlang set_names .data
#' @importFrom dplyr as_tibble summarise
#' @importFrom forcats fct_collapse
#' 
#' @examples
#' data("HairEyeColor") # Table form data
#' str(HairEyeColor)
#' 
#' collapse_levels(
#'   HairEyeColor,                 # Dataset
#'   Hair = list(                  # List of arguments for first variable
#'     Dark = c("Black", "Brown"), # Collapse "Black" and "Brown" -> "Dark"
#'     Light = c("Blond", "Red")   # Collapse "Blond" and "Red" -> "Light"
#'   ),
#'   Eye = list(                   # List of arguments for second variable
#'     Common = c("Brown"),        # Collapse (rename) "Brown" -> "Common"
#'     Uncommon = c("Blue", "Green", "Hazel")
#'   )
#' ) |> str()
#' 
#' # To illustrate `freq` argument usage, convert Hoyt dataset to frequency form 
#' # (ff) and then rename frequency column to "n"
#' 
#' data("Hoyt", package = "vcdExtra")
#' ff_Hoyt <- as_freqform(Hoyt)
#' names(ff_Hoyt)[length(ff_Hoyt)] <- "n"
#' str(ff_Hoyt)
#' 
#' collapse_levels(
#'   ff_Hoyt,
#'   
#'   # Ensure to supply if data is in frequency form and frequency column name
#'   # differs from "Freq"
#'   freq = "n",
#'   
#'   Occupation = list(
#'     High = c(1, 2),
#'     Middle = 3,
#'     Low = 4,
#'     VeryLow = c(5, 6, 7)
#'   )
#' ) |> str()
#' 
#'
#' @export

collapse_levels <- function(x, freq = "Freq", ...){
  
  argms <- list(...)
  x_class <- NULL
  
  if (is(x, "array"))
    x_class <- "array"
  else if (is(x, "matrix"))
    x_class <- "matrix"
  else if (is(x, "table"))
    x_class <- "table"
  else if (is(x, "tbl"))
    x_class <- "tbl"
  
  
  # Convert to data frame. Preserve new frequency column
  coll_x <- as_freqform(x, freq = freq, tidy = FALSE)
  freq <- "Freq"
  
  
  fact_lvls <- list() # Initialize list
  
  # Iterate through and collapse factors according to specified levels
  for (i in 1:length(names(argms))){
    
    # Gather the specified levels
    for (j in 1:length(argms[[i]])){
      fact_lvls <- append(fact_lvls, list(argms[[i]][[j]]))
    }
    
    # Collapse factor according to specified levels
    coll_x[[names(argms)[i]]] <- forcats::fct_collapse(
      coll_x[[names(argms)[i]]], 
      !!!rlang::set_names(fact_lvls, names(argms[[i]]))
    )
    
    fact_lvls <- list() # Reset list for the next factor
  }
  
  
  # Combine duplicate rows
  coll_x <- coll_x |> dplyr::summarise(Freq = sum(.data[[freq]]), .by = -all_of(freq))
  
  
  # Return the user a collapsed object of same type as their input
  if (!is.null(x_class)){
    if (x_class == "array")
      coll_x <- as_array(coll_x, freq = "Freq")
    else if (x_class == "matrix")
      coll_x <- as_matrix(coll_x, freq = "Freq")
    else if (x_class == "table")
      coll_x <- as_table(coll_x, freq = "Freq")
    else if (x_class == "tbl")
      coll_x <- dplyr::as_tibble(coll_x)
  }
  
  return (coll_x)
}