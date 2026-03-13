#' @param freq Supply only if your data is in frequency form AND your frequency column differs in name from the default ("Freq").
#'
#' @importFrom methods is
#' @importFrom rlang set_names .data
#' @importFrom dplyr as_tibble summarise

collapse_levels <- function(obj, freq = "Freq", ...){
  
  argms <- list(...)
  obj_class <- NULL
  
  if (is(obj, "array"))
    obj_class <- "array"
  else if (is(obj, "matrix"))
    obj_class <- "matrix"
  else if (is(obj, "table"))
    obj_class <- "table"
  else if (is(obj, "tbl"))
    obj_class <- "tbl"
  
  
  # Convert to data frame. Preserve new frequency column
  coll_obj <- as_freqform(obj, freq = freq, tidy = FALSE)
  freq <- "Freq"
  
  
  fact_lvls <- list() # Initialize list
  
  # Iterate through and collapse factors according to specified levels
  for (i in 1:length(names(argms))){
    
    # Gather the specified levels
    for (j in 1:length(argms[[i]])){
      fact_lvls <- append(fact_lvls, list(argms[[i]][[j]]))
    }
    
    # Collapse factor according to specified levels
    coll_obj[[names(argms)[i]]] <- fct_collapse(
      coll_obj[[names(argms)[i]]], 
      !!!set_names(fact_lvls, names(argms[[i]]))
    )
    
    fact_lvls <- list() # Reset list for the next factor
  }
  
  
  # Combine duplicate rows
  coll_obj <- coll_obj |> dplyr::summarise(Freq = sum(.data[[freq]]), .by = -freq)
  
  
  # Return the user a collapsed object of same type as their input
  if (!is.null(obj_class)){
    if (obj_class == "array")
      coll_obj <- as_array(coll_obj, freq = "Freq")
    else if (obj_class == "matrix")
      coll_obj <- as_matrix(coll_obj, freq = "Freq")
    else if (obj_class == "table")
      coll_obj <- as_table(coll_obj, freq = "Freq")
    else if (obj_class == "tbl")
      coll_obj <- dplyr::as_tibble(coll_obj)
  }
  
  return (coll_obj)
}


data("HairEyeColor")
tList <- collapse_levels(
  HairEyeColor,
  Hair = list(
    Dark = c("Black", "Brown"), 
    Light = c("Blond", "Red")
  ),
  Eye = list(
    Common = c("Brown"),
    Uncommon = c("Blue", "Green", "Hazel")
  )
)
