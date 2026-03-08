#' Convert frequency or case form data into table form
#' 
#' Converts object (`obj`) in frequency or case form into table form. The 
#' column containing the frequencies (`freq`) must be supplied if `obj` is in 
#' frequency form. Optionally returns a table of proportions with (optionally) specified margins.
#' 
#' @param obj object to be converted to table form
#' @param freq If `obj` is in frequency form, this is the name of the frequency column. Leave as `NULL` if `obj` is in any other form.
#' @param dims A character vector of dimensions. If not specified, all variables apart from `freq` will be used as dimensions
#' @param prop If set to TRUE, returns a table of proportions. May also be set to a character or numeric vector of margins.
#' @return object in table form
#' 
#' @details
#' If `obj` was in table form to begin with, it is returned to the user as-is
#' unless dimensions were specified (in which case it returns a table with
#' entries summed over excluded dimensions). When `prop` is set to `TRUE`, the 
#' returned table will have proportions that sum to one, whereas if a character 
#' or numerical vector of table dimensions is supplied to `prop`, proportions 
#' will be marginalized across the specified dimensions.
#' 
#' @author Gavin M. Klorfine
#' 
#' @seealso
#' \code{\link{as_freqform}}, \code{\link{as_caseform}}, \code{\link{as_array}},
#' \code{\link{as_matrix}}
#' 
#' @importFrom stats reformulate xtabs
#' @importFrom methods is
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
#' # Frequency form -> table form
#' as_table(freqForm, freq = "Freq") |> str()
#' 
#' # Warned if forgot to specify freq
#' as_table(freqForm) |> str()
#' 
#' # Frequency form (tibble) -> table form
#' as_table(tidy_freqForm, freq = "n") |> str()
#' 
#' # Case form -> table form
#' as_table(caseForm) |> str()
#' 
#' # For specific dimensions
#' as_table(tidy_freqForm, freq = "n", dims = c("Hair", "Eye")) |> str()
#' 
#' #-----For proportions-----#
#' 
#' as_table(freqForm, freq = "Freq", prop = TRUE) |> head(c(4,4,1)) # print only Sex == Male rows
#' 
#' # Marginalize proportions along "Sex" (i.e., male proportions sum to 1, female proportions sum to 1)
#' as_table(freqForm, freq = "Freq", prop = "Sex") |> head(c(4,4,1))
#' 
#' as_table(freqForm, freq = "Freq", prop = 3) |> head(c(4,4,1)) # Same as above
#' 
#' # Marginalize proportions along multiple variables
#' as_table(freqForm, freq = "Freq", prop = c("Hair", "Sex")) |> head(c(4,4,1))
#' 
#' as_table(freqForm, freq = "Freq", prop = c(1, 3)) |> head(c(4,4,1)) # Same as above
#' 
#' # Using dims and prop arguments in tandem
#' as_table(freqForm, freq = "Freq", dims = c("Hair", "Eye"), prop = TRUE)
#' 
#' 
#' @export

as_table <- function(obj, freq = NULL, dims = NULL, prop = NULL){
  
  # If user supplied a table or array, remember that
  tab_or_array <- FALSE
  if (is(obj, "table") || is(obj, "array")){
    
    tab <- as.table(obj) # Handle arrays
    
    # To include dimensions if specified
    if (!is.null(dims)){
      tab <- margin.table(tab, margin = dims)
    }
    
    tab_or_array <- TRUE
  }
  # If obj is a tibble, convert to data frame
  else if (is(obj, "table")){
    obj <- as.data.frame(obj)
  }
  
  if (!is.null(dims)){ # If dims supplied by user, use those
    cols <- dims
  }
  else { # If dims NOT supplied by user, use everything else
    cols <- colnames(obj)
  }
  
  if (!tab_or_array){ # If not a table or array...
    if (!is.null(freq)){  # If freq supplied by user, then... (freq form)
      cols <- cols[cols != freq] # Remove freq column
      tab <- xtabs(reformulate(cols, response = freq), data = obj) # freq ~ cols
    }
    else if (is.null(freq)){ # If freq NOT supplied by user, and not array, then... (case form)
      tab <- xtabs(reformulate(cols), data = obj)
      
      # Check if user forgot to supply freq, warn if they potentially forgot
      common <- c("n", "freq", "frequency", "count")
      if (length(intersect(tolower(colnames(obj)), common)) > 0){
        warning("Ensure a value for 'freq' was supplied if your data was in frequency form.")
      }
    }
  }
  
  
  if (!is.null(prop)){ # If user wants proportions
    
    if (is(prop, "logical") && prop == TRUE){ # If margins not specified
      tab <- prop.table(tab)
    }
    else if (is.character(prop) || is.numeric(prop)){ # If proportions are to be marginal
      
      ### Make sure margins are not problematic ###
      if (length(prop) > length(dim(tab))){ # Raise error if # of margin dims exceeds actual # of dims
        stop("Number of specified margins in `prop` exceeds number of dims in table.")
      }
      if (length(unique(prop)) != length(prop)){ # Make sure prop margins are unique
        stop("`prop` margins are not unique (i.e., a duplicate was provided).")
      }
      # If character vector for proportion margins, make sure margins in prop are a subset of dims in table
      if (is.character(prop) && length(prop) != length(intersect(prop, names(dimnames(tab))))){ 
        stop("Ensure all margins specified in `prop` are dims in the table.")
      }
      # If numeric vector for proportion margins, 
      else if(is.numeric(prop)){
        if (!all(prop > 0) || !all(prop %% 1 == 0)){ # Make sure margins in prop are positive, whole nums
          stop("Ensure all margins specified in `prop` are positive whole numbers.")
        }
        if (!all(prop <= length(dim(tab)))){ # Make sure margins in prop do not exceed # of dims in table
          stop("Ensure all margins specified in `prop` do not exceed the total number of dims in the table.")
        }
      }
      ### 
      
      tab <- prop.table(tab, margin = prop)
    }
    else{ # If prop not TRUE or a numeric / character vector
      stop("Argument `prop` must be supplied with either a numeric or character vector")
    }
  }
  
  
  return(tab)
}