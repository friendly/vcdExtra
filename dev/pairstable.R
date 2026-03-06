library(vcd)

### Claude fix to pairs_diagonal_mosaic() ###
pairs_diagonal_mosaic <- function(split_vertical = TRUE,
                               margins = unit(0, "lines"),
                               offset_labels = -0.4,
                               offset_varnames = 0,
                               gp = NULL,
                               fill = "grey",
                               labeling = labeling_border, # I (GK) changed this from defaulting to labeling_values
                               alternate_labels = TRUE,
                               counts = NULL, # I (GK) added to make turning off cell counts easier
                               ...) {
  
  ### GK Addition ###
  # Change labeling scheme if `counts` argument used
  if (!is.null(counts)){
    if (counts == FALSE){
      labeling = labeling_border
    }
    else{
      labeling = labeling_values
    }
  }
  ####
  
  
  function(x, i) {
    if (is.function(fill))
      fill <- rev(fill(dim(x)[i]))
    if (is.null(gp))
      gp <- gpar(fill = fill)
    mosaic(margin.table(x, i),
           newpage = FALSE,
           split_vertical = split_vertical,
           margins = margins,
           offset_labels = offset_labels,
           offset_varnames = offset_varnames,
           prefix = "diag",
           gp = gp,
           labeling = labeling,         # now uses the argument, not hardcoded
           labeling_args = list(alternate_labels = alternate_labels),
           ...)
  }
}
class(pairs_diagonal_mosaic) <- "grapcon_generator"
######


# Redefine pairs.table
pairs.table <- function(x, diag_panel = pairs_diagonal_mosaic, ...){
  vcd:::pairs.table(x, diag_panel = diag_panel, ...)
}


#### TESTS ####
if(FALSE){

pred_tab <- margin.table(Hoyt, c("Rank", "Occupation", "Sex"))

pairs(pred_tab, gp = shading_Friendly)

pairs(pred_tab, gp = shading_Friendly, diag_panel_args = list(counts = FALSE))
} 


