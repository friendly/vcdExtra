library(vcdExtra)
data("PreSex", package = "vcd")

str(PreSex)


pre_st <- structable(PremaritalSex + ExtramaritalSex ~ MaritalStatus + Gender, 
           data = PreSex) |> 
  print()
str(pre_st)

pre_ft <- ftable(PremaritalSex + ExtramaritalSex ~ MaritalStatus + Gender, 
                 data = PreSex) |>
  print()
str(pre_ft)



class(pre_st)

# as.matrix joins the levels of the `row_vars` and `col_vars` with "_" to create their combinations
as.matrix(pre_st) |> str()


color_table(PreSex, 
            formula = PremaritalSex + ExtramaritalSex ~ MaritalStatus + Gender)