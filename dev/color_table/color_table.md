# Colored tables for frequency data
#

I'd like to develop a function to display n-way frequency tables as graphic tables with shaded background for each
cell to show the patterns in either the observed frequencies or the residuals from a loglinear model.

The idea is to use the function `vcd::structable()` (or perhaps `stats::ftable()`) to produce a flat representation
of the n-way table, and then color the background using a sequential palette for observed frequencies
or a diverging palette if shading is based on the residuals from a model.

Both `structable()` and `ftable()` have a formula interface to specify which variables should form the rows
and which should form the columns of the flattened form.

For this application, another argument, either `model` or `expected` would specify a loglinear model
in a form acceptable to `MASS::loglm` or `glm(Freq ~ terms, data=, family = poisson)

I'm not sure what package to use for the coloring of tables, but I'd like to pick something that doesn't introduce
too much overhead in the vcdExtra package. Some of the possibilities:

* tinytable: Lightweight, no other dependencies; syntax for coloring a bit awkward

* gt: very full featured; `data_color()`, pretty easy to use, but only works for data.frames. Not sure how to make this work with frequency tables or their flattened versions.

```
HEC <- margin.table(HairEyeColor, 2:1)
library(gt)
library(paletteer)

# Example using the mtcars dataset
mtcars[1:5, 1:5] |>
  gt() |>
  data_color(
    columns = everything(),
    palette = "Reds", # Choose any color palette
    direction = "column"
  )
```

* ggplot2: can use geom_tile(). This seems fairly simple, when the data is in frequency form.
Not sure how to do this using flattened tables.

```
HEC |>
  as.data.frame() |>
  ggplot(aes(x = Hair, y = Eye, fill = Freq)) +
  geom_tile() +
  geom_text(aes(label = Freq), size = 8) +
  scale_fill_gradient(low = "white", high = "red") +
  theme_minimal(base_size = 16)
```  

* there are some heatmap packages


## shading by residuals

There are two images that show what I mean, showing the observed frequencies in a table, but shading
the background according to residuals from the model of complete independence.

![](dev/haireye-residual-shading.png)

![](dev/SexOccHeart-residual-shading.png)


