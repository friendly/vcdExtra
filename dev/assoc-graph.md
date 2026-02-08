# Association graphs for loglinear models


Association graphs for loglinear models represent variables as nodes and their partial associations 
between pairs of variables as edges.
If two variables are not connected by an edge, they are conditionally independent given the other variables in the model.
How can we use this in practice, to understand a model, or how well it fits a given dataset?

## Ideas:

* use this as a visual representation of a loglinear model in a network diagram of nodes and edges representing associations
allowed (fitted) in the model. Allow nodes (variables) to 

Read the lecture slides, C:/Dropbox/Documents/psy6136/lectures/04-Loglin.pdf, slides 41-47

I want to develop this idea into R functions for representing loglinear models as graph structures.
There is a collection of old packages: gRbase, gRim, ... that implement these ideas. 
See: https://people.math.aau.dk/~sorenh/software/gR/

Not clear
