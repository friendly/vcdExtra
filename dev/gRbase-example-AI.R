# Load required libraries
install.packages(c("gRbase", "graph", "Rgraphviz")) # Install if needed
library(gRbase)
library(graph) # Required by gRbase for plotting - now defunct

# The UCBAdmissions dataset is built-in.
data(UCBAdmissions)

# 1. Fit the loglinear model for homogeneous association
# Formula: ~ Admit:Gender + Gender:Dept + Admit:Dept
model_formula <- ~ Admit*Gender + Gender*Dept + Admit*Dept

# Note: The loglm() function doesn't directly output the *graph* object,
# but we can specify the graph structure (UG for Undirected Graph) based
# on the model formula.

# 2. Define the Undirected Graph (UG) based on the 2-way interactions
# The graph structure represents the conditional independencies implied by the model.
# In a homogeneous association model, all pairs are associated.
# We define the cliques (maximal complete subgraphs): (Admit, Gender), (Gender, Dept), (Admit, Dept)
# This model implies NO three-way interaction.

# A more direct way to generate a graph object from a formula:
assoc_graph <- grBase::ug(model_formula)

# 3. Plot the association graph
# This requires Rgraphviz, which is installed differently via Bioconductor.
# If Rgraphviz is not an option, the base plot function works within 'graph'.

plot(assoc_graph)

# OR, using the vcd package for a different visualization (mosaic plot):
library(vcd)
# This visually emphasizes the strength of association through residual shading.
mosaic(UCBAdmissions, shade = TRUE, legend = TRUE)
