library(vcdExtra)
data(housetasks, package="factoextra")

ht <- housetasks[sort(rownames(housetasks)), sort(colnames(housetasks))]

ht <- as.table(as.matrix(ht))
names(dimnames(ht)) <- c("Task", "Who")

ht

HouseTasks <- ht
row.names(HouseTasks)[1] <- "Breakfast"

save(HouseTasks, file = "data/HouseTasks.RData")

#prompt(HouseTasks, file = "man/HouseTasks.Rd")

spineplot(HouseTasks)

ht.ca <-ca::ca(HouseTasks)

plot(ht.ca, lines=TRUE) 

mosaic(HouseTasks, shade = TRUE,
       labeling = labeling_border(rot_labels = c(45,0, 0, 0), 
                                  offset_label =c(.5,5,0, 0),
                                  varnames = c(FALSE, TRUE),
                                  just_labels=c("center","right"),
                                  tl_varnames = FALSE),
       legend = FALSE)

library(seriation)
order <- seriate(HouseTasks, method = "CA")
order

rownames(HouseTasks)[order[[1]]]
colnames(HouseTasks)[order[[2]]]

HT_perm <- permute(HouseTasks, order, margin=1)

mosaic(HT_perm, shade = TRUE,
       labeling = labeling_border(rot_labels = c(45,0, 0, 0), 
                                  offset_label =c(.5,5,0, 0),
                                  varnames = c(FALSE, TRUE),
                                  just_labels=c("center","right"),
                                  tl_varnames = FALSE),
       legend = FALSE)
