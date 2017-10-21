#Displaying collapsed Tree

library("ape")
library("ggtree")

tree_test <- rtree(n = 20) #need phylo class to work with edges as vectors

#read metadata
metadata <- read.delim(file="./GitHub/2017_project_9/testing_files/dummy_metadata.txt", sep="\t", header=T)
#read tree
tree <- read.tree(file.path("./GitHub/2017_project_9/testing_files/", "newick_format_tree.tre"))
#parameters

#randoming tree size to 100
metadata <- metadata[1:100,]
tree <- drop.tip(tree, tree$tip.label[!tree$tip.label %in% metadata[,1]])

#plotting tree
ggplot(tree, aes(x, y)) + geom_tree() + theme_tree()


p <- ggtree(tree, layout="circular", branch.length="none") + ggtitle("(Cladogram) circular layout")
p + geom_tiplab(aes(angle=angle),size=3, color="blue", label=metadata[match(tree$tip.label, metadata[,1]), 9])