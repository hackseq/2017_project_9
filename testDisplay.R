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
p <- ggtree(tree, layout="circular", branch.length = "none") + ggtitle("Collapsed Tree")
p+ geom_tiplab(aes(angle=angle),size=3, color="blue", label=metadata[match(tree$tip.label, metadata[,1]), 9])+geom_nodepoint(color="red", alpha=1, size=1)

#p+ geom_tiplab(aes(x,y,label=metadata[match(tree$tip.label, metadata[,1]), 9],subset=(abs(angle) < 90), angle=angle,size=3, color="blue", node))+
#  geom_tiplab(aes(x,y,label=metadata[match(tree$tip.label, metadata[,1]), 9],subset=(abs(angle) >=90), angle=angle+180,size=3, color="blue", node))
#    geom_nodepoint(color="red", alpha=1, size=1)
