#Displaying collapsed Tree

library("ape")
library("ggtree")

tree_test <- rtree(n = 20) #need phylo class to work with edges as vectors


#read metadata
metadata <- read.delim(file="./GitHub/2017_project_9/testing_files/dummy_metadata.txt", sep="\t", header=T)
#read tree
tree <- read.tree(file.path("./GitHub/2017_project_9/testing_files/dummy_metadata.txt", "newick_format_tree.tre"))
#parameters

#randoming tree size to 100
metadata <- metadata[1:100,]
tree <- drop.tip(tree, tree$tip.label[!tree$tip.label %in% metadata[,1]])

#plotting tree
ggplot(tree, aes(x, y)) + geom_tree() + theme_tree()


#changing shape to circular and adding leaves name
#p <- ggtree(tree) + ggtitle("(Phylogram) circular layout")+geom_text2(aes(subset=!isTip, label=node), hjust=-.3)

p <- ggtree(tree) + ggtitle("Phylogram")
#p + geom_tiplab( size=3, color="purple", label=paste0(tree$tip.label, metadata[which(tree$tip.label %in% metadata[,1]), 9]))
p + geom_tiplab( size=3, color="purple", label=metadata[match(tree$tip.label, metadata[,1]), 9])