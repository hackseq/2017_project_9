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

#pruned tree
p <- ggtree(pruned_tree) + ggtitle("")
png("pruned_tree.png", #name of file to print. can also include relative or absolute path before filename.
    width = 800, height = 1200)# define plot width and height. completely up to user.
p + geom_tiplab(size=3, color="blue", label=pruned_tree$tip.label)
dev.off()

#original tree
p <- ggtree(tree) + ggtitle("")
png("orignal_tree.png", #name of file to print. can also include relative or absolute path before filename.
    width = 800, height = 1200)# define plot width and height. completely up to user.
p + geom_tiplab(size=3, color="blue", label=tree$tip.label)
dev.off()
