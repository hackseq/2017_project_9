#testing functionality of collapseTree
library(ape)
library(phytools)
library(phylobase)

tree_test <- rtree(n = 20) #need phylo class to work with edges as vectors
#tree_test <- read.tree("/projects/kelp_seagrass_18s/tree/filtered_input_tree.tre")

#prep code
#read metadata
metadata <- read.delim(file="/projects/git/2017_project_9/testing_files/dummy_metadata.txt", sep="\t", header=T)
#read tree
tree <- read.tree(file.path("/projects/git/2017_project_9/testing_files/", "newick_format_tree.tre"))
#parameters
exclude <- c("Bivalvia", "Corallinophycidae") #two clades from rank5
collapse <- c("rank5")

metadata <- metadata[1:100,]
tree <- drop.tip(tree,tree$tip.label[-match(metadata[,1], tree$tip.label)])


#inputs
metadata <- metadata
tree <- tree
cladestoexclude <- exclude
rankN <- collapse

#make phylo4 object
tree4 <- as(tree, "phylo4") #need phylo4 class to get nodes from tips

#select tree tips
nodes <- c()
tipstocollapse <- c()
for (clade in unique(metadata[[rankN]])) {
  if (clade %in% cladestoexclude) {
    print(paste0("Excluding clade: ", clade))
  }
  else {
   tipstocollapse <- append(tipstocollapse, metadata[which(metadata[[rankN]] == clade),1])
   
   #TODO FIX THIS, THESE TIP LABELS ARE COMING OUT AS INDICES NOT NAMES
  }
}
nodes <- returnNodes(tipstocollapse, tree4)


#select nodes and branch positions based on tree tips
#from tips to nodes
returnNodes <- function(x,y) {
  z <- c()
  for (tip in x) {
    node <- as.numeric(getNode(y, tip, type = c("tip"), missing = c("OK"))[[1]])
    if (node != "NA") {
      z <- append(z, node)
    }
  }
  return(z)
}

#eliminate redundancies, we only want to collapse once for each complete clade we are collapsing
for (node in nodes) {
  #get parent from tree_test$edge[,1] where tree_test$edge[,2] is the child
  #flag node for collapse on second appearance, do recursively
  return(listofnodestocollapse)
}

#for each node, do the collapsing
for (node in listofnodestocollapse) {
  reduced_tree <- splitTree(tree, list(node=node, bp=tree$edge.length[which(tree$edge[,2]==node)]))
  return(reduced_tree)
}

