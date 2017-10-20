#testing functionality of collapseTree
library(ape)
library(phytools)
library(phylobase)

tree_test <- rtree(n = 20) #need phylo class to work with edges as vectors
tree_test_4 <- as(tree_test, "phylo4") #need phylo4 class to get nodes from tips
#tree_test <- read.tree("/projects/kelp_seagrass_18s/tree/filtered_input_tree.tre")
plot(tree_test)

#prep code
#read metadata

#read tree

#select tree tips
foreach (clade in unique(metadata$rankN)) {
  if (clade %in% cladestoexclude) {
    do nothing
  }
  else {
   tipstocollapse <- metadata$tips[which(metadata$rankN == clade)]
  }
  return(tipstocollapse)  
}


#select nodes and branch positions based on tree tips
#from tips to nodes
foreach tip {
  x <- as.numeric(getNode(tree_test_4, "t20", type = c("tip"), missing = c("warn"))[[1]])
  return(x)
}

#eliminate redundancies, we only want to collapse once for each complete clade we are collapsing
foreach node {
  get parent from tree_test$edge[,1] where tree_test$edge[,2] is the child
  flag node for collapse on second appearance, do recursively
  return(listofnodestocollapse)
}

#for each node, do the collapsing
foreach node in listofnodestocollapse {
  reduced_tree <- splitTree(tree, list(node=NODE, bp=tree$edge.length[which(tree$edge[,2]==nn)]))
  return(reduced_tree)
}

