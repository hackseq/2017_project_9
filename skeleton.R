#testing functionality of collapseTree
library(ape)
library(phytools)
library(phylobase)

tree_test <- rtree(n = 20) #need phylo class to work with edges as vectors
tree_test_4 <- as(tree_test, "phylo4") #need phylo4 class to get nodes from tips
#tree_test <- read.tree("/projects/kelp_seagrass_18s/tree/filtered_input_tree.tre")
plot(tree_test)



#building code

#read metadata

#read tree

#select tree tips

#from tips to nodes
foreach tip {
  x <- as.numeric(getNode(tree_test_4, "t20", type = c("tip"), missing = c("warn"))[[1]])
  return(x)
}

foreach node {
  get parent from tree_test$edge[,1] where tree_test$edge[,2] is the child
  flag node for collapse on second appearance, do recursively
  return(listofnodestocollapse)
}


tree_test$edge[tree_test$edge[ ,2] == c("13"), 1]

findMRCA(tree_test, tips=c("t20", "t14"))
#select nodes and branch positions based on tree tips
select ancestors of tips, for each ancestor, compare number selected vs number possible, if equal, collapse (set to be collapsed)