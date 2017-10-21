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
   tipstocollapse <- append(tipstocollapse, as.character(metadata[which(metadata[[rankN]] == clade),1]))
  }
  nodes <- append(nodes, returnNodes(tipstocollapse, tree4))
}

# return all nodes to collapse
collapse <- collapse.nodes(nodes)

# a wrapper for getNode
gn.wrapper <- function(node_label, tree) {
  return(as.numeric(getNode(tree, node_label, type = c("tip", missing = c("warn"))[[1]])))
}

get.ancestor <- function(node, edges) {
  return(edges[which(edges[,2] == node)][1])
}

collapse.nodes <- function(nodes) {
  ancestors <- matrix(, nrow = 0, ncol = 3)
  for (i in 1:length(nodes)) {
    node = unlist(nodes[i])
    anc = get.ancestor(node, tree_test$edge)
    if (anc %in% ancestors[,1]) {
      ind <- which(ancestors[,1] == anc)
      ancestors[ind, 2] <- 1
      nodes[nodes == node] <- NA
      nodes[nodes == ancestors[ind, 3]] <- NA
    }
    else {
      ancestors <- rbind(ancestors, c(anc, 0, node))
    }
  }
  nodes <- nodes[!is.na(nodes)]
  ancestors <- ancestors[which(ancestors[,2] == 1)]
  if (length(ancestors) == 0)
    return(nodes)
  else
    return(collapse.nodes(c(ancestors,nodes)))
}

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