#testing functionality of collapseTree
library(ape)
library(phytools)
library(phylobase)

#prep code
#read metadata
metadata <- read.delim(file="testing_files/dummy_metadata.txt", sep="\t", header=T)
#read tree
tree <- read.tree(file.path("testing_files/", "newick_format_tree.tre"))
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

# gets ancestor using child node and edge list
get.ancestor <- function(node, edges) {
  return(edges[which(edges[,2] == node)][1])
}

collapse.nodes <- function(nodes) {
  # create structure for storing ancestors
  ancestors <- matrix(, nrow = 0, ncol = 3)
  # iterate through nodes
  for (i in 1:length(nodes)) {
    node = unlist(nodes[i])
    anc = get.ancestor(node, tree_test$edge)
    # if ancestor already in matrix
    if (anc %in% ancestors[,1]) {
      # get ancestor's index
      ind <- which(ancestors[,1] == anc)
      # flag to collapse
      ancestors[ind, 2] <- 1
      # remove its children from nodes
      nodes[nodes == node] <- NA
      nodes[nodes == ancestors[ind, 3]] <- NA
    }
    else {
      # add new ancestor (don't flag to collapse, remember its child)
      ancestors <- rbind(ancestors, c(anc, 0, node))
    }
  }
  nodes <- nodes[!is.na(nodes)]
  # get ancestors flagged to collapse
  ancestors <- ancestors[which(ancestors[,2] == 1)]
  # if we have none, return nodes
  if (length(ancestors) == 0)
    return(nodes)
  # recursive call on new node list
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