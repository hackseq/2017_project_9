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
tree <- drop.tip(tree, tree$tip.label[!tree$tip.label %in% metadata[,1]])


#inputs
metadata <- metadata
tree <- tree
cladestoexclude <- exclude
rankN <- collapse


# create exclusion list
cladestoexclude <- createExclusionVec(metadata,rankN,excludeType=c("list","col"), excludeItem=NULL)



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
  nodes <- append(nodes, returnNodes(tipstocollapse, tree4)) #generate full node list
  nodes <- collapse.nodes(nodes, tree$edge) #prune nodes to avoid redundant collapse iterations
  pruned_tree <- pruneTree(tree, nodes, clade) #prune tree for each node
}

# return all nodes to collapse
collapse <- collapse.nodes(nodes)

# gets ancestor using child node and edge list
get.ancestor <- function(node, edges) {
  return(edges[which(edges[,2] == node)][1])
}

collapse.nodes <- function(nodes, edges) {
  # create structure for storing ancestors
  ancestors <- matrix(, nrow = 0, ncol = 3)
  # iterate through nodes
  for (i in 1:length(nodes)) {
    node = unlist(nodes[i])
    anc = get.ancestor(node, edges)
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
    return(collapse.nodes(c(ancestors,nodes), edges))
}

#select nodes and branch positions based on tree tips
#from tips to nodes
returnNodes <- function(x,y) {
  z <- c() #declare z for storing selected nodes
  for (tip in x) {
    node <- as.numeric(getNode(y, tip, type = c("tip"), missing = c("OK"))[[1]]) #find node ID for tree tip
    if (node != "NA") {
      z <- append(z, node) #store it if it isn't an NA #if they are NAs, they aren't in the tree. possible if metadata and tree don't match
    }
    else {
      print(paste0("tip not in tree: ", tip)) #tell user if their tip isn't in the tree
    }
  }
  return(z)
}

# Function to take a tree, collapse a node, and return the new tree
pruneTree <- function(tree, nodes, cladeName) {
  reduced.tree <- tree #assign so we can work with the new object. this gets pruned and re-assigned each iteration of loop below
  for (node in nodes) { #split tree for each node
    trees.split <- splitTree(reduced.tree, list(node = node, bp = tree$edge.length[which(tree$edge[,2] == node)]))
    reduced.tree <- trees.split[[1]]
    reduced.tree$tip.label[node] <- cladeName # Check if this works!
  }
  return(reduced.tree)
}



#Function to create vector of clades to exclude at a given rank, 
#using various optional user input
createExclusionVec <- function(metadata,collapseAtRank, excludeType=NULL, excludeItem=NULL) {
  
  if (excludeType == 'list') { # list of tips
    if (is.vector(excludeItem)) {
      print("List of clades given.")
      cladestoexclude <- excludeItem
      
      return(cladestoexclude)
    } else {
      #break
      print("Invalid input. Exclusion type 'list' must be accompanied by a vectorof clade names")
    }
    
  } else if (excludeType == 'col' | excludeType == 'column') { # additional binary column
    if (is.character(excludeItem)) {
      # access column with the user specified name
      excludeBinaryCol <- excludeItem
      
      # include only rows user wants to exclude
      for (index in which(metadata[[excludeBinaryCol]]==1)) {
        rankN <- collapseAtRank
        
        currentClade <- as.character(metadata[[rankN]][index])
        cladestoexclude <- append(cladestoexclude,currentClade)
      }
      
      return(cladestoexclude)
    }
    
  } else if (is.null(excludeType)) {  # nothing excluded
    print("No clades provided to exclude.")
    
  } else {  # input error
    #break
    print("Invalid exclusion method.")
  }
  
} 