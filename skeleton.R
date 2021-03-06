#testing functionality of collapseTree
library(ape)
library(phytools)
library(phylobase)

#prep code
#read metadata
metadata <- read.delim(file="testing_files/dummy_metadata.txt", sep="\t", header=T)
#read tree
tree <- read.tree(file.path("testing_files/", "newick_format_tree.no_query.tre"))
#parameters
exclude <- c("Bivalvia", "Embryophyta") #two clades from rank5
collapse <- c("rank5")

#make sure tree and metadata correspond in their leaves/rows
tree <- drop.tip(tree, tree$tip.label[!tree$tip.label %in% metadata[,1]])
a <- as.character(tree$tip.label)
metadata <- metadata[match(a, metadata$taxon_ID),]

#inputs
metadata <- metadata
tree <- tree
cladestoexclude <- exclude
rankN <- collapse

#dummy example of column exclude input
metadata$excludebin <- as.numeric(metadata$rank5)
a <- which(as.character(metadata[,6]) == "Bivalvia")
b <- which(as.character(metadata[,6]) == "Embryophyta")
c <- c(a,b)
metadata$excludebin[c] <- 1
metadata$excludebin[-c] <- 0
pruned_tree <- metadataCollapseTree(tree, metadata, "rank5", excludeType = "column", excludeItem = "excludebin")

metadataCollapseTree <- function (tree, metadata, rankN, excludeType, excludeItem) {
  #throw out metadata that has no assignment at rankN
  metadata[[rankN]][which(metadata[[rankN]] == "")] <- NA #convert missing to NA
  metadata[[rankN]][which(metadata[[rankN]] == "NA")] <- NA #convert character NA to NA
  metadata <- metadata[which(!is.na(metadata[[rankN]])),] #remove rows with NA values
  #drop tips that don't have that assignment too
  tree <- drop.tip(tree, tree$tip.label[!tree$tip.label %in% metadata[,1]])
  #create exclusion list
  cladestoexclude <- createExclusionVec(metadata, rankN, excludeType, excludeItem)
  #select tree tips
  pruned_tree <- tree
  i <- 0
  for (clade in unique(metadata[[rankN]])) {
    i <-  i + 1
    pruned_tree4 <- as(pruned_tree, "phylo4")
    if (clade %in% cladestoexclude) {
      print(paste0("Excluding clade: ", clade))
      next
    }
    tipstocollapse <- as.character(metadata[which(metadata[[rankN]] == clade),1])
    nodes <- returnNodes(tipstocollapse, pruned_tree4) #generate full node list
    nodes <- collapse.nodes(nodes, pruned_tree$edge) #prune nodes to avoid redundant collapse iterations
    pruned_tree <- pruneTree(pruned_tree, nodes, clade) #prune tree for each node
    print(paste0("finished with clade: ", clade, "(iteration ", i, "of ", length(unique(metadata[[rankN]]))))
  }
  return(pruned_tree)
}



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
    if (!is.na(node)) {
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
  # Prune the tree by splitting at each node
  reduced.tree <- tree
  for (node in nodes) {
    # Each time we prune, node indices get updated. This function matches node
    # IDs from two trees, so we can figure out what the node ID is in our
    # pruned tree
    node.map <- if(node > length(tree$tip.label)) { matchNodes(tree, reduced.tree) } else { matchLabels(tree, reduced.tree) }
    node.reduced <- as.numeric(node.map[which(node.map[,1] == node), 2])
    
    # Split the tree, cutting at the node we are collapsing
    trees.split <- splitTree(
      reduced.tree,
      list(node = node.reduced, bp = reduced.tree$edge.length[which(reduced.tree$edge[,2] == node.reduced)]))
    reduced.tree <- trees.split[[1]]
    # Replace the new "NA" label with the name of the clade
    reduced.tree$tip.label[which(reduced.tree$tip.label == "NA")] <- cladeName
  }
  return(reduced.tree)
}

#Function to create vector of clades to exclude at a given rank, 
#using various optional user input
createExclusionVec <- function(metadata, collapseAtRank, excludeType=NULL, excludeItem=NULL) {
  if (excludeType == 'list') { # list of tips
    if (is.vector(excludeItem)) {
      print("List of clades given.")
      cladestoexclude <- excludeItem
      return(cladestoexclude)
    }
    else { #break
      print("Invalid input. Exclusion type 'list' must be accompanied by a vectorof clade names")
    }
  }
  else if (excludeType == 'col' | excludeType == 'column') { # additional binary column
    if (is.character(excludeItem)) {
      # access column with the user specified name
      excludeBinaryCol <- excludeItem
      # generate unique list of clades to exclude
      cladestoexclude <- unique(metadata[[rankN]][which(metadata[[excludeBinaryCol]]==1)])
      return(cladestoexclude)
    }
  }
  else if (is.null(excludeType)) {  #nothing excluded
    print("No clades provided to exclude.")
  }
  else { #input error #break
    print("Invalid exclusion method.")
  }
}
