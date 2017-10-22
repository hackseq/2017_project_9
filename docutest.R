library(roxygen2) 
 


#' Collapse a (taxonomic) tree at a given rank, with options to exclude specific clades
#' 
#' @param tree A tree object 
#' @param metadata A data.frame with 7 (+1 optional) columns. Data rows are tree leaves with (clade names at each rank).
#' @param rankN A character string corresponding to the rank at which to collapse the tree. Must match header name on \code{metadata}
#' @param excludeType Specify method for inputting clades to exclude from collapsing. Either \code{'list'}, \code{'col'} or none. 
#' @param excludeItem A vector of clade names if \code{excludeType=='list'}. The header name of a binary vector on \code{metadata} if \code{excludeType=='col'}. 
#' 
#' \strong{Example function call}
#' docutest(tree.object,metadata.data.frame, 'rank5', 'list', c("cladeA", "cladeB"))
#' docutest(tree.object,metadata.data.frame, 'rank5', 'col', 'excludeBin')
#' 
#' 
#' \strong{Example metadata}
#' \tabular{cccccccc}{
#' taxon_ID \tab rank1	\tab rank2 \tab	rank3	\tab rank4 \tab	rank5 \tab rank6 \tab	rank7 \tab rank8 \cr
#' 6014 \tab Eukaryota \tab	Opisthokonta \tab	Metazoa \tab Cnidaria \tab Anthozoa \tab Actiniaria \tab Anthopleura \tab	Anthopleura_elegantissima_(clonal_anemone) \cr
#' 5801 \tab Eukaryota \tab	Archaeplastida \tab	Chloroplastida \tab	Phragmoplastophyta \tab	Embryophyta \tab Solanum \tab Solanum	\tab Solanum_chacoense_(Chaco_potato)
#' }
#' 
metadataCollapseTree <- function (tree, metadata, rankN, excludeType, excludeItem) {
  return(1)
}


#roxygen2::roxygenise()
