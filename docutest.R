#' Collapse a (taxonomic) tree at a given rank, with options to exclude specific clades.
#' 
#' @param tree A tree object 
#' @param metadata A data.frame with 9 (+1 optional) columns. Data rows are tree leaves with (clade names at each rank).
#' @param rankN A character string corresponding to the rank at which to collapse the tree. Must match header name on \code{metadata}
#' @param excludeType Specify method for inputting clades to exclude from collapsing. Either \code{'list'}, \code{'col'} or none. 
#' @param excludeItem A vector of clade names if \code{excludeType=='list'}. The header name of a binary vector on \code{metadata} if \code{excludeType=='col'}. 
#' 
#' 
#' @examples
#' metadataCollapseTree(tree.object,metadata.data.frame, 'rank5', 'list', c("Anthozoa", "Embyophyta"))
#' metadataCollapseTree(tree.object,metadata.data.frame, 'rank5', 'col', 'excludeBin')
#' 
#' 
#' @details 
#' The data.frame \code{metadata} should be in the following format:
#'   \tabular{ccccccccc}{
#'  taxon_ID \tab rank1	\tab rank2 \tab	rank3	\tab rank4 \tab	rank5 \tab rank6 \tab	rank7 \tab rank8 \cr
#'  6014 \tab Eukaryota \tab	Opisthokonta \tab	Metazoa \tab Cnidaria \tab Anthozoa \tab Actiniaria \tab Anthopleura \tab	Anthopleura_elegantissima_(clonal_anemone) \cr
#'  5801 \tab Eukaryota \tab	Archaeplastida \tab	Chloroplastida \tab	Phragmoplastophyta \tab	Embryophyta \tab Solanum \tab Solanum	\tab Solanum_chacoense_(Chaco_potato)
#' }
#' Each row corresponds to a tip in \code{tree}. An optional column should be included if \code{exclusionType 'col'} is chosen. 
#' This additional column should contain a value of \code{1} if the tip should NOT be collapsed, \code{0} otherwise.
#' 
#' @author Evan Morien , Lauren Chong, Allison Tai, Cedric Wang, Mark Forteza
#' 
docutest <- function (tree, metadata, rankN, excludeType, excludeItem) {
  return(1)
}

