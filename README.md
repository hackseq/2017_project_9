# Using CollapseTree with Taxonomic Metadata

Large phylogenetic trees with hundreds or even thousands of nodes are impractical to view, and those who need them in their research have limited options for manipulating trees of this size. The collapseTree function in R's phytools package allows users to manually select branches of a phylogenetic tree to collapse, which achieves what the viewer of a large tree might want, but manually selecting the branches becomes difficult for the largest trees, or if the user would like to quickly make a series of different views of the same large input tree. By using taxonomic rank metadata and a stylesheet indicating which taxa should be collapsed, the existing collapseTree function could be used to be used to automate how branches in the tree are collapsed.

For collapseTree, the team will work towards adding the following functionalities:

1. Collapse a specific set of tree tips, info supplied as a separate metadata object in the form of a stylesheet.
2. Collapse a specific set of taxa to a specific taxonomic level, info supplied as taxonomy strings for each tree tip in metadata, plus a stylesheet that designates the desired level of detail for specific taxonomic groups.

Team Lead: Evan | evan.morien@gmail.com | @morien | | UBC Botany
