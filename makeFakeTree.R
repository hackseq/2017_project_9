# faketree.R
library(ape)
library(geiger)


faketree <- rtree(n=100)
plot(faketree)
write.tree(faketree, "testing_files/fakeTree.tre")


#####
#hard coded clade names based on nodelabels. (sorry)
nodelabels()
cladeNames <- c(179, 175, 167, 166, 163, 162, 151, 104)

clades <- c()
tips <- c()


for (clade in cladeNames) {
  
  currentClade <- extract.clade(faketree,clade)
  
  currentMembers <- currentClade$tip.label
  
  for (tip in currentMembers) {
    clades <- append(clades, clade)
    tips <- append(tips, tip)
  }
}

fakeData <- data.frame(clades, tips)

### print fake meta data
write.table(fakeData, "testing_files/fakeData.txt", sep="\t")




