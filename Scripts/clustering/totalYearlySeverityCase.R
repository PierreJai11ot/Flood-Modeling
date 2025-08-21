# Total yearly loss case

# Selecting countries and proceeding with the clustering method for group 1 

source(paste0(scriptDirectory, "/Scripts/clustering/totalYearlySeverityCase/group1.R"))

snapshotName <- paste0(resultsDirectory, "/snapPost_group1_withreworkNEW.RData")
# save.image(file = snapshotName)
load(snapshotName)
# rm(snapshotName)


# Proceeding with the clustering method for the remaining countries of group 2

source(paste0(scriptDirectory, "/Scripts/clustering/totalYearlySeverityCase/group2.R"))

snapshotName <- paste0(resultsDirectory, "/snapPost_group2_withReworkNEW.RData")
# save.image(file = snapshotName)
# load(snapshotName)
# rm(snapshotName)