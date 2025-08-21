
# Tools for the different clustering strategies

source(paste0(scriptDirectory, "/Scripts/clustering/clusteringTools.R"))
# snapshotName <- paste0(resultsDirectory, "/snapPost_clutool.RData")
# save.image(file = snapshotName)
# load(snapshotName)

# Total yearly loss case

source(paste0(scriptDirectory, "/Scripts/clustering/totalYearlySeverityCase.R"))
snapshotName <- paste0(resultsDirectory, "/snapPost_totalSevCasClusWithNewNEW.RData")
# save.image(file = snapshotName)
load(snapshotName)

# Compound model case

source(paste0(scriptDirectory, "/Scripts/clustering/compoundModelCase.R"))

snapshotName <- paste0(resultsDirectory, "/snapPost_compoundModClusnewlook.RData")
# save.image(file = snapshotName)
load(snapshotName)
# rm(snapshotName)


