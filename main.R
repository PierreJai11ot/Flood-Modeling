# Main - orchestrates the data loading, cleaning, and modelling

# Load all relevant libraries used across the project

libraries <- c(
  "ParetoPosStable", "rvest", "dplyr", "stringr", "xml2", "XML", "readr", 
  "ggplot2", "patchwork", "tidyr", "zoo", "e1071", "extRemes", "readxl", 
  "vcd", "sp", "rworldmap", "countries", "magrittr", "purrr", "gridExtra", 
  "TeachingDemos", "statmod", "ggrepel", "tidyverse", "sf", "classInt", 
  "giscoR", "eurostat", "fitdistrplus", "units", "actuar", "mc2d", "distrMod", 
  "stats4", "cowplot", "gridGraphics", "VGAM", "latex2exp", "shadowtext", 
  "strucchange", "lmtest", "goftest", "rlang", "kableExtra", "maxLik", "kableExtra",
  "cluster", "ggdendro", "ape", 'dendextend'
)

lapply(libraries, library, character.only = TRUE)

rm(libraries)


# Store the path to the working directory

scriptDirectory <- dirname(rstudioapi::getActiveDocumentContext()$path)

plotDirectory <- paste0(scriptDirectory, "/Plots")

resultsDirectory <- paste0(scriptDirectory, "/Results")


# Data loading – reading all relevant external data sets

source(paste0(scriptDirectory, "/Scripts/dataLoading.R"))

snapshotName <- paste0(resultsDirectory, "/snapPost_dataLoading.RData")
# save.image(file = snapshotName)
# load(snapshotName)
# rm(snapshotName)


# Data preparation - Harmonizing labels and adding useful tags, filtering out irrelevant entries

source(paste0(scriptDirectory, "/Scripts/dataPreparation.R"))

snapshotName <- paste0(resultsDirectory, "/snapPost_dataPreparation.RData")
# save.image(file = snapshotName)
# load(snapshotName)
# rm(snapshotName)


# Setting the time frame

startingYear <- 1950
endingYear <- 2020


# Data exploration

source(paste0(scriptDirectory, "/Scripts/dataExploration.R"))

snapshotName <- paste0(resultsDirectory, "/snapPost_dataExploration.RData")
# save.image(file = snapshotName)
# load(snapshotName)
# rm(snapshotName)


# Specifying the retained normalized loss metric - "normalizedLossTotalVulnerability" or "normalizedLossAreaWeighedAverageVulnerability"

retainedLossMetric <- "normalizedLossTotalVulnerability"


# Flood modelling 

source(paste0(scriptDirectory, "/Scripts/floodModelling.R"))

snapshotName <- paste0(resultsDirectory, "/snapPost_floodModelling.RData")
# save.image(file = snapshotName)
# load(snapshotName)
# rm(snapshotName)


# Model evaluation
source(paste0(scriptDirectory, "/Scripts/modelEvaluation.R"))

snapshotName <- paste0(resultsDirectory, "/snapPost_modelEvaluation.RData")
# save.image(file = snapshotName)
# load(snapshotName)
# rm(snapshotName)


# Model selection

source(paste0(scriptDirectory, "/Scripts/modelSelection.R"))

snapshotName <- paste0(resultsDirectory, "/snapPost_modelSelection.RData")
# save.image(file = snapshotName)
load(snapshotName)
# rm(snapshotName)


# Monte Carlo simulation of the selected models

iterations <- 50000

source(paste0(scriptDirectory, "/Scripts/monteCarloSimulation.R"))

snapshotName <- paste0(resultsDirectory, "/snapPost_monteCarloSimulation50knew.RData")
# save.image(file = snapshotName)
load(snapshotName)
# rm(snapshotName)

# iterations <- 50000

# Tail risk assessment
source(paste0(scriptDirectory, "/Scripts/tailRiskAssessment.R"))

snapshotName <- paste0(resultsDirectory, "/snapPost_tailRiskAssessment.RData")
# save.image(file = snapshotName)
load(snapshotName)
# rm(snapshotName)

# iterations <- 50000

# Clustering
# BALISE
source(paste0(scriptDirectory, "/Scripts/clustering.R"))

snapshotName <- paste0(resultsDirectory, "/snapPost_clustering.RData")
# save.image(file = snapshotName)
load(snapshotName)
# rm(snapshotName)

