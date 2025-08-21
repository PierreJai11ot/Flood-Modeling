# Preparation of the external data sets

# Harmonizing and filtering the raw EM-DAT and HANZE flood data sets

# Harmonize key variable names for consistency across data sets

source(paste0(scriptDirectory, "/Scripts/dataPreparation/floodData/generalHarmonization.R"))


# Filter to flood-related events

source(paste0(scriptDirectory, "/Scripts/dataPreparation/floodData/eventTypeFiltering.R"))


# Relabel and filter events to retain those located in relevant countries

source(paste0(scriptDirectory, "/Scripts/dataPreparation/floodData/geographicHarmonizationAndFiltering.R"))


# Identifying a suitable time frame for the study and filter events accordingly

source(paste0(scriptDirectory, "/Scripts/dataPreparation/floodData/temporalScopeFiltering.R"))


# Identifying and harmonizing the most suitable loss metric 

source(paste0(scriptDirectory, "/Scripts/dataPreparation/floodData/lossMetricHarmonization.R"))


# Filter out countries with insufficient observations

source(paste0(scriptDirectory, "/Scripts/dataPreparation/floodData/sparseCountriesFiltering.R"))


# As HANZE contains a greater array of relevant entries, it will henceforth be preferred to EM-DAT

rm(dataEMDAT, countryTally, countryTallyLaTeX)


# Visualization - Generating maps of the remaining occurrences and a final tally table

source(paste0(scriptDirectory, "/Scripts/dataPreparation/floodData/occurrencesMapGeneration.R"))


# Loss Normalization - Normalizing the most suitable loss metric

source(paste0(scriptDirectory, "/Scripts/dataPreparation/floodData/lossMetricNormalization.R"))


