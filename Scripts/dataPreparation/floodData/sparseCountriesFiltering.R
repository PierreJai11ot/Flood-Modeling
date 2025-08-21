# Filter out countries with insufficient observations - Removing countries with less than an arbitrary number of observations

# Setting the chosen arbitrary minimal number of observations

minNumberObservations <- 5


# Generating a tally to compare the data sets, highlighting countries without enough observations

tallyLaTeX <- countryTallyLaTeX(dataEMDAT, dataHANZE, (minNumberObservations - 1))
writeLines(tallyLaTeX, file.path(paste0(scriptDirectory, "/Texts"), "Tally4.txt"))

rm(tallyLaTeX)


# Identifying countries with an insufficient number of observations

countriesInsufficientDataEMDAT <- setdiff(relevantCountries$Country, unique((dataEMDAT %>% count(`Country`, sort = TRUE) %>% filter(n >= minNumberObservations))$Country)) 

countriesInsufficientDataHANZE <- setdiff(relevantCountries$Country, unique((dataHANZE %>% count(`Country`, sort = TRUE) %>% filter(n >= minNumberObservations))$Country))

  
# Filtering out countries with an insufficient number of observations

entriesCount <- nrow(dataEMDAT)

dataEMDAT <- filter(dataEMDAT, ! Country %in% countriesInsufficientDataEMDAT)

message(paste("Out of the ", entriesCount, " flood-related entries of EM-DAT located in relevant countries and happeing during the set time interval ", nrow(dataEMDAT), " are located in countries with ", minNumberObservations, " observations or more"))

entriesCount <- nrow(dataHANZE)

dataHANZE <- filter(dataHANZE, ! Country %in% countriesInsufficientDataHANZE)

message(paste("Out of the ", entriesCount, " flood-related entries of HANZE located in relevant countries and happeing during the set time interval ", nrow(dataHANZE), " are located in countries with ", minNumberObservations, " observations or more"))

rm(entriesCount, minNumberObservations, countriesInsufficientDataEMDAT, countriesInsufficientDataHANZE)
  