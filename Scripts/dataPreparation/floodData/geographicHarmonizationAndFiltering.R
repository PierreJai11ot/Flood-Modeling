# Filter to events located in relevant countries - Our study is restricted to member states of the European Union as well as Norway, the United Kingdom and Switzerland
# Several challenges arise when considering the country labels of some event

# Some events fall entirely outside of the geographic scope of our study

# EM-DAT has worldwide entries
# Checking if all entries in EM-DAT have a continent specified in Region

message(paste("Out of the ",  nrow(dataEMDAT), "flood-related entries of EM-DAT", nrow(dataEMDAT) - sum(is.na(dataEMDAT$Region)), "have a Region label among: ", paste(sort(unique(dataEMDAT$Region)), collapse = ", ")))


# Filtering to events located in Europe

entriesCount <- nrow(dataEMDAT)

dataEMDAT <- filter(dataEMDAT, Region == "Europe")

message(paste("Out of the ", entriesCount, "flood-related entries of EM-DAT", nrow(dataEMDAT), "are located in Europe"))


# Some relevant events are listed under under European countries that no longer exist
# For instance: "Soviet Union", "Germany Federal Republic", "Czechoslovakia", "German Democratic Republic", "Yugoslavia"

# Two options to adequately re-locate the problematic entries using the other variables : using the coordinates in 'Latitude' and 'Longitude' or using the specifications in 'Location'

# Defining a function to assess the relabelling possibilites for each obsolete country label

successorStatesAssessmentEMDAT <- function(obsoleteCountryName, successorStates) {
  message(paste("In EM-DAT ",
                sum(dataEMDAT$Country == obsoleteCountryName), 
                " relevant entries are labeled ", obsoleteCountryName, ", of which ", 
                sum( (!is.na(dataEMDAT$Latitude)) & (!is.na(dataEMDAT$Longitude)) & dataEMDAT$Country == obsoleteCountryName),
                " with coordinates, ", 
                sum( (!is.na(dataEMDAT$Location)) & dataEMDAT$Country == obsoleteCountryName), 
                " with location of which ", 
                sum(rowSums(sapply(successorStates, function(p) grepl(p, c(filter(dataEMDAT, Country == obsoleteCountryName)$Location, " "), fixed = TRUE))) >= 1),
                " containing one of the successor states and ", 
                sum(rowSums(sapply(successorStates, function(p) grepl(p, c(filter(dataEMDAT, Country == obsoleteCountryName)$Location, " "), fixed = TRUE))) == 1), 
                " containing exactly one of them.  \n Location tags include: \n", 
                paste(sort(unique(filter(dataEMDAT, Country == obsoleteCountryName)$Location)), collapse = ", \n")))
}


# "Czechoslovakia" labelled entries need to be distributed between the two successor states, the Czech Republic and Slovakia

successorStatesAssessmentEMDAT("Czechoslovakia", c("Czech Republic", "Slovakia"))


# "Germany Federal Republic", "German Democratic Republic" can both be assigned to Germany, the only successor state, without further investigation

# "Soviet Union" labelled entries need to be distributed between the three successor states relevant to our study : Lithuania, Latvia and Estonia

successorStatesAssessmentEMDAT("Soviet Union", c("Estonia", "Latvia", "Lithuania"))


# "Yugoslavia" labelled entries need to be distributed between the two successor states relevant to our study : Slovenia and Croatia 

successorStatesAssessmentEMDAT("Yugoslavia", c("Croatia", "Slovenia"))


# One can use the Location column to alter the country label of each salvageable entry with an obsolete country name appropriately
# Note that since entries with more than one country listed in Location cannot be reliably split, they will be dropped


# Some country labels differ between the candidate data sets, including countries relevant to our study
# For instance: "Netherlands (Kingdom of the)", "United Kingdom of Great Britain and Northern Ireland"
# The following alternative tags will be preferred: "Netherlands", "United Kingdom"
# Introducing and applying a function to appropriately relabel relevant entries of EM-DAT

relocationFunctionEMDAT <- function(country, location) {
  
  obsoleteCountries <- c("Czechoslovakia", "Soviet Union", "Yugoslavia")
  successorStates <- c("Czech Republic", "Slovakia", "Slovenia", "Croatia", "Estonia", "Latvia", "Lithuania")
  
  if (country %in% obsoleteCountries){
    identifiedSubstrings <- sapply(successorStates, function(sub) grepl(sub, location))
    if (sum(identifiedSubstrings, na.rm = TRUE) == 1) {
      return(successorStates[which(identifiedSubstrings)[1]])
    }
  }
  
  deprecatedCountryNames <- c("Czechia", "United Kingdom of Great Britain and Northern Ireland", "German Democratic Republic", "Germany Federal Republic", "Netherlands (Kingdom of the)")
  preferredCountryNames <- c("Czech Republic", "United Kingdom", "Germany", "Germany", "Netherlands")
  
  if (country %in% deprecatedCountryNames) {
    return(preferredCountryNames[which(deprecatedCountryNames == country)])
  }
  
  return(country)
} 

dataEMDAT <- dataEMDAT %>% 
  rowwise() %>% 
  mutate(Country = relocationFunctionEMDAT(Country, Location)) %>% 
  ungroup()

rm(successorStatesAssessmentEMDAT, relocationFunctionEMDAT)


# Similarly, in HANZE some country labels differ between the candidate data sets, including countries relevant to our study
# For instance: "Czechia"
# The following alternative tags will be preferred: "Czech Republic"

dataHANZE <- dataHANZE %>% 
  mutate(Country = if_else(Country == "Czechia", "Czech Republic", Country))


# Some autonomous regions and overseas territories are listed separately in HANZE
# For instance: "Isle of Man" (UK), "Azores Islands" (Portugal), "Guadeloupe" (France)
# These may not be appropriate to model floods in Europe and will be considered to be outside of the scope of our study


# Both candidate data sets contain entries located in countries falling outside of the scope of our study
# Defining the relevant countries, possibly excluding the Netherlands as extensive flood protection could constitute an important bias 

memberStatesEU <- c("Austria", "Belgium", "Bulgaria", "Croatia", "Cyprus", "Czech Republic", "Denmark", "Estonia", "Finland", "France", "Germany", "Greece", "Hungary", "Ireland", "Italy", "Latvia", "Lithuania", "Luxembourg", "Malta", "Netherlands", "Poland", "Portugal", "Romania", "Slovakia", "Slovenia", "Spain", "Sweden")
relevantCountries <- c(memberStatesEU, "Norway", "Switzerland", "United Kingdom")
relevantCountries <- setdiff(relevantCountries, "Netherlands")
relevantCountries <- sort(relevantCountries)


# Adding a column corresponding to the ISO 3166-1 alpha-2 country codes

relevantCountriesISO <- dataHANZE$'Country code'[match(relevantCountries, dataHANZE$Country)]
relevantCountries <- data.frame(Country = relevantCountries, CountryISO = relevantCountriesISO)

rm(memberStatesEU, relevantCountriesISO)

dataHANZE <- dataHANZE  %>% 
  rename(CountryISO = 'Country code')

dataEMDAT <- dataEMDAT %>% 
  left_join(relevantCountries, by = "Country")

# Filter to entries located in relevant countries

entriesCount <- nrow(dataEMDAT)

dataEMDAT <- dataEMDAT %>% 
  filter(Country %in% relevantCountries$Country)

message(paste("Out of ", entriesCount, "flood-related entries located in Europes of EM-DAT", nrow(dataEMDAT), "are in countries within the scope of our study"))

entriesCount <- nrow(dataHANZE)

dataHANZE <- dataHANZE %>% 
  filter(Country %in% relevantCountries$Country)

message(paste("Out of ", entriesCount, "flood-related entries located in Europes of HANZE", nrow(dataHANZE), "are in countries within the scope of our study"))

rm(entriesCount)


# Introducing functions to generate useful tallies of the number of entries

source(paste0(scriptDirectory, "/Scripts/dataPreparation/tallyFunctions.R"))


# Generating a tally to compare the data sets

tally <- countryTally(dataEMDAT, dataHANZE)
print(tally, n = Inf)

tallyLaTeX <- countryTallyLaTeX(dataEMDAT, dataHANZE)
writeLines(tallyLaTeX, file.path(paste0(scriptDirectory, "/Texts"), "Tally1.txt"))

rm(tally, tallyLaTeX)
