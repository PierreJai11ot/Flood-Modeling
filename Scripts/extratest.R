

countryList <- list(c("Austria", "Belgium"))

test <- clusterNormalizationDataFetcher(list(countryList), severityNormalizationDataHANZE, year = 1950, isISO = FALSE)


test

filter(filter(severityNormalizationDataHANZE, Country == "Austria"), Year ==1950) 

filter(filter(severityNormalizationDataHANZE, Country == "Belgium"), Year ==1950)


