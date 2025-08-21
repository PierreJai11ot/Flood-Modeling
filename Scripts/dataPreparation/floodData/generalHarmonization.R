# Harmonize key variable names for consistency across data sets

dataEMDAT <- rename(rawDataEMDAT, Year = "Start Year")

dataHANZE <- rename(rawDataHANZE, Country = "Country name")

rm(rawDataEMDAT, rawDataHANZE)


# Consistency check - Verifying that the Year variable correspond to the year of the starting date in the HANZE data set

sum(substr(dataHANZE$`Start date`, 1,4) != dataHANZE$`Year`)
