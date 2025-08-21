# Data exploration

# Generating preliminary maps

# Introducing a standardized map generator function for metrics on the national scale 

source(paste0(scriptDirectory, "/Scripts/standardMapGenerator.R"))


# Altering data sets to visualize the mean and variance of the number of yearly flood events

yearlyOccurencesMapData <- dataHANZE %>% 
  group_by(CountryISO) %>% 
  count(Year, sort = TRUE) %>%
  complete(Year = full_seq(startingYear:endingYear, 1), fill = list(n=0)) %>% 
  rename(NumberOfFloods = n) %>% 
  group_by(CountryISO) %>% 
  summarise(averageYearlyOccurences = mean(NumberOfFloods),
            varianceYearlyOccurences = var(NumberOfFloods))


# Generating the corresponding maps

meanYearlyOccurencesMap <- nationalMapGenerator(yearlyOccurencesMapData, "averageYearlyOccurences", "Mean yearly flood events", "Mean number of yearly floods")

varianceYearlyOccurencesMap <- nationalMapGenerator(yearlyOccurencesMapData, "varianceYearlyOccurences", "Variance of yearly flood events", "Variance of the number of yearly floods")


# Storing the generated maps 

ggsave(file.path(mapDirectory, "Mean yearly flood events.pdf"), width = 21, height = 21, units = "cm", plot = meanYearlyOccurencesMap)

ggsave(file.path(mapDirectory, "Variance of yearly flood events.pdf"), width = 21, height = 21, units = "cm", plot = varianceYearlyOccurencesMap)

rm(yearlyOccurencesMapData, meanYearlyOccurencesMap, varianceYearlyOccurencesMap)


# Altering data sets to visualize the mean and variance of normalized losses 

normalizedLossMapData <- dataHANZE %>%
  group_by(CountryISO) %>%
  summarise(averageNormalizedLossTotalVulnerability = mean(normalizedLossTotalVulnerability), 
            varianceNormalizedLossTotalVulnerability = var(normalizedLossTotalVulnerability),
            averageNormalizedLossAreaWeighedAverageVulnerability = mean(normalizedLossAreaWeighedAverageVulnerability), 
            varianceNormalizedLossAreaWeighedAverageVulnerability = var(normalizedLossAreaWeighedAverageVulnerability))


# Generating the corresponding maps

meanNormalizedLossMap1 <- nationalMapGenerator(normalizedLossMapData, "averageNormalizedLossTotalVulnerability", "Average normalized loss per flood (for method 1)", "Average normalized loss")

varianceNormalizedLossMap1 <- nationalMapGenerator(normalizedLossMapData, "varianceNormalizedLossTotalVulnerability", "Variance of normalized loss per flood (for method 1)", "Variance of normalized loss")

meanNormalizedLossMap2 <- nationalMapGenerator(normalizedLossMapData, "averageNormalizedLossAreaWeighedAverageVulnerability", "Average normalized loss per flood (for method 2)", "Average normalized loss")

varianceNormalizedLossMap2 <- nationalMapGenerator(normalizedLossMapData, "varianceNormalizedLossAreaWeighedAverageVulnerability", "Variance of normalized loss per flood (for method 2)", "Variance of normalized loss")


# Storing the generated maps 

ggsave(file.path(mapDirectory, "Average normalized loss per flood (for method 1).pdf"), width = 21, height = 21, units = "cm", plot = meanNormalizedLossMap1)

ggsave(file.path(mapDirectory, "Variance of normalized loss per flood (for method 1).pdf"), width = 21, height = 21, units = "cm", plot = varianceNormalizedLossMap1)

ggsave(file.path(mapDirectory, "Average normalized loss per flood (for method 2).pdf"), width = 21, height = 21, units = "cm", plot = meanNormalizedLossMap2)

ggsave(file.path(mapDirectory, "Variance of normalized loss per flood (for method 2).pdf"), width = 21, height = 21, units = "cm", plot = varianceNormalizedLossMap2)

rm(normalizedLossMapData, meanNormalizedLossMap1, varianceNormalizedLossMap1, meanNormalizedLossMap2, varianceNormalizedLossMap2)



# Altering data sets to visualize the mean and variance of normalized total losses per year with flods

normalizedLossMapData <- dataHANZE %>%
  group_by(CountryISO, Year) %>%
  summarise(totalNormalizedLossTotalVulnerability = sum(normalizedLossTotalVulnerability), 
            totalNormalizedLossAreaWeighedAverageVulnerability = sum(normalizedLossAreaWeighedAverageVulnerability)) %>%
  ungroup() %>%
  group_by(CountryISO) %>%
  summarise(averageNormalizedLossTotalVulnerability = mean(totalNormalizedLossTotalVulnerability), 
            varianceNormalizedLossTotalVulnerability = var(totalNormalizedLossTotalVulnerability),
            averageNormalizedLossAreaWeighedAverageVulnerability = mean(totalNormalizedLossAreaWeighedAverageVulnerability), 
            varianceNormalizedLossAreaWeighedAverageVulnerability = var(totalNormalizedLossAreaWeighedAverageVulnerability))


# Generating the corresponding maps

meanNormalizedLossMap1 <- nationalMapGenerator(normalizedLossMapData, "averageNormalizedLossTotalVulnerability", "Average normalized total loss per year with floods (for method 1)", "Average normalized total loss")

varianceNormalizedLossMap1 <- nationalMapGenerator(normalizedLossMapData, "varianceNormalizedLossTotalVulnerability", "Variance of normalized total loss per year with floods (for method 1)", "Variance of normalized total loss")

meanNormalizedLossMap2 <- nationalMapGenerator(normalizedLossMapData, "averageNormalizedLossAreaWeighedAverageVulnerability", "Average normalized total loss per year with floods (for method 2)", "Average normalized total loss")

varianceNormalizedLossMap2 <- nationalMapGenerator(normalizedLossMapData, "varianceNormalizedLossAreaWeighedAverageVulnerability", "Variance of normalized total loss per year with floods (for method 2)", "Variance of normalized total loss")


# Storing the generated maps 

ggsave(file.path(mapDirectory, "Average normalized total loss per year with floods (for method 1).pdf"), width = 21, height = 21, units = "cm", plot = meanNormalizedLossMap1)

ggsave(file.path(mapDirectory, "Variance of normalized total loss per year with floods (for method 1).pdf"), width = 21, height = 21, units = "cm", plot = varianceNormalizedLossMap1)

ggsave(file.path(mapDirectory, "Average normalized total loss per year with floods (for method 2).pdf"), width = 21, height = 21, units = "cm", plot = meanNormalizedLossMap2)

ggsave(file.path(mapDirectory, "Variance of normalized total loss per year with floods (for method 2).pdf"), width = 21, height = 21, units = "cm", plot = varianceNormalizedLossMap2)

rm(normalizedLossMapData, meanNormalizedLossMap1, varianceNormalizedLossMap1, meanNormalizedLossMap2, varianceNormalizedLossMap2)


