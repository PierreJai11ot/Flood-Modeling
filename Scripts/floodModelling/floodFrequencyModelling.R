# Flood frequency modelling 

# Implementing additional relevant discrete distributions 

source(paste0(scriptDirectory, "/Scripts/floodModelling/zeroInflatedDistributions.R"))


# Evaluating the number of floods per year per country

frequencyModelsData <- dataHANZE %>% 
  group_by(Country) %>% 
  count(Year, sort = TRUE) %>%
  complete(Year = full_seq(startingYear:endingYear, 1), fill = list(n=0)) %>% rename(NumberOfFloods = n)


# Initializing the structure hosting the frequency models as well as their combined evaluation plots

frequencyModels <- tibble(
  Country = character(),       
  Poisson = list(),
  ZeroInflatedPoisson = list(), 
  NegativeBinomial = list(),
  ZeroInflatedNegativeBinomial = list(),
  Plots = list()
  )


# Defining a function to fit frequency models for a group of countries 

source(paste0(scriptDirectory, "/Scripts/floodModelling/frequencyModelsFittingFunction.R"))


# Fitting the frequency models for each country

for (currentCountry in relevantCountries$Country) {
  frequencyModels <- frequencyModelsFit(frequencyModelsData, frequencyModels, currentCountry, c(currentCountry))
}


# Fitting the frequency models for all countries combined 

frequencyModels <- frequencyModelsFit(frequencyModelsData, frequencyModels, "All Considered Countries", relevantCountries$Country)

# Storing the fitted models

resultsDirectory <- paste0(scriptDirectory, "/Results")

save(frequencyModels, file = file.path(paste0(resultsDirectory, "/frequencyModelsFit.RData")))


#  Generating plots relevant for the graphical analysis of the frequency models

# Introducing a function to generate the relevant plots to evaluate the quality of the frequency models

source(paste0(scriptDirectory, "/Scripts/floodModelling/frequencyModelsPlotsGenerator.R"))


# Generating the relevant plots for all our fitted frequency models

frequencyModels <- frequencyModels %>% 
  rowwise() %>% 
  mutate(plotList = list(frequencyModelPlots(Country, list(Poisson), list(ZeroInflatedPoisson), list(NegativeBinomial), list(ZeroInflatedNegativeBinomial), TRUE)))


# Introducing a function to combine all the plots of a given kind for all considered countries

source(paste0(scriptDirectory, "/Scripts/plotArrayCombiner.R"))


# Splitting the plots

batch1 <- frequencyModels$plotList[1:12]

batch2 <- frequencyModels$plotList[13:24]


# Generating and storing the combined probability mass and cumulative distribution functions for all out fitted models

combinedFrequencyPmfPlots1 <-  plotArrayCombiner (batch1, "pmf", "Empirical and theoretical PMFs of frequency models", 3, TRUE)

combinedFrequencyPmfPlots2 <-  plotArrayCombiner (batch2, "pmf", "Empirical and theoretical PMFs of frequency models", 3, TRUE)

combinedFrequencyCdfPlots1 <-  plotArrayCombiner (batch1, "cdf", "Empirical and theoretical CDFs of frequency models", 3, TRUE)

combinedFrequencyCdfPlots2 <-  plotArrayCombiner (batch2, "cdf", "Empirical and theoretical CDFs of frequency models", 3, TRUE)


# Storing the generated plots

ggsave(file.path(plotDirectory, "Empirical and theoretical PMFs of frequency models - Part 1.pdf"), width = 21, height = 29.7, units = "cm", plot = combinedFrequencyPmfPlots1)

ggsave(file.path(plotDirectory, "Empirical and theoretical PMFs of frequency models - Part 2.pdf"), width = 21, height = 29.7, units = "cm", plot = combinedFrequencyPmfPlots2)

ggsave(file.path(plotDirectory, "Empirical and theoretical CDFs of frequency models - Part 1.pdf"), width = 21, height = 29.7, units = "cm", plot = combinedFrequencyCdfPlots1)

ggsave(file.path(plotDirectory, "Empirical and theoretical CDFs of frequency models - Part 2.pdf"), width = 21, height = 29.7, units = "cm", plot = combinedFrequencyCdfPlots2)


rm(dzinbinom, dzipois, logLikelihoodZinbinom, logLikelihoodZipois, mleZinbinom, mleZipois, momZinbinom, momZipois, 
   pzinbinom, pzipois, qzinbinom, qzipois, rzinbinom, rzipois, zipoisFitter, zinbinomFitter)

rm(combinedFrequencyPmfPlots1, combinedFrequencyCdfPlots1, combinedFrequencyPmfPlots2, combinedFrequencyCdfPlots2, batch1, batch2)

rm(frequencyModelsData, frequencyModelPlots, frequencyModelsFit)
