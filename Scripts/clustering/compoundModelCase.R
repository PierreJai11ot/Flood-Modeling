# Compound model case

# Motivation for the compound model case 

frequencyToTotalSeverityCorrelation <- dataHANZE %>%
  filter(Country != "Lithuania") %>%
  group_by(Country, Year) %>%
  summarise(
    count = dplyr::n(),
    totalYearlySeverity = sum(!!as.symbol(retainedLossMetric), na.rm = TRUE),
    .groups = "drop"
  ) %>%
  arrange(Country, Year)

frequencyToTotalSeverityCorrelation <- frequencyToTotalSeverityCorrelation %>%
  complete(Country, Year = 1950:2020,
         fill = list(count = 0, totalYearlySeverity = 0)) %>%
  arrange(Country, Year)

frequencyToTotalSeverityCorrelation <- frequencyToTotalSeverityCorrelation %>%
  group_by(Country) %>%
  summarise(pearsonCorrelation = cor(count, totalYearlySeverity, method = "pearson"),
            spearmanCorrelation = cor(count, totalYearlySeverity, method = "spearman"))

# MENTION ISSUE WITH THE NUMEROUS ZEROES 
# PEARSON OR SPEARMAN ? SPEARMAN IN OUR CASE IS EASIER 

countries <- setdiff(unique(dataHANZE$Country), c("Lithuania"))

correlationFormattedData <- dataHANZE %>% 
  filter(Country != "Lithuania") %>%
  group_by(CountryISO, Year) %>% 
  summarise(
    count = dplyr::n(),
    .groups = "drop"
  ) %>%
  complete(CountryISO, Year = 1950:2020, fill = list(count = 0)) %>%
  pivot_wider(names_from = CountryISO, values_from = count) %>%
  arrange(Year) %>%
  dplyr::select(-Year) %>%
  as.matrix()

# Generating the Pearson and Spearman rank correlation tables for our loss data

pearsonCorrelationMatrix <- cor(correlationFormattedData, method = "pearson", use = "pairwise.complete.obs")

spearmanRankCorrelationMatrix <- cor(correlationFormattedData, method = "spearman", use = "pairwise.complete.obs")

caption1 <- "Pearson correlation matrix for the national yearly number of flood events"
pearsonCorrelationMatrixTeX <- kable(round(pearsonCorrelationMatrix, 2), format = "latex", booktabs = TRUE, caption = caption1, escape = FALSE)
# frequencyTable <- kable(frequencyTableData, format = "latex", booktabs = TRUE, escape = FALSE, caption = frequencyCaption) %>% kable_styling(latex_options = c("hold_position"))
pearsonCorrelationMatrixTeX <- tableCleaner(pearsonCorrelationMatrixTeX)
writeLines(pearsonCorrelationMatrixTeX,
           file.path(paste0(scriptDirectory, "/Texts"), 
                     "Pearson correlation matrix for the national yearly number of flood events.txt"))

caption1 <- "Spearman's rank correlation matrix for the national yearly number of flood events"
spearmanRankCorrelationMatrixTeX <- kable(round(spearmanRankCorrelationMatrix, 2), format = "latex", booktabs = TRUE, caption = caption1, escape = FALSE)
# frequencyTable <- kable(frequencyTableData, format = "latex", booktabs = TRUE, escape = FALSE, caption = frequencyCaption) %>% kable_styling(latex_options = c("hold_position"))
spearmanRankCorrelationMatrixTeX <- tableCleaner(spearmanRankCorrelationMatrixTeX)
writeLines(spearmanRankCorrelationMatrixTeX,
           file.path(paste0(scriptDirectory, "/Texts"), 
                     "Spearman's rank correlation matrix for the national yearly number of flood events.txt"))

  
# Generating the corresponding dissimilarity matrices 

pearsonDissimilarityMatrix <- correlationToDissimilarityConverter(pearsonCorrelationMatrix, "Half1PlusCorr")

spearmanDissimilarityMatrix <- correlationToDissimilarityConverter(spearmanRankCorrelationMatrix, "Half1PlusCorr")


# Generating the clusters through AGNES hierarchical clustering 

# Linkage methods : complete, average or single

numberOfClusters <- 6

hierarchicalClustering <- hclust(as.dist(spearmanDissimilarityMatrix), method = "average")

partitionClusters <-  cutree(hierarchicalClustering, k = numberOfClusters)

clustersList <- clusterConverter(partitionClusters)

reformattedList <- list()

for (i in 1:length(clustersList)){
  reformattedList <- append(reformattedList, list(clustersList[[i]]))
}

clustersList <- reformattedList

rm(reformattedList)


# Generating a dendogram corresponding to the hierarchical clustering and highlighting the chosen partition

# BALISE ADJUST STRINGS TITLE AND LEGEND
dendogram <- dendogramGenerator(hierarchicalClustering, numberOfClusters, c("blue", "red", "yellow", "green", "purple", "turquoise"), "Hierarchical Clustering", xlabel = " ", ylabel = "height")

dendogram

ggsave(file.path(paste0(plotDirectory, "/", "Dendogram - Compound.pdf")), width = 21, height = 21, units = "cm", plot = dendogram)


# Generating the clustered loss data 

clusterCompoundData <- clusterDataGenerator(dataHANZE, severityNormalizationDataHANZE, clustersList, commonYearsOnly = FALSE, isISO = TRUE, isCompound = TRUE)


# Retaining the retained loss metric

clusterCompoundSeverityData <- clusterCompoundData %>% rename(loss = !!as.symbol(retainedLossMetric))


# Extracting the frequency data

clusterCompoundFrequencyData <- clusterCompoundData %>%
  group_by(clusterID, cluster, Year) %>%
  summarise(occurences = dplyr::n(), .groups = "drop") %>%
  complete(clusterID, Year = 1950:2020, fill = list(occurences = 0)) %>%
  rename(clusterID2 = clusterID) %>%
  rowwise() %>%
  mutate(cluster = (clusterCompoundData %>%
                      group_by(clusterID, cluster) %>%
                      summarise(placeholder = 1, .groups = "drop") %>% 
                      filter(clusterID == clusterID2) %>%
                      pull(cluster))) %>%
  rename(clusterID = clusterID2) 




# Flood severity modelling 

# Implementing additional relevant continuous distributions 

source(paste0(scriptDirectory, "/Scripts/floodModelling/paretoDistributions.R"))
source(paste0(scriptDirectory, "/Scripts/floodModelling/severityModelsFittingFunction.R"))
source(paste0(scriptDirectory, "/Scripts/floodModelling/severityModelsPlotsGenerator.R"))

# Initializing the structure hosting severity models

compoundClusterSeverityModels <- tibble(
  Country = character(),       
  LogNormal = list(),
  Weibull = list(),
  Pareto = list(), 
  TruncatedPareto = list(), 
  Plots = list())

clusterCompoundSeverityData <- clusterCompoundSeverityData %>%
  dplyr::select(- Country) %>%
  rename(Country = clusterID)

# Fitting the frequency models for each country

for (currentID in sort(unique(clusterCompoundSeverityData$Country))) {
  
  currentCluster <- unlist(unique(
    clusterCompoundSeverityData %>% 
      filter(Country == currentID) %>% 
      pull(cluster)))
  
  currentCluster <- sort(unique(
    relevantCountries %>% 
      filter(CountryISO %in% currentCluster) %>%
      pull(Country)))
  
  print("hello")
  print(currentID)
  # group2ClusterTotalSeverityModels <- severityModelsFit(clusterData, "loss", group2ClusterTotalSeverityModels, paste0("Cluster ", currentID), c(currentID))
  compoundClusterSeverityModels <- severityModelsFit(dataHANZE, retainedLossMetric, compoundClusterSeverityModels, paste0("Cluster ", currentID), currentCluster)
  # compoundClusterSeverityModels <- severityModelsFit(clusterCompoundSeverityData, "loss", compoundClusterSeverityModels, paste0("Cluster ", currentID), c(currentID))
}


#  Generating plots relevant for the graphical analysis of the severity models

compoundClusterSeverityModels <- compoundClusterSeverityModels %>% 
  rowwise() %>% 
  mutate(plotList = list(severityModelPlots(Country, list(LogNormal), list(Weibull), list(Pareto), list(TruncatedPareto), list(Plots), zeroAsMinObs = TRUE)))


# Splitting the plots

batch1 <- compoundClusterSeverityModels$plotList[1:6]


# Combining and naming the combined plots

combinedCompoundClusterSeverityPlotList <- list()

combinedPlotsInstuctions <- tibble(
  tag = c("pdf", "cdf", "cdfLog", "qq", "qqLog", "pp", "paretoQQ", "truncatedParetoQQ", "estimatorAlphaVSInverseHill", "estimatorD", "estimatorQ", "estimatorT", "statisticalTest"), 
  title = c("Empirical and theoretical PDFs of severity models", 
            "Empirical and theoretical CDFs of severity models", 
            "Empirical and theoretical log-scale CDFs of severity models",
            "Q-Q Plot for severity models", 
            "Q-Q Plot in log scale for severity models",
            "P-P Plot for severity models", 
            "Pareto Q-Q plot",
            "Truncated Pareto Q-Q plot",
            "Inverse Hill statistic and shape estimator against extreme value subset size", 
            "Estimator of the odds ratio against the extreme value subset size", 
            "Estimator of the extreme quantile against the extreme value subset size", 
            "Estimator of the upper parameter T against the extreme value subset size", 
            "Statistical truncation tests for against the extreme value subset size"
  )
)


# Generating and storing the combined evaluation plots

numberOfColumns <- 3

for (i in 1:nrow(combinedPlotsInstuctions)) {
  plotbatch1 <- plotArrayCombiner (batch1, combinedPlotsInstuctions$tag[i], combinedPlotsInstuctions$title[i], numberOfColumns, TRUE)
  
  ggsave(file.path(paste0(plotDirectory, "/", combinedPlotsInstuctions$title[i], " - compound cluster.pdf")), width = 21, height = 19.8, units = "cm", plot = plotbatch1)
  
  combinedCompoundClusterSeverityPlotList <- c(combinedCompoundClusterSeverityPlotList, list(list(batch1)))
}



# "LogNormal", "TruncatedPareto", "Weibull", "Pareto",
compoundClusterSeverityModelSelection <- c(
  `Cluster 1` = "LogNormal", 
  `Cluster 2` = "LogNormal", 
  `Cluster 3` = "LogNormal",
  `Cluster 4` = "TruncatedPareto", 
  `Cluster 5` = "Weibull", 
  `Cluster 6` = "LogNormal"
)


# Applying the selection

compoundClusterSeverityModels <- compoundClusterSeverityModels %>%
  mutate(Country = as.character(Country)) %>%
  mutate(selectedModel = compoundClusterSeverityModelSelection[Country]) %>%
  pivot_longer(cols = c(LogNormal, Weibull, Pareto, TruncatedPareto), names_to = "ModelType", values_to = "Model") %>%
  filter(ModelType == selectedModel) %>%
  dplyr::select(Country, selectedModel, Model)  %>%
  rename(selectedModel.y = selectedModel, 
         Model.y = Model)












compoundClusterFrequencyModels <- tibble(
  Country = character(),       
  Poisson = list(),
  ZeroInflatedPoisson = list(), 
  NegativeBinomial = list(),
  ZeroInflatedNegativeBinomial = list(),
  Plots = list()
)


# Defining a function to fit frequency models for a group of countries 

source(paste0(scriptDirectory, "/Scripts/floodModelling/frequencyModelsFittingFunction.R"))
source(paste0(scriptDirectory, "/Scripts/floodModelling/frequencyModelsPlotsGenerator.R"))
source(paste0(scriptDirectory, "/Scripts/plotArrayCombiner.R"))



clusterCompoundFrequencyData <- clusterCompoundFrequencyData %>%
  rename(Country = clusterID) %>%
  rename(NumberOfFloods = occurences)

# Fitting the frequency models for each country

for (currentID in sort(unique(clusterCompoundSeverityData$Country))) {
  compoundClusterFrequencyModels <- frequencyModelsFit(clusterCompoundFrequencyData, compoundClusterFrequencyModels, paste0("Cluster ", currentID), c(currentID))
}


# Generating the relevant plots for all our fitted frequency models

compoundClusterFrequencyModels <- compoundClusterFrequencyModels %>% 
  rowwise() %>% 
  mutate(plotList = list(frequencyModelPlots(Country, list(Poisson), list(ZeroInflatedPoisson), list(NegativeBinomial), list(ZeroInflatedNegativeBinomial), TRUE)))

batch1 <- compoundClusterFrequencyModels$plotList[1:6]

# Generating and storing the combined probability mass and cumulative distribution functions for all out fitted models

combinedCompoundClusterFrequencyPmfPlots1 <-  plotArrayCombiner (batch1, "pmf", "Empirical and theoretical PMFs of frequency models", 3, TRUE)

combinedCompoundClusterFrequencyCdfPlots1 <-  plotArrayCombiner (batch1, "cdf", "Empirical and theoretical CDFs of frequency models", 3, TRUE)

# Storing the generated plots

ggsave(file.path(plotDirectory, "Empirical and theoretical PMFs of frequency models - compound cluster.pdf"), width = 21, height = 19.8, units = "cm", plot = combinedCompoundClusterFrequencyPmfPlots1)

ggsave(file.path(plotDirectory, "Empirical and theoretical CDFs of frequency models - compound cluster.pdf"), width = 21, height = 19.8, units = "cm", plot = combinedCompoundClusterFrequencyCdfPlots1)


# "LogNormal", "TruncatedPareto", "Weibull", "Pareto",
compoundClusterFrequencyModelSelection <- c(
  `Cluster 1` = "NegativeBinomial", 
  `Cluster 2` = "NegativeBinomial", 
  `Cluster 3` = "NegativeBinomial",
  `Cluster 4` = "Poisson", 
  `Cluster 5` = "NegativeBinomial", 
  `Cluster 6` = "NegativeBinomial"
)


# Applying the selection

compoundClusterFrequencyModels <- compoundClusterFrequencyModels %>%
  mutate(Country = as.character(Country)) %>%
  mutate(selectedModel = compoundClusterFrequencyModelSelection[Country]) %>%
  pivot_longer(cols = c(Poisson, ZeroInflatedPoisson, NegativeBinomial, ZeroInflatedNegativeBinomial), names_to = "ModelType", values_to = "Model") %>%
  filter(ModelType == selectedModel) %>%
  dplyr::select(Country, selectedModel, Model)  %>%
  rename(selectedModel.x = selectedModel, 
         Model.x = Model)

compoundClusterCombinedModels <- compoundClusterFrequencyModels %>%
  left_join(compoundClusterSeverityModels, by = "Country")






snapshotName <- paste0(resultsDirectory, "/snapPost_issuecompclus_newver.RData")
save.image(file = snapshotName)
# load(snapshotName)


compoundClusterCombinedModels <- compoundClusterCombinedModels %>% 
  # rename(label = Country) %>%
  rowwise() %>%
  mutate(clusterID = as.integer(str_remove(Country, "Cluster "))) %>%
  mutate(cluster = (unique(
    clusterCompoundSeverityData %>%
      filter(Country == clusterID) %>%
      pull(cluster)
  ))) %>%
  mutate(cluster = list(sort(unique(filter(relevantCountries, CountryISO %in% cluster)$Country))))

compoundClusterCombinedModelsData <- compoundClusterCombinedModels %>%
  rowwise() %>%
  mutate(
    clusterLength = length(cluster),
    simulationPureSeverity = ifelse(
      (clusterLength == 1),
      list((pureSeveritySimulationData %>%
              filter(Country == unlist(cluster)) %>% 
              pull(simulation))[[1]]),
      list(
        monteCarloSimulation(
          frequencyModel = Model.x, 
          frequencyModelName = "Pure Severity Simulation", 
          severityModel = Model.y, 
          severityModelName = selectedModel.y, 
          iterations = iterations
        ) %>%
          mutate(SimulationIndex = row_number())
      )
    )
  ) %>%
  group_by()

compoundClusterCombinedModelsData <- compoundClusterCombinedModels %>%
  rowwise() %>%
  mutate(
    clusterLength = length(cluster),
    simulation = ifelse(
      (clusterLength == 1),
      list((simulationData %>%
              filter(Country == unlist(cluster)) %>% 
              pull(simulation))[[1]]),
      list(
        monteCarloSimulation(
          frequencyModel = Model.x, 
          frequencyModelName = selectedModel.x, 
          severityModel = Model.y, 
          severityModelName = selectedModel.y, 
          iterations = iterations
        ) %>%
          mutate(SimulationIndex = row_number()))
    )
  ) %>%
  ungroup()



compoundClusterCombinedModelsData <- compoundClusterCombinedModelsData %>%
  rename(label = Country) %>%
  rowwise() %>%
  mutate(clusterID = as.integer(str_remove(label, "Cluster "))) %>%
  mutate(cluster = (unique(
    clusterCompoundData %>%
      dplyr::select(-Country) %>%
      rename(Country = clusterID) %>%
      filter(Country == clusterID) %>%
      pull(cluster)
  )))

conversionIndex <- ifelse(retainedLossMetric == "normalizedLossTotalVulnerability", 3, 2)

compoundClusterCombinedModelsData <- compoundClusterCombinedModelsData %>% 
  rowwise() %>%
  mutate(clusterNormalizationData = 
           list(clusterNormalizationDataFetcher(cluster, severityNormalizationDataHANZE, 2020, isISO = TRUE))) %>%
  mutate(simulatedSample2020Euros = 
           list(unlist(simulation$totalSeverity) * unlist(clusterNormalizationData)[conversionIndex])) 

rm(conversionIndex)

compoundClusterCombinedModelsData <- compoundClusterCombinedModelsData %>% 
  rowwise() %>%
  mutate(VaRSev = valueAtRisk(unlist(simulation$totalSeverity)), 
         ESSev = expectedShortfall(unlist(simulation$totalSeverity)), 
         valueAtRisk = valueAtRisk(unlist(simulatedSample2020Euros)), 
         expectedShortfall = expectedShortfall(unlist(simulatedSample2020Euros))) %>%
  rename(ID = clusterID)


# BALISE
# Generating tables illustrating the impact of the clustering 
# 
# tableData <- clusterTableDataGenerator(totalSeveritySimulationData, clusterSeverityModels, T)
#   
# test2 <- clusterTableLaTeXGenerator(tableData, "title")

writeLines(clusterTableLaTeXGenerator(clusterTableDataGenerator(simulationData, compoundClusterCombinedModelsData, T), 
                                      "Risk Measures for the correlation based 3 Clustering compound"),
           file.path(paste0(scriptDirectory, "/Texts"), "Risk Measures for the correlation based 3 Clustering compound.txt"))




frequencyDistributionNamesConversion <- tibble(
  name = c("Poisson", "NegativeBinomial", "ZeroInflatedPoisson", "ZeroInflatedNegativeBinomial"), 
  nickname = c("PO", "NB", "ZP", "ZN")
)

severityDistributionNamesConversion <- tibble(
  name = c("LogNormal", "TruncatedPareto", "Weibull", "Pareto"), 
  nickname = c("LN", "TP", "WE", "PA")
)

tableData <- tibble(
  clusterID = compoundClusterCombinedModelsData$ID
)

compoundClusterSeverityTableData <- tableData %>%
  rowwise() %>%
  mutate(
    SeverityDistribution = filter(compoundClusterCombinedModelsData, ID == clusterID)$selectedModel.y[1],
    SeverityDistribution = filter(severityDistributionNamesConversion, name == SeverityDistribution)$nickname[1],
    
    SeveritySimulation = list(filter(compoundClusterCombinedModelsData, ID == clusterID)$simulation[[1]]$totalSeverity),
    SeverityEmpirical = list(filter(clusterCompoundSeverityData, Country == clusterID)$loss),
    
    meanSeverityEmpirical = mean(SeverityEmpirical),
    meanSeveritySimulated = mean(SeveritySimulation),
    
    standardDeviationSeverityEmpirical = sd(SeverityEmpirical),
    standardDeviationSeveritySimulated = sd(SeveritySimulation),
    
    coeffVariationSeverityEmpirical = standardDeviationSeverityEmpirical / meanSeverityEmpirical,
    coeffVariationSeveritySimulated = standardDeviationSeveritySimulated / meanSeveritySimulated,
    
    medianSeverityEmpirical = median(SeverityEmpirical),
    medianSeveritySimulated = median(SeveritySimulation),
    
    skewnessSeverityEmpirical = e1071::skewness(SeverityEmpirical),
    skewnessSeveritySimulated = e1071::skewness(SeveritySimulation),
    
    kurtosisSeverityEmpirical = e1071::kurtosis(SeverityEmpirical),
    kurtosisSeveritySimulated = e1071::kurtosis(SeveritySimulation),
    
    # Formatting 
    
    meanSeverityEmpirical = formatC(meanSeverityEmpirical, format = "e", digits = 2),
    meanSeveritySimulated = formatC(meanSeveritySimulated, format = "e", digits = 2),
    
    standardDeviationSeverityEmpirical = formatC(sd(SeverityEmpirical), format = "e", digits = 2),
    standardDeviationSeveritySimulated = formatC(sd(SeveritySimulation), format = "e", digits = 2),
    
    coeffVariationSeverityEmpirical = round(coeffVariationSeverityEmpirical, 3),
    coeffVariationSeveritySimulated = round(coeffVariationSeveritySimulated, 3),
    
    medianSeverityEmpirical = formatC(medianSeverityEmpirical, format = "e", digits = 2),
    medianSeveritySimulated = formatC(medianSeveritySimulated, format = "e", digits = 2),
    
    skewnessSeverityEmpirical = formatC(skewnessSeverityEmpirical, 3),
    skewnessSeveritySimulated = formatC(skewnessSeveritySimulated, 3),
    
    kurtosisSeverityEmpirical = round(kurtosisSeverityEmpirical, 3),
    kurtosisSeveritySimulated = formatC(kurtosisSeveritySimulated, 3)
  ) %>%
  ungroup() %>%
  dplyr::select(-SeveritySimulation, -SeverityEmpirical)    %>%
  rename('Cluster' = clusterID, 
         'Dist.' = SeverityDistribution, 
         '$\\underset{\\text{Emp.}}{\\text{Mean}}$' = meanSeverityEmpirical, 
         '$\\underset{\\text{Sim.}}{\\text{Mean}}$' = meanSeveritySimulated, 
         '$\\underset{\\text{Emp.}}{\\sigma}$' = standardDeviationSeverityEmpirical,
         '$\\underset{\\text{Sim.}}{\\sigma}$' = standardDeviationSeveritySimulated, 
         '$\\underset{\\text{Emp.}}{\\text{CV}}$' = coeffVariationSeverityEmpirical,
         '$\\underset{\\text{Sim.}}{\\text{CV}}$' = coeffVariationSeveritySimulated,
         '$\\underset{\\text{Emp.}}{\\text{Med.}}$' = medianSeverityEmpirical,
         '$\\underset{\\text{Sim.}}{\\text{Med.}}$' = medianSeveritySimulated,
         '$\\underset{\\text{Emp.}}{\\text{Skew}}$' = skewnessSeverityEmpirical,
         '$\\underset{\\text{Sim.}}{\\text{Skew}}$' = skewnessSeveritySimulated,
         '$\\underset{\\text{Emp.}}{\\text{Kurt}}$' = kurtosisSeverityEmpirical,
         '$\\underset{\\text{Sim.}}{\\text{Kurt}}$' = kurtosisSeveritySimulated
  )

compoundClusterSeverityTableDataBatch1 <- compoundClusterSeverityTableData %>% 
  dplyr::select('Cluster', 
                'Dist.', 
                '$\\underset{\\text{Emp.}}{\\text{Mean}}$', 
                '$\\underset{\\text{Sim.}}{\\text{Mean}}$',
                '$\\underset{\\text{Emp.}}{\\sigma}$',
                '$\\underset{\\text{Sim.}}{\\sigma}$',
                '$\\underset{\\text{Emp.}}{\\text{CV}}$',
                '$\\underset{\\text{Sim.}}{\\text{CV}}$')

caption1 <- "Comparison of empirical and simulated total flood severity distributions by cluster - Part 1"
compoundClusterSeverityTable1 <- kable(compoundClusterSeverityTableDataBatch1, format = "latex", booktabs = TRUE, caption = caption1, escape = FALSE)
compoundClusterSeverityTable1 <- tableCleaner(compoundClusterSeverityTable1)

writeLines(compoundClusterSeverityTable1,
           file.path(paste0(scriptDirectory, "/Texts"), 
                     "Comparison of empirical and simulated total flood severity distributions by cluster - Part 1 - Compound.txt"))

compoundClusterSeverityTableDataBatch1 <- compoundClusterSeverityTableData %>% 
  dplyr::select('Cluster',
                'Dist.',
                '$\\underset{\\text{Emp.}}{\\text{Med.}}$',
                '$\\underset{\\text{Sim.}}{\\text{Med.}}$',
                '$\\underset{\\text{Emp.}}{\\text{Skew}}$',
                '$\\underset{\\text{Sim.}}{\\text{Skew}}$',
                '$\\underset{\\text{Emp.}}{\\text{Kurt}}$',
                '$\\underset{\\text{Sim.}}{\\text{Kurt}}$')

caption2 <- "Comparison of empirical and simulated total flood severity distributions by cluster - Part 2"
compoundClusterSeverityTable2 <- kable(compoundClusterSeverityTableDataBatch1, format = "latex", booktabs = TRUE, caption = caption2, escape = FALSE)
compoundClusterSeverityTable2 <- tableCleaner(compoundClusterSeverityTable2)

writeLines(compoundClusterSeverityTable2,
           file.path(paste0(scriptDirectory, "/Texts"), 
                     "Comparison of empirical and simulated total flood severity distributions by cluster - Part 2 - Compound.txt"))





compoundClusterFrequencyTableData <- tableData %>%
  rowwise() %>%
  mutate(
    FrequencyDistribution = filter(compoundClusterCombinedModelsData, ID == clusterID)$selectedModel.x[1],
    FrequencyDistribution = filter(frequencyDistributionNamesConversion, name == FrequencyDistribution)$nickname[1],
    
    FrequencyEmpirical = list(filter(clusterCompoundFrequencyData, Country == clusterID)$NumberOfFloods),
    FrequencySimulation = list(filter(compoundClusterCombinedModelsData, ID == clusterID)$simulation[[1]]$frequency), 
    
    meanFrequencyEmpirical = round(mean(FrequencyEmpirical), 2),
    meanFrequencySimulated = round(mean(FrequencySimulation), 2),
    
    standardDeviationFrequencyEmpirical = round(sd(FrequencyEmpirical), 2),
    standardDeviationFrequencySimulated = round(sd(FrequencySimulation), 2),
    
    coeffVariationFrequencyEmpirical = round(standardDeviationFrequencyEmpirical/meanFrequencyEmpirical, 2),
    coeffVariationFrequencySimulated = round(standardDeviationFrequencySimulated/meanFrequencySimulated, 2),
    
    medianFrequencyEmpirical = round(median(FrequencyEmpirical), 2),
    medianFrequencySimulated = round(median(FrequencySimulation), 2)
  ) %>%
  ungroup() %>%
  dplyr::select(-FrequencyEmpirical, -FrequencySimulation) %>%
  rename('Cluster' = clusterID, 
         'Dist.' = FrequencyDistribution, 
         '$\\underset{\\text{Emp.}}{\\text{Mean}}$' = meanFrequencyEmpirical, 
         '$\\underset{\\text{Sim.}}{\\text{Mean}}$' = meanFrequencySimulated, 
         '$\\underset{\\text{Emp.}}{\\sigma}$' = standardDeviationFrequencyEmpirical,
         '$\\underset{\\text{Sim.}}{\\sigma}$' = standardDeviationFrequencySimulated, 
         '$\\underset{\\text{Emp.}}{\\text{CV}}$' = coeffVariationFrequencyEmpirical,
         '$\\underset{\\text{Sim.}}{\\text{CV}}$' = coeffVariationFrequencySimulated,
         '$\\underset{\\text{Emp.}}{\\text{Med.}}$' = medianFrequencyEmpirical,
         '$\\underset{\\text{Sim.}}{\\text{Med.}}$' = medianFrequencySimulated
  )


frequencyCaption <- "Comparison of empirical and simulated flood frequency distributions by cluster"
frequencyTable <- kable(compoundClusterFrequencyTableData, format = "latex", booktabs = TRUE, caption = frequencyCaption, escape = FALSE)
# frequencyTable <- kable(frequencyTableData, format = "latex", booktabs = TRUE, escape = FALSE, caption = frequencyCaption) %>% kable_styling(latex_options = c("hold_position"))
frequencyTable <- tableCleaner(frequencyTable)
writeLines(frequencyTable,
           file.path(paste0(scriptDirectory, "/Texts"), 
                     "Comparison of empirical and simulated flood frequency distributions by cluster - Compound.txt"))


# Sanity check
# %>%
#   group_by(clusterID, cluster) %>%
#   summarise(placeholder = dplyr::n(), .groups = "drop")
# 
# {
# # Initializing the structure to store the compound models fitted to the selected clusters
# 
# clusterCompoundModels <- tibble(
#   ID = 1:length(clustersList), 
#   cluster = clustersList
# )
# 

# Extracting the frequency and loss data of each cluster from the cluster data and the normalization data necessary to the conversion of simulated samples back to 2020 Euros 

# clusterCompoundSeverityData <- clusterCompoundSeverityData %>% 
#   dplyr::select(clusterID, cluster, loss)
# 
# list(filter(clusterCompoundSeverityData, clusterID == clusterCompoundModels$ID[1])$loss)
# 
# clusterCompoundModels <- clusterCompoundModels %>%
#   rowwise() %>%
#   mutate(
#     clusterLength = length(unlist(cluster)),
#     severitySample = list(filter(clusterCompoundSeverityData, clusterID == ID)[1]$loss),
#     frequencySample = list(filter(clusterCompoundFrequencyData, clusterID == ID)$occurences),
#     severitySampleLength = length(unlist(severitySample)),
#     clusterNormalizationData = list(clusterNormalizationDataFetcher(cluster, severityNormalizationDataHANZE, 2020, isISO = TRUE))
#   ) %>%
#   ungroup()
# 
# 
# # BALISE
# # DISTRIBUTION SELECTION CHECK 
# 
# clusterCompoundModels <- clusterCompoundModels %>%
#   filter(severitySampleLength > 1) %>%
#   mutate(severityModel = list(fitdist(data = unlist(severitySample), distr = "lnorm"))) %>%
#   mutate(frequencyModel = list(fitdist(data = unlist(frequencySample), distr = "pois"))) 
# 
# iterations <- 10000
# 
# # Proceeding with the Monte Carlo simulation
# 
# clusterCompoundModels <- clusterCompoundModels %>% 
#   rowwise() %>%
#   mutate(simulation = list(
#     monteCarloSimulation(
#     frequencyModel = list(frequencyModel), 
#     frequencyModelName = "Poisson", 
#     severityModel = list(severityModel), 
#     severityModelName = "LogNormal", 
#     iterations = iterations
#     )
#   ))
# 
# clusterCompoundModels <- clusterCompoundModels %>%
#   rowwise() %>%
#   mutate(simulationTotalSample = list(simulation$totalSeverity)
#          )
# 
# 
# # Converting the simulated sample into 2020 Euros
# 
# conversionIndex <- ifelse(retainedLossMetric == "normalizedLossTotalVulnerability", 3, 2)
# 
# clusterCompoundModels <- clusterCompoundModels %>% 
#   rowwise() %>%
#   mutate(simulatedTotalSample2020Euros = list(unlist(simulationTotalSample) * unlist(clusterNormalizationData)[conversionIndex])) 
# 
# rm(conversionIndex)
# 
# 
# # Estimating our risk assessment metrics 
# 
# clusterCompoundModels <- clusterCompoundModels %>% 
#   rowwise() %>%
#   mutate(VaRSev = valueAtRisk(unlist(simulationTotalSample)), 
#          ESSev = expectedShortfall(unlist(simulationTotalSample)), 
#          valueAtRisk = valueAtRisk(unlist(simulatedTotalSample2020Euros)), 
#          expectedShortfall = expectedShortfall(unlist(simulatedTotalSample2020Euros)))
# 
# 
# # Generating tables illustrating the impact of the clustering 
# # 
# # tableData <- clusterTableDataGenerator(totalSeveritySimulationData, clusterSeverityModels, T)
# #   
# # test2 <- clusterTableLaTeXGenerator(tableData, "title")
# 
# clusterTableDataGenerator(simulationData, clusterCompoundModels, T)
# 
# writeLines(clusterTableLaTeXGenerator(clusterTableDataGenerator(simulationData, clusterCompoundModels, T), 
#                                       "Risk Measures for the correlation based 6 Clustering"),
#            file.path(paste0(scriptDirectory, "/Texts"), "Risk Measures for the correlation based 6 Clustering.txt"))
# }
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# # Flood frequency modelling functions
# 
# source(paste0(scriptDirectory, "/Scripts/floodModelling/zeroInflatedDistributions.R"))
# source(paste0(scriptDirectory, "/Scripts/floodModelling/frequencyModelsFittingFunction.R"))
# 
# 
# # Initializing the structure hosting the frequency models as well as their combined evaluation plots
# 
# frequencyModels <- tibble(
#   Country = character(),       
#   Poisson = list(),
#   ZeroInflatedPoisson = list(), 
#   NegativeBinomial = list(),
#   ZeroInflatedNegativeBinomial = list(),
#   Plots = list()
# )
# 
# 
# # Defining a function to fit frequency models for a group of countries 
# 
# 
# 
# 
# # Fitting the frequency models for each country
# 
# for (currentCountry in relevantCountries$Country) {
#   frequencyModels <- frequencyModelsFit(frequencyModelsData, frequencyModels, currentCountry, c(currentCountry))
# }
# 
# 
# # Fitting the frequency models for all countries combined 
# 
# frequencyModels <- frequencyModelsFit(frequencyModelsData, frequencyModels, "All Considered Countries", relevantCountries$Country)
# 
# # Storing the fitted models
# 
# resultsDirectory <- paste0(scriptDirectory, "/Results")
# 
# save(frequencyModels, file = file.path(paste0(resultsDirectory, "/frequencyModelsFit.RData")))
# 
# 
# #  Generating plots relevant for the graphical analysis of the frequency models
# 
# # Introducing a function to generate the relevant plots to evaluate the quality of the frequency models
# 
# source(paste0(scriptDirectory, "/Scripts/floodModelling/frequencyModelsPlotsGenerator.R"))
# 
# 
# # Generating the relevant plots for all our fitted frequency models
# 
# frequencyModels <- frequencyModels %>% 
#   rowwise() %>% 
#   mutate(plotList = list(frequencyModelPlots(Country, list(Poisson), list(ZeroInflatedPoisson), list(NegativeBinomial), list(ZeroInflatedNegativeBinomial), TRUE)))
# 
# 
# # Introducing a function to combine all the plots of a given kind for all considered countries
# 
# source(paste0(scriptDirectory, "/Scripts/plotArrayCombiner.R"))
# 
# 
# # Splitting the plots
# 
# batch1 <- frequencyModels$plotList[1:12]
# 
# batch2 <- frequencyModels$plotList[13:24]
# 
# 
# # Generating and storing the combined probability mass and cumulative distribution functions for all out fitted models
# 
# combinedFrequencyPmfPlots1 <-  plotArrayCombiner (batch1, "pmf", "Empirical and theoretical PMFs of frequency models", 3, TRUE)
# 
# combinedFrequencyPmfPlots2 <-  plotArrayCombiner (batch2, "pmf", "Empirical and theoretical PMFs of frequency models", 3, TRUE)
# 
# combinedFrequencyCdfPlots1 <-  plotArrayCombiner (batch1, "cdf", "Empirical and theoretical CDFs of frequency models", 3, TRUE)
# 
# combinedFrequencyCdfPlots2 <-  plotArrayCombiner (batch2, "cdf", "Empirical and theoretical CDFs of frequency models", 3, TRUE)
# 
# 
# # Storing the generated plots
# 
# ggsave(file.path(plotDirectory, "Empirical and theoretical PMFs of frequency models - Part 1.pdf"), width = 21, height = 29.7, units = "cm", plot = combinedFrequencyPmfPlots1)
# 
# ggsave(file.path(plotDirectory, "Empirical and theoretical PMFs of frequency models - Part 2.pdf"), width = 21, height = 29.7, units = "cm", plot = combinedFrequencyPmfPlots2)
# 
# ggsave(file.path(plotDirectory, "Empirical and theoretical CDFs of frequency models - Part 1.pdf"), width = 21, height = 29.7, units = "cm", plot = combinedFrequencyCdfPlots1)
# 
# ggsave(file.path(plotDirectory, "Empirical and theoretical CDFs of frequency models - Part 2.pdf"), width = 21, height = 29.7, units = "cm", plot = combinedFrequencyCdfPlots2)
# 
# 
# rm(dzinbinom, dzipois, logLikelihoodZinbinom, logLikelihoodZipois, mleZinbinom, mleZipois, momZinbinom, momZipois, 
#    pzinbinom, pzipois, qzinbinom, qzipois, rzinbinom, rzipois, zipoisFitter, zinbinomFitter)
# 
# rm(combinedFrequencyPmfPlots1, combinedFrequencyCdfPlots1, combinedFrequencyPmfPlots2, combinedFrequencyCdfPlots2, batch1, batch2)
# 
# rm(frequencyModelsData, frequencyModelPlots, frequencyModelsFit)
# 
# 
# 
# 
# 
# # 
# # clusterSeverityModels <- tibble(
# #   ID = 1:length(clustersList), 
# #   cluster = clustersList
# # )
# # 
# # clusterSeverityModels <- clusterSeverityModels %>% 
# #   rowwise() %>% 
# #   mutate(clusterLength = length(unlist(cluster))) %>% 
# #   mutate(sample = list(filter(clusterData, clusterID == ID)$loss)) %>% 
# #   mutate(sampleLenth = length(unlist(sample))) %>% 
# #   mutate(clusterNormalizationData = list(
# #     clusterNormalizationDataFetcher(cluster, severityNormalizationDataHANZE, 2020, isISO = TRUE)
# #   )) 
# 
# # Initializing the structure to store the severity models fitted to the potential clusters
# 
# 
# 
# 
# # Extracting the loss data of each cluster from the cluster data and the normalization data necessary to the conversion of simulated samples back to 2020 Euros 
# 
# # BALISE MOST REPRESENTED MODEL ? 
# 


# Fitting the severity models to the ret

# clusterSeverityModels <- clusterSeverityModels %>% 
#   rowwise() %>%
#   mutate(simulatedFrequencySample = list(rpois(iterations, frequencyModel$estimate[1])),
#          simulatedSeveritiesSample = list(rlnorm(, meanlog = severityModel$estimate[1], sdlog = severityModel$estimate[2])))


