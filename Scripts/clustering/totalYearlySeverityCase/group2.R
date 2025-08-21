# Selection of group 2 - Selecting countries for the second method of clustering as the countries not selected in group 1

group2Countries <- setdiff(unique(dataHANZE$Country), group1Countries)

group2Countries <- setdiff(group2Countries, c("Lithuania"))


# Examining the number of years with positive total loss per pair of countries 

countryYears <- dataHANZE %>%
  filter(Country %in% group2Countries) %>%
  dplyr::select(CountryISO, Year) %>%
  distinct() %>%
  group_by(CountryISO) %>%
  summarise(Years = list(sort(unique(Year))), .groups = "drop")

countryPairs <- expand_grid(CountryA = countryYears$CountryISO,
                             CountryB = countryYears$CountryISO)

commonYearsTable <- countryPairs %>%
  left_join(countryYears, by = c("CountryA" = "CountryISO")) %>%
  rename(YearsA = Years) %>%
  left_join(countryYears, by = c("CountryB" = "CountryISO")) %>%
  rename(YearsB = Years) %>%
  mutate(SharedYears = map2_int(YearsA, YearsB, ~ length(intersect(.x, .y)))) %>%
  dplyr::select(CountryA, CountryB, SharedYears)

commonYearsMatrix <- commonYearsTable %>%
  pivot_wider(names_from = CountryB, values_from = SharedYears)


# Reorganizing the yearly loss data to facilitate the correlation computations

# BALISE NEED TO CHECK NORMALIZATION PIPELINE
# NORMALIZE ON TWO COUNTRY SCALE ? 

correlationFormattedData <- dataHANZE %>% 
  filter(Country %in% group2Countries) %>%
  group_by(CountryISO, Year) %>% 
  summarise(
    totalYearlyLoss = sum(`Losses (2020 euro)`, na.rm = TRUE),
    # totalYearlyLoss = sum(!!as.symbol(retainedLossMetric), na.rm = TRUE),
    .groups = "drop"
  ) %>%
  complete(Year, CountryISO, fill = list(totalYearlyLoss = 0)) %>%
  pivot_wider(names_from = CountryISO, values_from = totalYearlyLoss) %>%
  arrange(Year)
  

# Generating the Pearson and Spearman rank correlation tables for our loss data

pearsonCorrelationMatrix <- correlationMatrixGenerator(correlationFormattedData, severityNormalizationDataHANZE, nonNegativeEntriesOnly = TRUE, bothNonNegative = FALSE, 
                                                       type = "pearson", dropFirstColumn = TRUE, normalizationMethod = "clusterPair")

spearmanRankCorrelationMatrix <- correlationMatrixGenerator(correlationFormattedData, severityNormalizationDataHANZE, nonNegativeEntriesOnly = TRUE, bothNonNegative = FALSE, 
                                                            type = "spearman", dropFirstColumn = TRUE, normalizationMethod = "clusterPair")



caption1 <- "Pearson correlation matrix for the national yearly aggregated severities"
pearsonCorrelationMatrixTeX <- kable(round(pearsonCorrelationMatrix, 2), format = "latex", booktabs = TRUE, caption = caption1, escape = FALSE)
# frequencyTable <- kable(frequencyTableData, format = "latex", booktabs = TRUE, escape = FALSE, caption = frequencyCaption) %>% kable_styling(latex_options = c("hold_position"))
pearsonCorrelationMatrixTeX <- tableCleaner(pearsonCorrelationMatrixTeX)
writeLines(pearsonCorrelationMatrixTeX,
           file.path(paste0(scriptDirectory, "/Texts"), 
                     "Pearson correlation matrix for the national yearly aggregated severities.txt"))

caption1 <- "Spearman's rank correlation matrix for the national yearly aggregated severities"
spearmanRankCorrelationMatrixTeX <- kable(round(spearmanRankCorrelationMatrix, 2), format = "latex", booktabs = TRUE, caption = caption1, escape = FALSE)
# frequencyTable <- kable(frequencyTableData, format = "latex", booktabs = TRUE, escape = FALSE, caption = frequencyCaption) %>% kable_styling(latex_options = c("hold_position"))
spearmanRankCorrelationMatrixTeX <- tableCleaner(spearmanRankCorrelationMatrixTeX)
writeLines(spearmanRankCorrelationMatrixTeX,
           file.path(paste0(scriptDirectory, "/Texts"), 
                     "Spearman's rank correlation matrix for the national yearly aggregated severities.txt"))


 # Generating the corresponding dissimilarity matrices 

pearsonDissimilarityMatrix <- correlationToDissimilarityConverter(pearsonCorrelationMatrix, "Half1PlusCorr")

spearmanDissimilarityMatrix <- correlationToDissimilarityConverter(spearmanRankCorrelationMatrix, "Half1PlusCorr")


# Generating the clusters through AGNES hierarchical clustering 

# Linkage methods : complete, average or single

numberOfClusters <- 3

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
dendogram <- dendogramGenerator(hierarchicalClustering, numberOfClusters, c("blue", "green", "red"), "Hierarchical Clustering", xlabel = " ", ylabel = "height")

dendogram

ggsave(file.path(paste0(plotDirectory, "/", "Dendogram - Group 2 cluster.pdf")), width = 21, height = 21, units = "cm", plot = dendogram)



# Generating the clustered loss data 

clusterData <- clusterDataGenerator(dataHANZE, severityNormalizationDataHANZE, clustersList, commonYearsOnly = FALSE, isISO = TRUE)


# Retaining the retained loss metric

clusterData <- clusterData %>% rename(loss = !!as.symbol(retainedLossMetric))
  





# BALISE NEW
# Flood severity modelling 

# Implementing additional relevant continuous distributions 

source(paste0(scriptDirectory, "/Scripts/floodModelling/paretoDistributions.R"))
source(paste0(scriptDirectory, "/Scripts/floodModelling/severityModelsFittingFunction.R"))
source(paste0(scriptDirectory, "/Scripts/floodModelling/severityModelsPlotsGenerator.R"))

# Initializing the structure hosting severity models

group2ClusterTotalSeverityModels <- tibble(
  Country = character(),       
  LogNormal = list(),
  Weibull = list(),
  Pareto = list(), 
  TruncatedPareto = list(), 
  Plots = list())

clusterData <- clusterData %>%
  rename(Country = clusterID)

# Fitting the frequency models for each country

for (currentID in sort(unique(clusterData$Country))) {
  
  currentCluster <- unlist(unique(
    clusterData %>% 
      filter(Country == currentID) %>% 
      pull(cluster)))
  
  currentCluster <- sort(unique(
    relevantCountries %>% 
      filter(CountryISO %in% currentCluster) %>%
      pull(Country)))
  
  # group2ClusterTotalSeverityModels <- severityModelsFit(clusterData, "loss", group2ClusterTotalSeverityModels, paste0("Cluster ", currentID), c(currentID))
  group2ClusterTotalSeverityModels <- severityModelsFit(dataHANZE, retainedLossMetric, group2ClusterTotalSeverityModels, paste0("Cluster ", currentID), currentCluster,  totalSeverity = TRUE)
                                                        
}


#  Generating plots relevant for the graphical analysis of the severity models

group2ClusterTotalSeverityModels <- group2ClusterTotalSeverityModels %>% 
  rowwise() %>% 
  mutate(plotList = list(severityModelPlots(Country, list(LogNormal), list(Weibull), list(Pareto), list(TruncatedPareto), list(Plots), zeroAsMinObs = TRUE)))


# Splitting the plots

batch1 <- group2ClusterTotalSeverityModels$plotList[1:3]


# Combining and naming the combined plots

combinedClusterTotalSeverityPlotList <- list()

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
  plotBatch1 <- plotArrayCombiner (batch1, combinedPlotsInstuctions$tag[i], combinedPlotsInstuctions$title[i], numberOfColumns, TRUE)

  ggsave(file.path(paste0(plotDirectory, "/", combinedPlotsInstuctions$title[i], " - Group 2 cluster.pdf")), width = 21, height = 9.9, units = "cm", plot = plotBatch1)

  combinedClusterTotalSeverityPlotList <- c(combinedClusterTotalSeverityPlotList, list(list(plotBatch1)))
}





# "LogNormal", "TruncatedPareto", "Weibull", "Pareto",
group2ClusterTotalSeverityModelSelection <- c(
  `Cluster 1` = "LogNormal", 
  `Cluster 2` = "LogNormal", 
  `Cluster 3` = "Weibull"
)


# Applying the selection

group2ClusterTotalSeverityModels <- group2ClusterTotalSeverityModels %>%
  mutate(Country = as.character(Country)) %>%
  mutate(selectedModel = group2ClusterTotalSeverityModelSelection[Country]) %>%
  pivot_longer(cols = c(LogNormal, Weibull, Pareto, TruncatedPareto), names_to = "ModelType", values_to = "Model") %>%
  filter(ModelType == selectedModel) %>%
  dplyr::select(Country, selectedModel, Model)  %>%
  rename(selectedModel.y = selectedModel, 
         Model.y = Model)

group2ClusterTotalSeverityModels <- group2ClusterTotalSeverityModels %>%
  mutate(selectedModel.x = "placeholder", 
         Model.x = list(c()))

# iterations <- 10000

group2ClusterTotalSeverityModels <- group2ClusterTotalSeverityModels %>% 
  rowwise() %>%
  mutate(clusterID = as.integer(str_remove(Country, "Cluster "))) %>%
  mutate(cluster = (unique(
    clusterData %>%
      filter(Country == clusterID) %>%
      pull(cluster)
  ))) %>%
  mutate(cluster = list(sort(unique(filter(relevantCountries, CountryISO %in% cluster)$Country))))

group2ClusterTotalSeverityModels2Data <- group2ClusterTotalSeverityModels %>%
  rowwise() %>%
  mutate(
    clusterLength = length(cluster),
    simulation = ifelse(
      (clusterLength == 1),
      list((totalSeveritySimulationData %>%
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
          mutate(SimulationIndex = row_number()) %>%
          dplyr::select(SimulationIndex, totalSeverity)
      )
    )
    ) %>%
  ungroup()

group2ClusterTotalSeverityModels2Data <- group2ClusterTotalSeverityModels2Data %>%
  rename(label = Country) %>%
  rowwise() %>%
  mutate(clusterID = as.integer(str_remove(label, "Cluster "))) %>%
  mutate(cluster = (unique(
    clusterData %>%
      filter(Country == clusterID) %>%
      pull(cluster)
    )))

conversionIndex <- ifelse(retainedLossMetric == "normalizedLossTotalVulnerability", 3, 2)

group2ClusterTotalSeverityModels2Data <- group2ClusterTotalSeverityModels2Data %>% 
  rowwise() %>%
  mutate(clusterNormalizationData = 
           list(clusterNormalizationDataFetcher(cluster, severityNormalizationDataHANZE, 2020, isISO = TRUE))) %>%
  mutate(simulatedSample2020Euros = 
           list(unlist(simulation$totalSeverity) * unlist(clusterNormalizationData)[conversionIndex])) 

rm(conversionIndex)

group2ClusterTotalSeverityModels2Data <- group2ClusterTotalSeverityModels2Data %>% 
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

writeLines(clusterTableLaTeXGenerator(clusterTableDataGenerator(totalSeveritySimulationData, group2ClusterTotalSeverityModels2Data, T), 
                                      "Risk Measures for the correlation based 3 Clustering"),
           file.path(paste0(scriptDirectory, "/Texts"), "Risk Measures for the correlation based 3 Clustering.txt"))




frequencyDistributionNamesConversion <- tibble(
  name = c("Poisson", "NegativeBinomial", "ZeroInflatedPoisson", "ZeroInflatedNegativeBinomial"), 
  nickname = c("PO", "NB", "ZP", "ZN")
)

severityDistributionNamesConversion <- tibble(
  name = c("LogNormal", "TruncatedPareto", "Weibull", "Pareto"), 
  nickname = c("LN", "TP", "WE", "PA")
)

tableData <- tibble(
  clusterID = group2ClusterTotalSeverityModels2Data$ID
)
  
clusterSeverityTableData <- tableData %>%
  rowwise() %>%
  mutate(
    SeverityDistribution = filter(group2ClusterTotalSeverityModels2Data, ID == clusterID)$selectedModel.y[1],
    SeverityDistribution = filter(severityDistributionNamesConversion, name == SeverityDistribution)$nickname[1],
    
    SeveritySimulation = list(filter(group2ClusterTotalSeverityModels2Data, ID == clusterID)$simulation[[1]]$totalSeverity),
    SeverityEmpirical = list(filter(clusterData, Country == clusterID)$loss),
    
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

clusterSeverityTableDataBatch1 <- clusterSeverityTableData %>% 
  dplyr::select('Cluster', 
                'Dist.', 
                '$\\underset{\\text{Emp.}}{\\text{Mean}}$', 
                '$\\underset{\\text{Sim.}}{\\text{Mean}}$',
                '$\\underset{\\text{Emp.}}{\\sigma}$',
                '$\\underset{\\text{Sim.}}{\\sigma}$',
                '$\\underset{\\text{Emp.}}{\\text{CV}}$',
                '$\\underset{\\text{Sim.}}{\\text{CV}}$')

caption1 <- "Comparison of empirical and simulated total flood severity distributions by cluster - Part 1"
clusterSeverityTable1 <- kable(clusterSeverityTableDataBatch1, format = "latex", booktabs = TRUE, caption = caption1, escape = FALSE)
clusterSeverityTable1 <- tableCleaner(clusterSeverityTable1)

writeLines(clusterSeverityTable1,
           file.path(paste0(scriptDirectory, "/Texts"), 
                     "Comparison of empirical and simulated total flood severity distributions by cluster - Part 1.txt"))

clusterSeverityTableDataBatch2 <- clusterSeverityTableData %>% 
  dplyr::select('Cluster',
                'Dist.',
                '$\\underset{\\text{Emp.}}{\\text{Med.}}$',
                '$\\underset{\\text{Sim.}}{\\text{Med.}}$',
                '$\\underset{\\text{Emp.}}{\\text{Skew}}$',
                '$\\underset{\\text{Sim.}}{\\text{Skew}}$',
                '$\\underset{\\text{Emp.}}{\\text{Kurt}}$',
                '$\\underset{\\text{Sim.}}{\\text{Kurt}}$')

caption2 <- "Comparison of empirical and simulated total flood severity distributions by cluster - Part 2"
clusterSeverityTable2 <- kable(clusterSeverityTableDataBatch2, format = "latex", booktabs = TRUE, caption = caption2, escape = FALSE)
clusterSeverityTable2 <- tableCleaner(clusterSeverityTable2)

writeLines(clusterSeverityTable2,
           file.path(paste0(scriptDirectory, "/Texts"), 
                     "Comparison of empirical and simulated total flood severity distributions by cluster - Part 2.txt"))


clusterSeverityTableDataBatch2 <- clusterSeverityTableData %>% 
  dplyr::select('Cluster',
                'Dist.',
                '$\\underset{\\text{Emp.}}{\\text{Mean}}$', 
                '$\\underset{\\text{Sim.}}{\\text{Mean}}$',
                '$\\underset{\\text{Emp.}}{\\sigma}$',
                '$\\underset{\\text{Sim.}}{\\sigma}$',
                '$\\underset{\\text{Emp.}}{\\text{CV}}$',
                '$\\underset{\\text{Sim.}}{\\text{CV}}$',
                '$\\underset{\\text{Emp.}}{\\text{Med.}}$',
                '$\\underset{\\text{Sim.}}{\\text{Med.}}$',
                '$\\underset{\\text{Emp.}}{\\text{Skew}}$',
                '$\\underset{\\text{Sim.}}{\\text{Skew}}$',
                '$\\underset{\\text{Emp.}}{\\text{Kurt}}$',
                '$\\underset{\\text{Sim.}}{\\text{Kurt}}$')

caption2 <- "Comparison of empirical and simulated total flood severity distributions by cluster"
clusterSeverityTable2 <- kable(clusterSeverityTableDataBatch2, format = "latex", booktabs = TRUE, caption = caption2, escape = FALSE)
clusterSeverityTable2 <- tableCleaner(clusterSeverityTable2)

writeLines(clusterSeverityTable2,
           file.path(paste0(scriptDirectory, "/Texts"), 
                     "Comparison of empirical and simulated total flood severity distributions by cluster - COMPLETE.txt"))

# 
# clusterSeverityModels <- tibble(
#   ID = 1:length(clustersList), 
#   cluster = clustersList
# )
# 
# clusterSeverityModels <- clusterSeverityModels %>% 
#   rowwise() %>% 
#   mutate(clusterLength = length(unlist(cluster))) %>% 
#   mutate(sample = list(filter(clusterData, clusterID == ID)$loss)) %>% 
#   mutate(sampleLenth = length(unlist(sample))) %>% 
#   mutate(clusterNormalizationData = list(
#     clusterNormalizationDataFetcher(cluster, severityNormalizationDataHANZE, 2020, isISO = TRUE)
#   )) 

# Initializing the structure to store the severity models fitted to the potential clusters




# Extracting the loss data of each cluster from the cluster data and the normalization data necessary to the conversion of simulated samples back to 2020 Euros 

# BALISE MOST REPRESENTED MODEL ? 



# Fitting the severity models to the retained loss metric 

# # BALISE REplacE MODEL IF NECEsSARY 
# clusterSeverityModels <- clusterSeverityModels %>%
#   filter(sampleLenth > 1) %>%
#   mutate(model = list(fitdist(data = unlist(sample), distr = "lnorm")))


# Setting the number of iterations for the Monte Carlo simulation estimations


# Proceeding with the Monte Carlo simulation

# MODIFY MODEL IF NECESSARY 
# clusterSeverityModels <- clusterSeverityModels %>% 
#   rowwise() %>%
#   mutate(simulatedSample = list(rlnorm(iterations, meanlog = model$estimate[1], sdlog = model$estimate[2])))


# Converting the simulated sample into 2020 Euros



# Estimating our risk assessment metrics 
