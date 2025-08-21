# Selection of group 1 - Selecting countries for the first method of clustering

# Defining the cutoff point for the minimal number of entries necessary to the stability the chosen goodness of fit metric

goodnessOfFitMinEntries <- 30 


# Defining the cutoff point for the chosen goodness of fit metric, here the Anderson-Darling statistic

goodnessOfFitCutoff <- 0.75


# Identifying countries with a sufficient number of entries to consider the chosen goodness of fit metric

group1CandidateCountries <- (dataHANZE %>% 
                               group_by(Country, Year) %>% 
                               summarise(
                                 totalYearlyLoss = sum(!!as.symbol(retainedLossMetric), na.rm = TRUE),
                                 .groups = "drop"
                               ) %>% 
                               ungroup() %>%
                               group_by(Country) %>% 
                               summarise(entries = dplyr::n()) %>% 
                               filter(entries >= goodnessOfFitMinEntries))$Country


# Retrieving the chosen goodness of fit metric for the candidate countries and filtering accordingly 
# In this case, we use the Anderson Darling statistic for its sensitivity to tail behavior

group1Countries <- (totalSeverityModels %>% 
                      filter(Country %in% group1CandidateCountries) %>%
                      dplyr::select(Country, selectedModel.y, Model.y) %>% 
                      rowwise() %>% 
                      mutate(goodnessOfFit = ifelse(selectedModel.y %in% c("LogNormal", "Weibull"), 
                                                    Model.y$goodnessOfFits$ad, 
                                                    Model.y$goodnessOfFits$Statistics$`Anderson-Darling statistic`[[1]])) %>% 
                      filter(goodnessOfFit <= goodnessOfFitCutoff))$Country


rm(group1CandidateCountries, goodnessOfFitCutoff, goodnessOfFitMinEntries)


# Introducing a function to generate all possible clusters spanning countries in group 1, for a given number of clusters

clusterListGenerator <- function(countryList, numberOfClusters, minClusterSize = 1) {
  
  values <- 1:numberOfClusters
  input_list <- setNames(rep(list(values), length(countryList)), countryList)
  
  indexCombinations <- as_tibble(expand_grid(!!!input_list)) %>%
    filter(.data[[countryList[1]]] == 1) %>%
    filter(apply(., 1, function(row) all(values %in% row))) %>%
    filter(apply(., 1, function(row) {
      sapply(1:(numberOfClusters - 1), function(i) {
        pos_i   <- match(i, row)
        pos_ip1 <- match(i + 1, row)
        is.na(pos_ip1) || (!is.na(pos_i) && pos_i < pos_ip1)
      }) %>% all()
    }))  %>%
    rowwise() %>%
    mutate(minAppearances = min(sapply(values, function(value) sum(c_across(everything()) == value)))) %>%
    ungroup() %>%
    filter(minAppearances >= minClusterSize) %>%
    dplyr::select(-minAppearances)
  
  return(indexCombinations)
}


# Generating all decomposition of the group 1 countries in to 2 and 3 clusters

clusterListSize2 <- clusterListGenerator(group1Countries, 2)

clusterListSize3 <- clusterListGenerator(group1Countries, 3)

clusterList <- bind_rows(clusterListSize2, clusterListSize3)

rm(clusterListSize2, clusterListSize3)


# Reformatting the possible clusters data to extract a list of potential clusters without redundancy

clusterDecompostion <- clusterList %>%
  mutate(clusteringID = row_number()) %>%
  pivot_longer(-clusteringID, names_to = "Country", values_to = "Cluster")  %>%
  group_by(clusteringID, Cluster) %>%
  summarise(Countries = list(sort(Country)), .groups = "drop") %>%
  mutate(Cluster = paste0("Cluster", Cluster)) %>%
  pivot_wider(
    names_from = Cluster,
    values_from = Countries,
    values_fill = list(Countries = list(character(0)))
  ) %>%
  distinct()


# Extracting the list of potential clusters without redundancy

possibleClusters <- unique(lapply(c(clusterDecompostion$Cluster1, clusterDecompostion$Cluster2, clusterDecompostion$Cluster3), sort))


# Generating the clustered loss data 

clusterData <- clusterDataGenerator(dataHANZE, severityNormalizationDataHANZE, possibleClusters, commonYearsOnly = FALSE, isISO = FALSE)


# Retaining the retained loss metric

clusterData <- clusterData %>% rename(loss = !!as.symbol(retainedLossMetric))



# SANITY CHECKS, it works so why :( )
# test <- clusterData %>% 
#   rowwise() %>%
#   mutate(clusterLength = length(cluster))  %>%
#   filter(clusterLength == 3)  %>%
#   filter(clusterID == 7)
# 
# unique(test$cluster)
# 
# 
# 
# test2 <- dataHANZE %>%
#   filter(Country %in% c("France", "Germany", "Italy")) %>%
#   group_by(Year) %>%
#   summarise(lossEuro = sum(`Losses (2020 euro)`)) %>%
#   rename(Year2 = Year) %>%
#   rowwise() %>%
#   mutate(totalSeverityFetch = sum((
#     severityNormalizationDataHANZE %>%
#       filter(Country %in% c("France", "Germany", "Italy")) %>%
#       filter(Year == Year2)
#     )$totalVulnerability)
#     ) %>%
#   mutate(normLoss = lossEuro / totalSeverityFetch )





# Initializing the structure to store the severity models fitted to the potential clusters

clusterSeverityModels <- tibble(
  ID = 1:length(possibleClusters), 
  cluster = possibleClusters
)


# Extracting the loss data of each cluster from the cluster data and the normalization data necessary to the conversion of simulated samples back to 2020 Euros 

clusterSeverityModels <- clusterSeverityModels %>% 
  rowwise() %>% 
  mutate(clusterLength = length(unlist(cluster))) %>% 
  mutate(sample = list(filter(clusterData, clusterID == ID)$loss)) %>% 
  mutate(sampleLenth = length(unlist(sample))) %>% 
  mutate(clusterNormalizationData = list(clusterNormalizationDataFetcher(cluster, severityNormalizationDataHANZE, 2020, isISO = FALSE))) 
  
  
# Fitting the severity models to the retained loss metric 

clusterSeverityModels <- clusterSeverityModels %>%
  filter(sampleLenth > 1) %>%
  mutate(model = list(fitdist(data = unlist(sample), distr = "lnorm")))



# Setting the number of iterations for the Monte Carlo simulation estimations

# iterations <- 10000


# Proceeding with the Monte Carlo simulation

snapshotName <- paste0(resultsDirectory, "/snapPost_debug1.RData")
save.image(file = snapshotName)
# load(snapshotName)
print("ping 1 ")

clusterSeverityModels <- clusterSeverityModels %>% 
  rowwise() %>%
  mutate(simulatedSample = ifelse(
    (clusterLength == 1) && (
      (totalSeveritySimulationData %>%
        filter(Country == unlist(cluster)) %>%
        pull(selectedModel.y)) == "LogNormal"
    ), 
    list((totalSeveritySimulationData %>%
       # filter(Country %in% unlist(cluster)) %>% 
         filter(Country == unlist(cluster)) %>%
       pull(simulation))[[1]]$totalSeverity),
    list(rlnorm(iterations, meanlog = model$estimate[1], sdlog = model$estimate[2])))
  )

print("pong 1 ")

# print("BALISEBUG")

# Converting the simulated sample into 2020 Euros

conversionIndex <- ifelse(retainedLossMetric == "normalizedLossTotalVulnerability", 3, 2)

clusterSeverityModels <- clusterSeverityModels %>% 
  rowwise() %>%
  mutate(conversionFactor = unlist(clusterNormalizationData)[conversionIndex]) %>%
  mutate(simulatedSample2020Euros = list(unlist(simulatedSample) * conversionFactor) ) 

rm(conversionIndex)


# Estimating our risk assessment metrics 

clusterSeverityModels <- clusterSeverityModels %>% 
  rowwise() %>%
  mutate(VaRSev = valueAtRisk(unlist(simulatedSample)), 
         ESSev = expectedShortfall(unlist(simulatedSample)), 
         valueAtRisk = valueAtRisk(unlist(simulatedSample2020Euros)), 
         expectedShortfall = expectedShortfall(unlist(simulatedSample2020Euros)))


# Aggregating the expected shortfall and value at risks for the different partitions of the candidate countries 

partitionsRiskAssessmentData <- clusterDecompostion %>%
  rowwise() %>%
  mutate(
    valueAtRisk1 = if (!is.empty.list(Cluster1)) {
      idx <- which(sapply(clusterSeverityModels$cluster, function(x) identical(sort(unlist(x)), sort(unlist(Cluster1)))))
      if (length(idx) > 0) clusterSeverityModels$valueAtRisk[idx] else 0
    } else {
      0
    },
    valueAtRisk2 = if (!is.empty.list(Cluster2)) {
      idx <- which(sapply(clusterSeverityModels$cluster, function(x) identical(sort(unlist(x)), sort(unlist(Cluster2)))))
      if (length(idx) > 0) clusterSeverityModels$valueAtRisk[idx] else 0
    } else {
      0
    },
    valueAtRisk3 = if (!is.empty.list(Cluster3)) {
      idx <- which(sapply(clusterSeverityModels$cluster, function(x) identical(sort(unlist(x)), sort(unlist(Cluster3)))))
      if (length(idx) > 0) clusterSeverityModels$valueAtRisk[idx] else 0
    } else {
      0
    },
    expectedShortfall1 = if (!is.empty.list(Cluster1)) {
      idx <- which(sapply(clusterSeverityModels$cluster, function(x) identical(sort(unlist(x)), sort(unlist(Cluster1)))))
      if (length(idx) > 0) clusterSeverityModels$expectedShortfall[idx] else 0
    } else {
      0
    },
    expectedShortfall2 = if (!is.empty.list(Cluster2)) {
      idx <- which(sapply(clusterSeverityModels$cluster, function(x) identical(sort(unlist(x)), sort(unlist(Cluster2)))))
      if (length(idx) > 0) clusterSeverityModels$expectedShortfall[idx] else 0
    } else {
      0
    },
    expectedShortfall3 = if (!is.empty.list(Cluster3)) {
      idx <- which(sapply(clusterSeverityModels$cluster, function(x) identical(sort(unlist(x)), sort(unlist(Cluster3)))))
      if (length(idx) > 0) clusterSeverityModels$expectedShortfall[idx] else 0
    } else {
      0
    }
  ) %>%
  mutate(
    totalValueAtRisk = valueAtRisk1 + valueAtRisk2 + valueAtRisk3,
    totalExpectedShortfall = expectedShortfall1 + expectedShortfall2 + expectedShortfall3
  ) %>%
  mutate(clusters = list(list(Cluster1, Cluster2, Cluster3))) %>%
  ungroup()


# Splitting the risk assessment data between partitions into 2 clusters and 3 clusters

partitionsRiskAssessmentData2Clusters <- partitionsRiskAssessmentData %>%
  rowwise() %>%
  filter(length(unlist(Cluster3)) == 0)

partitionsRiskAssessmentData3Clusters <- partitionsRiskAssessmentData %>%
  rowwise() %>%
  filter(length(unlist(Cluster3)) != 0)


# Extracting the cluster decomposition resulting in the models with lowest aggregate value at risk, for partitions into 2 and 3 clusters 

minVaRIndex2Clusters <- which_min(partitionsRiskAssessmentData2Clusters$totalValueAtRisk)

minVaR2Clusters <- partitionsRiskAssessmentData2Clusters$clusters[minVaRIndex2Clusters]

minVaRIndex3Clusters <- which_min(partitionsRiskAssessmentData3Clusters$totalValueAtRisk)

minVaR3Clusters <- partitionsRiskAssessmentData3Clusters$clusters[minVaRIndex3Clusters]


# Extracting the cluster decomposition resulting in the models with lowest aggregate expected shortall, for partitions into 2 and 3 clusters 

minESIndex2Clusters <- which_min(partitionsRiskAssessmentData2Clusters$totalExpectedShortfall)

minES2Clusters <- partitionsRiskAssessmentData2Clusters$clusters[minESIndex2Clusters]

minESIndex3Clusters <- which_min(partitionsRiskAssessmentData3Clusters$totalExpectedShortfall)

minES3Clusters <- partitionsRiskAssessmentData3Clusters$clusters[minESIndex3Clusters]


# Introducing a function to generate the appropriate summary tables

group1TableGenerator <- function(clusteringData, individualData, clusters) {
  # TEST
  # clusters <- minES3Clusters
  # clusteringData <- clusterSeverityModels
  # individualData <- totalSeveritySimulationData
  
  retainedClusterSeverityModels <- clusteringData %>%
    filter( list(cluster) %in% clusters[[1]])
  
  retainedClusterSeverityModels$ID <- 1:nrow(retainedClusterSeverityModels)
  
  tableData <- clusterTableDataGenerator(individualData, retainedClusterSeverityModels, F)
  
  return(tableData)
}

writeLines(clusterTableLaTeXGenerator(group1TableGenerator(clusterSeverityModels, totalSeveritySimulationData, minVaR2Clusters), 
                                      "Risk Measures for the 2 Clustering Minimizing the VaR.txt"), 
           file.path(paste0(scriptDirectory, "/Texts"), 
                     "Risk Measures for the 2 Clustering Minimizing the VaR.txt"
                     ))

writeLines(clusterTableLaTeXGenerator(group1TableGenerator(clusterSeverityModels, totalSeveritySimulationData, minVaR3Clusters), 
                                      "Risk Measures for the 3 Clustering Minimizing the VaR.txt"), 
           file.path(paste0(scriptDirectory, "/Texts"), 
                     "Risk Measures for the 3 Clustering Minimizing the VaR.txt"
           ))

writeLines(clusterTableLaTeXGenerator(group1TableGenerator(clusterSeverityModels, totalSeveritySimulationData, minES2Clusters), 
                                      "Risk Measures for the 2 Clustering Minimizing the ES.txt"), 
           file.path(paste0(scriptDirectory, "/Texts"), 
                     "Risk Measures for the 2 Clustering Minimizing the ES.txt"
           ))

writeLines(clusterTableLaTeXGenerator(group1TableGenerator(clusterSeverityModels, totalSeveritySimulationData, minES3Clusters), 
                                      "Risk Measures for the 3 Clustering Minimizing the ES.txt"), 
           file.path(paste0(scriptDirectory, "/Texts"), 
                     "Risk Measures for the 3 Clustering Minimizing the ES.txt"
           ))


# OLD
# retainedClusterSeverityModels <- clusterSeverityModels %>%
#   filter( list(cluster) %in% minES3Clusters[[1]])
# retainedClusterSeverityModels$ID <- 1:nrow(retainedClusterSeverityModels)
# tableData <- clusterTableDataGenerator(totalSeveritySimulationData, retainedClusterSeverityModels, F)
# test2 <- clusterTableLaTeXGenerator(tableData, "title")



# EXTENSION 

clusterDecompostionV2 <- clusterDecompostion

clusterDecompostionV2 <- clusterDecompostionV2 %>% 
  rowwise() %>%
  mutate(l1 = length(Cluster1), 
         l2 = length(Cluster2),
         l3 = length(Cluster3))

clusterDecompostionV2 <- clusterDecompostionV2 %>% 
  filter(min(l1,l2,l3) > 1) 

possibleClustersV2 <- clusterDecompostionV2 

possibleClustersV2 <- unique(lapply(c(possibleClustersV2$Cluster1, possibleClustersV2$Cluster2, possibleClustersV2$Cluster3), sort))

# Generating the clustered loss data 

clusterDataV2 <- clusterDataGenerator(dataHANZE, severityNormalizationDataHANZE, possibleClustersV2, commonYearsOnly = FALSE, isISO = FALSE)

# Retaining the retained loss metric

clusterDataV2 <- clusterDataV2 %>% rename(loss = !!as.symbol(retainedLossMetric))

# BALISE NEW
# Flood severity modelling 

# Implementing additional relevant continuous distributions 

source(paste0(scriptDirectory, "/Scripts/floodModelling/paretoDistributions.R"))
source(paste0(scriptDirectory, "/Scripts/floodModelling/severityModelsFittingFunction.R"))
source(paste0(scriptDirectory, "/Scripts/floodModelling/severityModelsPlotsGenerator.R"))

# Initializing the structure hosting severity models

group1ClusterTotalSeverityModelsV2 <- tibble(
  Country = character(),       
  LogNormal = list(),
  Weibull = list(),
  Pareto = list(), 
  TruncatedPareto = list(), 
  Plots = list())

clusterDataV2 <- clusterDataV2 %>%
  rename(Country = clusterID)

# Fitting the frequency models for each country

for (currentID in sort(unique(clusterDataV2$Country))) {
  
  currentCluster <- unlist(unique(
    clusterDataV2 %>% 
      filter(Country == currentID) %>% 
      pull(cluster)))
  
  # group2ClusterTotalSeverityModels <- severityModelsFit(clusterData, "loss", group2ClusterTotalSeverityModels, paste0("Cluster ", currentID), c(currentID))
  group1ClusterTotalSeverityModelsV2 <- severityModelsFit(dataHANZE, retainedLossMetric, group1ClusterTotalSeverityModelsV2, paste0("Cluster ", currentID), currentCluster,  totalSeverity = TRUE)
  
}

#  Generating plots relevant for the graphical analysis of the severity models

group1ClusterTotalSeverityModelsV2 <- group1ClusterTotalSeverityModelsV2 %>% 
  rowwise() %>% 
  mutate(plotList = list(severityModelPlots(Country, list(LogNormal), list(Weibull), list(Pareto), list(TruncatedPareto), list(Plots), zeroAsMinObs = TRUE)))


# Splitting the plots

batch1 <- group1ClusterTotalSeverityModelsV2$plotList[1:12]

batch2 <- group1ClusterTotalSeverityModelsV2$plotList[13:15]

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
  
  ggsave(file.path(paste0(plotDirectory, "/", combinedPlotsInstuctions$title[i], " - Group 1 cluster - Part 1.pdf")), width = 21, height = 29.7, units = "cm", plot = plotBatch1)
  
  combinedClusterTotalSeverityPlotList <- c(combinedClusterTotalSeverityPlotList, list(list(plotBatch1)))
  
  plotBatch2 <- plotArrayCombiner (batch2, combinedPlotsInstuctions$tag[i], combinedPlotsInstuctions$title[i], numberOfColumns, TRUE)
  
  ggsave(file.path(paste0(plotDirectory, "/", combinedPlotsInstuctions$title[i], " - Group 1 cluster - Part 2.pdf")), width = 21, height = 9.9, units = "cm", plot = plotBatch2)
  
  combinedClusterTotalSeverityPlotList <- c(combinedClusterTotalSeverityPlotList, list(list(plotBatch2)))
}


# "LogNormal", "TruncatedPareto", "Weibull", "Pareto",
group1ClusterTotalSeverityModelV2Selection <- c(
  `Cluster 1` = "LogNormal", 
  `Cluster 2` = "Weibull", 
  `Cluster 3` = "Weibull", 
  `Cluster 4` = "Weibull", 
  `Cluster 5` = "Weibull", 
  `Cluster 6` = "Weibull", 
  `Cluster 7` = "Weibull", 
  `Cluster 8` = "Weibull", 
  `Cluster 9` = "LogNormal", 
  `Cluster 10` = "LogNormal", 
  `Cluster 11` = "LogNormal",
  `Cluster 12` = "Weibull",
  `Cluster 13` = "LogNormal", 
  `Cluster 14` = "LogNormal", 
  `Cluster 15` = "LogNormal"
)


# Applying the selection

group1ClusterTotalSeverityModelsV2 <- group1ClusterTotalSeverityModelsV2 %>%
  mutate(Country = as.character(Country)) %>%
  mutate(selectedModel = group1ClusterTotalSeverityModelV2Selection[Country]) %>%
  pivot_longer(cols = c(LogNormal, Weibull, Pareto, TruncatedPareto), names_to = "ModelType", values_to = "Model") %>%
  filter(ModelType == selectedModel) %>%
  dplyr::select(Country, selectedModel, Model)  %>%
  rename(selectedModel.y = selectedModel, 
         Model.y = Model)

group1ClusterTotalSeverityModelsV2 <- group1ClusterTotalSeverityModelsV2 %>%
  mutate(selectedModel.x = "placeholder", 
         Model.x = list(c()))

# iterations <- 10000

snapshotName <- paste0(resultsDirectory, "/snapPost_debug2.RData")
# save.image(file = snapshotName)
load(snapshotName)
print("ping 2 ")

group1ClusterTotalSeverityModelsV2 <- group1ClusterTotalSeverityModelsV2 %>% 
  mutate(clusterID = as.integer(str_remove(Country, "Cluster "))) %>%
  mutate(cluster = (unique(
    clusterDataV2 %>%
      filter(Country == clusterID) %>%
      pull(cluster)
  )))

group1ClusterTotalSeverityModelsV2Data <- group1ClusterTotalSeverityModelsV2 %>%
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

print("pong 2 ")

snapshotName <- paste0(resultsDirectory, "/group1ReSimuPostFix.RData")
# save.image(file = snapshotName)
load(snapshotName)
# rm(snapshotName)

# 
# group1ClusterTotalSeverityModelsV2Data <- group1ClusterTotalSeverityModelsV2Data %>%
#   rename(Country = label) %>%
#   rename(clusterID = ID)

group1ClusterTotalSeverityModelsV2Data <- group1ClusterTotalSeverityModelsV2Data %>%
  rename(label = Country) %>%
  rowwise() %>%
  mutate(clusterID = as.integer(str_remove(label, "Cluster "))) %>%
  mutate(cluster = (unique(
    clusterDataV2 %>%
      filter(Country == clusterID) %>%
      pull(cluster)
  )))

conversionIndex <- ifelse(retainedLossMetric == "normalizedLossTotalVulnerability", 3, 2)

group1ClusterTotalSeverityModelsV2Data <- group1ClusterTotalSeverityModelsV2Data %>% 
  rowwise() %>%
  mutate(clusterNormalizationData = 
           list(clusterNormalizationDataFetcher(cluster, severityNormalizationDataHANZE, 2020, isISO = F))) %>%
  mutate(simulatedSample2020Euros = 
           list(unlist(simulation$totalSeverity) * unlist(clusterNormalizationData)[conversionIndex])) 

rm(conversionIndex)

group1ClusterTotalSeverityModelsV2Data <- group1ClusterTotalSeverityModelsV2Data %>% 
  rowwise() %>%
  mutate(VaRSev = valueAtRisk(unlist(simulation$totalSeverity)), 
         ESSev = expectedShortfall(unlist(simulation$totalSeverity)), 
         valueAtRisk = valueAtRisk(unlist(simulatedSample2020Euros)), 
         expectedShortfall = expectedShortfall(unlist(simulatedSample2020Euros))) %>%
  rename(ID = clusterID)

# 
# 
# 


writeLines(clusterTableLaTeXGenerator(clusterTableDataGenerator(totalSeveritySimulationData, group1ClusterTotalSeverityModelsV2Data, F), 
                                      "Risk Measures for Risk Measure Minimization 3 Clustering V2"),
           file.path(paste0(scriptDirectory, "/Texts"), "Risk Measures for Risk Measure Minimization 3 Clustering V2.txt"))




frequencyDistributionNamesConversion <- tibble(
  name = c("Poisson", "NegativeBinomial", "ZeroInflatedPoisson", "ZeroInflatedNegativeBinomial"), 
  nickname = c("PO", "NB", "ZP", "ZN")
)

severityDistributionNamesConversion <- tibble(
  name = c("LogNormal", "TruncatedPareto", "Weibull", "Pareto"), 
  nickname = c("LN", "TP", "WE", "PA")
)

# 
# 
# 


tableData <- tibble(
  clusterID = group1ClusterTotalSeverityModelsV2Data$ID
)

clusterSeverityTableData <- tableData %>%
  rowwise() %>%
  mutate(
    SeverityDistribution = filter(group1ClusterTotalSeverityModelsV2Data, ID == clusterID)$selectedModel.y[1],
    SeverityDistribution = filter(severityDistributionNamesConversion, name == SeverityDistribution)$nickname[1],
    
    SeveritySimulation = list(filter(group1ClusterTotalSeverityModelsV2Data, ID == clusterID)$simulation[[1]]$totalSeverity),
    SeverityEmpirical = list(filter(clusterDataV2, Country == clusterID)$loss),
    
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
                '$\\underset{\\text{Sim.}}{\\text{CV}}$',
                '$\\underset{\\text{Emp.}}{\\text{Med.}}$',
                '$\\underset{\\text{Sim.}}{\\text{Med.}}$',
                '$\\underset{\\text{Emp.}}{\\text{Skew}}$',
                '$\\underset{\\text{Sim.}}{\\text{Skew}}$',
                '$\\underset{\\text{Emp.}}{\\text{Kurt}}$',
                '$\\underset{\\text{Sim.}}{\\text{Kurt}}$')

caption1 <- "Comparison of empirical and simulated total flood severity distributions by cluster - V2 Strat1"
clusterSeverityTable1 <- kable(clusterSeverityTableDataBatch1, format = "latex", booktabs = TRUE, caption = caption1, escape = FALSE)
clusterSeverityTable1 <- tableCleaner(clusterSeverityTable1)

writeLines(clusterSeverityTable1,
           file.path(paste0(scriptDirectory, "/Texts"), 
                     "Comparison of empirical and simulated total flood severity distributions by cluster - V2 Strat1.txt"))


# 
# 
#  NEEDS NEW SELECTION BALISE 


# Aggregating the expected shortfall and value at risks for the different partitions of the candidate countries 

partitionsRiskAssessmentDataV2 <- clusterDecompostionV2 %>%
  rowwise() %>%
  mutate(
    idx1 = which(sapply(group1ClusterTotalSeverityModelsV2Data$cluster, function(x) identical(sort(unlist(x)), sort(unlist(Cluster1))))),
    valueAtRisk1 = if (!is.empty.list(Cluster1)) {
      idx <- which(sapply(group1ClusterTotalSeverityModelsV2Data$cluster, function(x) identical(sort(unlist(x)), sort(unlist(Cluster1)))))
      if (length(idx) > 0) group1ClusterTotalSeverityModelsV2Data$valueAtRisk[idx] else 0
    } else {
      0
    },
    valueAtRisk2 = if (!is.empty.list(Cluster2)) {
      idx <- which(sapply(group1ClusterTotalSeverityModelsV2Data$cluster, function(x) identical(sort(unlist(x)), sort(unlist(Cluster2)))))
      if (length(idx) > 0) group1ClusterTotalSeverityModelsV2Data$valueAtRisk[idx] else 0
    } else {
      0
    },
    valueAtRisk3 = if (!is.empty.list(Cluster3)) {
      idx <- which(sapply(group1ClusterTotalSeverityModelsV2Data$cluster, function(x) identical(sort(unlist(x)), sort(unlist(Cluster3)))))
      if (length(idx) > 0) group1ClusterTotalSeverityModelsV2Data$valueAtRisk[idx] else 0
    } else {
      0
    },
    expectedShortfall1 = if (!is.empty.list(Cluster1)) {
      idx <- which(sapply(group1ClusterTotalSeverityModelsV2Data$cluster, function(x) identical(sort(unlist(x)), sort(unlist(Cluster1)))))
      if (length(idx) > 0) group1ClusterTotalSeverityModelsV2Data$expectedShortfall[idx] else 0
    } else {
      0
    },
    expectedShortfall2 = if (!is.empty.list(Cluster2)) {
      idx <- which(sapply(group1ClusterTotalSeverityModelsV2Data$cluster, function(x) identical(sort(unlist(x)), sort(unlist(Cluster2)))))
      if (length(idx) > 0) group1ClusterTotalSeverityModelsV2Data$expectedShortfall[idx] else 0
    } else {
      0
    },
    expectedShortfall3 = if (!is.empty.list(Cluster3)) {
      idx <- which(sapply(group1ClusterTotalSeverityModelsV2Data$cluster, function(x) identical(sort(unlist(x)), sort(unlist(Cluster3)))))
      if (length(idx) > 0) group1ClusterTotalSeverityModelsV2Data$expectedShortfall[idx] else 0
    } else {
      0
    }
  ) %>%
  mutate(
    totalValueAtRisk = valueAtRisk1 + valueAtRisk2 + valueAtRisk3,
    totalExpectedShortfall = expectedShortfall1 + expectedShortfall2 + expectedShortfall3
  ) %>%
  mutate(clusters = list(list(Cluster1, Cluster2, Cluster3))) %>%
  ungroup()


# Splitting the risk assessment data between partitions into 2 clusters and 3 clusters

# partitionsRiskAssessmentData2ClustersV2 <- partitionsRiskAssessmentDataV2 %>%
#   rowwise() %>%
#   filter(length(unlist(Cluster3)) == 0)

partitionsRiskAssessmentData3ClustersV2 <- partitionsRiskAssessmentDataV2 %>%
  rowwise() %>%
  filter(length(unlist(Cluster3)) != 0)


# Extracting the cluster decomposition resulting in the models with lowest aggregate value at risk, for partitions into 2 and 3 clusters 

# minVaRIndex2Clusters <- which_min(partitionsRiskAssessmentData2Clusters$totalValueAtRisk)
# 
# minVaR2Clusters <- partitionsRiskAssessmentData2Clusters$clusters[minVaRIndex2Clusters]

minVaRIndex3Clusters <- which_min(partitionsRiskAssessmentData3ClustersV2$totalValueAtRisk)

minVaR3Clusters <- partitionsRiskAssessmentData3ClustersV2$clusters[minVaRIndex3Clusters]


# Extracting the cluster decomposition resulting in the models with lowest aggregate expected shortall, for partitions into 2 and 3 clusters 

# minESIndex2Clusters <- which_min(partitionsRiskAssessmentData2Clusters$totalExpectedShortfall)
# 
# minES2Clusters <- partitionsRiskAssessmentData2Clusters$clusters[minESIndex2Clusters]

minESIndex3Clusters <- which_min(partitionsRiskAssessmentData3ClustersV2$totalExpectedShortfall)

minES3Clusters <- partitionsRiskAssessmentData3ClustersV2$clusters[minESIndex3Clusters]


# Introducing a function to generate the appropriate summary tables

group1TableGenerator <- function(clusteringData, individualData, clusters) {
  # TEST
  # clusters <- minES3Clusters
  # clusteringData <- clusterSeverityModels
  # individualData <- totalSeveritySimulationData
  
  retainedClusterSeverityModels <- clusteringData %>%
    filter( list(cluster) %in% clusters[[1]])
  
  retainedClusterSeverityModels$ID <- 1:nrow(retainedClusterSeverityModels)
  
  tableData <- clusterTableDataGenerator(individualData, retainedClusterSeverityModels, F)
  
  return(tableData)
}

writeLines(clusterTableLaTeXGenerator(group1TableGenerator(group1ClusterTotalSeverityModelsV2Data, totalSeveritySimulationData, minVaR3Clusters), 
                                      "Risk Measures for the 3 Clustering Minimizing the VaR v2.txt"), 
           file.path(paste0(scriptDirectory, "/Texts"), 
                     "Risk Measures for the 3 Clustering Minimizing the VaR v2.txt"
           ))

writeLines(clusterTableLaTeXGenerator(group1TableGenerator(group1ClusterTotalSeverityModelsV2Data, totalSeveritySimulationData, minES3Clusters), 
                                      "Risk Measures for the 3 Clustering Minimizing the ES v2.txt"), 
           file.path(paste0(scriptDirectory, "/Texts"), 
                     "Risk Measures for the 3 Clustering Minimizing the ES v2.txt"
           ))

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
