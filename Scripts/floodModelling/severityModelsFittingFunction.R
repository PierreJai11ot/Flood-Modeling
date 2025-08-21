# Sourcing a relevant function 

source(paste0(scriptDirectory, "/Scripts/clustering/clusteringTools.R"))

# clusterNormalizationDataFetcher <- function(cluster, normalizationData, year = 2020, isISO = FALSE) { cluster is list
  

# Defining a function to fit frequency models for a group of country

severityModelsFit <- function(severityModelsDataSet, usedColumnName, severityModelsList, label, countryList, totalSeverity = FALSE) {
  
  editedDataSet <- severityModelsDataSet
  
  if(length(countryList) > 1) {
    # Re Normalization 
    
    editedDataSet <- editedDataSet %>%
      rowwise() %>%
      mutate(reNormalizationData = list(clusterNormalizationDataFetcher(list(countryList), severityNormalizationDataHANZE, year = Year, isISO = FALSE))) %>%
      mutate(countryArea = reNormalizationData$clusterArea, 
             areaWeighedAverageVulnerability = reNormalizationData$clusterAreaWeighedAverageVulnerability,
             totalVulnerability = reNormalizationData$clusterTotalTotalVulnerability
             ) %>%
      dplyr::select(- reNormalizationData) %>%
      mutate(normalizedLossAreaWeighedAverageVulnerability = `Losses (2020 euro)`/ areaWeighedAverageVulnerability ,
             normalizedLossTotalVulnerability = `Losses (2020 euro)`/totalVulnerability)
  }
  
  usedColumnSym <- sym(usedColumnName)
  
  editedDataSet <- editedDataSet %>% 
    rename(normalizedLoss = usedColumnSym) %>% 
    filter(Country %in% countryList)
  
  if(totalSeverity){
    editedDataSet <- editedDataSet %>% 
      group_by(Year) %>% 
      summarise(totalLoss = sum(normalizedLoss)) %>% 
      rename(normalizedLoss = totalLoss)
  }
  
  if(nrow(editedDataSet) > 0 ){
    
    # currentData <- drop_units(editedDataSet$normalizedLoss)
    currentData <- editedDataSet$normalizedLoss
    
    # Fitting and evaluating the Log Normal model
    
    logNormalFit <- fitdist(currentData, "lnorm")
    logNormaGoodnessOfFit <- gofstat(logNormalFit, fitnames = "Log Normal")
    
    # Fitting and evaluating the Weibull model
    
    if(setequal(countryList, c("Ireland", "Spain"))) {
      weibullFit <- fitdist(currentData[-106], "weibull")
    }
    if( ! setequal(countryList, c("Ireland", "Spain"))) {
      weibullFit <- fitdist(currentData, "weibull")
    }
    
    weibullGoodnessOfFit <- gofstat(weibullFit, fitnames = "Weibull")
    
    # Fitting and evaluating the Pareto and truncated Pareto models
    
    truncatedParetoFitterResult <- truncatedParetoFitter(label, currentData, newtonRaphsonIterations = 30, pLevel = 0.0005, minimal_kStar = 3, usingAdmissibleAlpha = TRUE, minimalAdmissibleAlpha = 10^-9, truncationTestRejectionLevel = 0.05, allowingInfiniteT = FALSE)

    # Register the fitted models
    
    severityModelsList <- add_row(severityModelsList, Country = label, 
                                  LogNormal = list(list(fitdist = logNormalFit, goodnessOfFits = logNormaGoodnessOfFit)),
                                  Weibull = list(list(fitdist = weibullFit, goodnessOfFits = weibullGoodnessOfFit)),
                                  Pareto = list(truncatedParetoFitterResult$pareto), 
                                  TruncatedPareto = list(truncatedParetoFitterResult$truncatedPareto), 
                                  Plots = list(truncatedParetoFitterResult$plots))
    
    return(severityModelsList)
  }
  
  else {
    return(severityModelsList)
  }
}
