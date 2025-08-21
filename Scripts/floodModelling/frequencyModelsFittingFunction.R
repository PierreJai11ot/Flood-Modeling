# Defining a function to fit frequency models for a group of countries 

frequencyModelsFit <- function(frequencyModelsDataSet, frequencyModelsList, currentGeographicLabel, countryList) {
  
  # Aggregating the number of recorded floods per year for all the countries in the country list
  
  editedDataSet <- frequencyModelsDataSet %>% 
    filter(Country %in% countryList) %>% 
    group_by(Year)  %>% 
    summarise(totalNumberOfFloods = sum(NumberOfFloods), .groups = "drop")
  
  
  if(nrow(editedDataSet) > 0 ){
    
    # Evaluating the total number of floods
    
    currentData <- editedDataSet$totalNumberOfFloods
    
    
    # Fitting and evaluating the Poisson model
    
    poissonFit <- fitdist(currentData, "pois")
    
    
    # Fitting and evaluating the Zero Inflated Poisson model
    
    zipoissonFit <- zipoisFitter(currentData)
    
    
    # Fitting and evaluating the Negative Binomial model
    
    negativeBinomialFit <- fitdist(currentData, "nbinom")
    
    
    # Fitting and evaluating the Zero Inflated Negative Binomial model
    
    zinegativeBinomialFit <- zinbinomFitter(currentData)
    
    
    # Storing the fitted models
    
    fitList <- list(poissonFit, zipoissonFit, negativeBinomialFit, zinegativeBinomialFit)
    fitLabelList <- c("Poisson", "Zero Inflated Poisson", "Negative Binomial", "Zero Inflated Negative Binomial")
    
    
    # Storing the fitted models and adding them to the initial list of fitted models
    
    frequencyModelsList <- add_row(frequencyModelsList, Country = currentGeographicLabel, 
                                   Poisson = list(list(poissonFit)), 
                                   ZeroInflatedPoisson = list(list(zipoissonFit)), 
                                   NegativeBinomial = list(list(negativeBinomialFit)),
                                   ZeroInflatedNegativeBinomial = list(list(zinegativeBinomialFit))
                                   ) 
    return(frequencyModelsList)
  }
  
  
  else {
    return(frequencyModelsList)
  }
}


