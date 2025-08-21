# Sourcing the custom zero inflated distributions

source(paste0(scriptDirectory, "/Scripts/floodModelling/zeroInflatedDistributions.R"))


# Introducing a function corresponding to Pearson's Chi Squared test for our custom distributions

pearsonChiSquaredTest <- function (sample, distributionName, estimatedParametersList, minNumberObservations = 5) {
  
  estimatedParameters <- unlist(estimatedParametersList)
  
  n <- length(sample)

  o <- tibble(category = sort(unique(sample))) %>%
    rowwise() %>% 
    mutate(count = sum(sample == category)) %>% 
    ungroup()
  
  indices <- c()
  countCheck <- 0 
    
  for (i in 1:nrow(o)) {
    countCheck <- countCheck + o$count[i]
    if(countCheck > minNumberObservations) {
      indices <- c(indices, i)
      countCheck <- 0 
    }
  }
  
  indices <- sort(unique(c(indices, nrow(o))))
  
  newCategories <- c()
  newCount <- c()
  
  for (i in 1:length(indices)) {
    index <- indices[i]
    newCategories <- c(newCategories, o$category[index])
    if(i == 1) {
      newCount <- c(newCount, sum(o$count[1:index]))
    }
    if(i > 1) {
      previousIndex <- indices[i-1]
      newCount <- c(newCount, sum(o$count[(previousIndex + 1):index]))
    }
  }
  
  o <- tibble(category = newCategories, count = newCount)
  
  if(distributionName == "Poisson"){
    probability <- ppois(o$category, estimatedParameters)
  }
  
  if(distributionName == "NegativeBinomial"){
    probability <- pnbinom(o$category, size = estimatedParameters[1], mu = estimatedParameters[2])
  }
  
  if(distributionName == "ZeroInflatedPoisson"){
    probability <- pzipois(o$category, lambda = estimatedParameters[1], pi = estimatedParameters[2])
  }
  
  if(distributionName == "ZeroInflatedNegativeBinomial"){
    probability <- pzinbinom(o$category,  size = estimatedParameters[1], prob = estimatedParameters[2], pi = estimatedParameters[3])
  }

  probability <- c(probability, 0) - c(0, probability)
  
  probability <- probability[-length(probability)] 
  
  probability[length(probability)] <- 1 - sum(probability[-length(probability)])
  
  o <- o %>% mutate(probability = probability) %>% 
    rowwise() %>% 
    mutate(theoreticalCount = n*probability) %>% 
    mutate(factor = (count - theoreticalCount)^2/theoreticalCount)
  
  chiSquared <- sum(o$factor)
  
  degreesOfFreedom <- (nrow(o) - length(estimatedParameters) - 1)
  
  if (degreesOfFreedom < 1) {
    pValue <- NA
  }
  
  if (degreesOfFreedom > 0) {
    pValue <- pchisq(chiSquared, df = degreesOfFreedom, lower.tail = FALSE)
  }

  return(list(chiSquared = chiSquared, pValue = pValue))
}




