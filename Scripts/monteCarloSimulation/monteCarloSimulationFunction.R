# Defining a function to run a given number of simulations for a given model 

source(paste0(scriptDirectory, "/Scripts/floodModelling/zeroInflatedDistributions.R"))

monteCarloSimulation <- function(frequencyModel, frequencyModelName, severityModel, severityModelName, iterations) {
  
  simulations <- tibble(
    frequency = integer(),
    severity = list(),
    totalSeverity = numeric()
  )
  
  frequencySimulations <- c()
  
  if(frequencyModelName == "Poisson") {
    lambda <- as.numeric(frequencyModel[[1]]$estimate[[1]])
    frequencySimulations <- rpois(iterations, lambda)
  }
  
  if(frequencyModelName == "ZeroInflatedPoisson") {
    lambda <- as.numeric(frequencyModel[[1]]$coefficients[[1]])
    pi <- as.numeric(frequencyModel[[1]]$coefficients[[2]])
    frequencySimulations <- rzipois(iterations, lambda, pi)
  }
  
  if(frequencyModelName == "NegativeBinomial") {
    size <- as.numeric(frequencyModel[[1]]$estimate[[1]])
    mu <- as.numeric(frequencyModel[[1]]$estimate[[2]])
    frequencySimulations <- rnbinom(iterations, size, mu = mu)
  }
  
  if(frequencyModelName == "ZeroInflatedNegativeBinomial") {
    size <- as.numeric(frequencyModel[[1]]$coefficients[[1]])
    prob <- as.numeric(frequencyModel[[1]]$coefficients[[2]])
    pi <- as.numeric(frequencyModel[[1]]$coefficients[[3]])
    frequencySimulations <- rzinbinom(iterations, size, prob, pi)
  }
  
  if(frequencyModelName == "Pure Severity Simulation"){
    frequencySimulations <- rep(1, iterations)
  }
  
  for (frequencySimulation in frequencySimulations) {
    
    severitySimulations <- c()
    
    severitySimulations <- numeric(0)
    
    if (!is.na(frequencySimulation) && frequencySimulation > 0) {
      
      if(frequencyModelName == "Pure Frequency Simulation"){
        frequencySimulations <- c(0)
      }
    
      if(severityModelName == "LogNormal") {
        meanlog <- as.numeric(severityModel[[1]]$estimate[[1]])
        sdlog <- as.numeric(severityModel[[1]]$estimate[[2]])
        severitySimulations <- rlnorm(frequencySimulation, meanlog, sdlog)
      }
      
      if(severityModelName == "Weibull") {
        shape <- as.numeric(severityModel[[1]]$estimate[[1]])
        scale <- as.numeric(severityModel[[1]]$estimate[[2]])
        severitySimulations <- rweibull(frequencySimulation, shape, scale)
      }
      
      if(severityModelName == "Pareto") {
        scale <- as.numeric(severityModel[[1]]$estimate[[1]])
        shape <- as.numeric(severityModel[[1]]$estimate[[2]])
        severitySimulations <- rpareto(frequencySimulation, scale = scale, shape = shape)
      }
      
      if(severityModelName == "TruncatedPareto") {
        lower <- as.numeric(severityModel[[1]]$estimate[[1]])
        upper <- as.numeric(severityModel[[1]]$estimate[[2]])
        shape <- as.numeric(severityModel[[1]]$estimate[[3]])
        severitySimulations <- rtruncpareto(frequencySimulation, lower = lower, upper = upper, shape = shape)
      }
    }
    
    simulations <- add_row( simulations,
      frequency = frequencySimulation,
      severity = list(severitySimulations),
      totalSeverity = sum(severitySimulations)
    )
  }
  
  return(simulations)
}
