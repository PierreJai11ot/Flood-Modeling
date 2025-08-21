
testSample <- rlnorm(50, 1.3, 1.2)

fit1 <- fitdist(testSample, distr = "lnorm")

fit2 <- fitdist(testSample, distr = "lnorm")

sample1 <- rlnorm(5000000, fit1$estimate[[1]], fit1$estimate[[2]])

sample2 <- rlnorm(5000000, fit2$estimate[[1]], fit2$estimate[[2]])

valueAtRisk(sample1)

valueAtRisk(sample2)

qlnorm()


meanEstimator <- function(parameters, distributionName) {
  
  if (distributionName == "LogNormal") {
    meanlog <- parameters[1]
    sdlog <- parameters[2]
    
    mean <- e^(meanlog - sdlog)
  }
  
  if ()
  
}








clusterSeverityModels <- clusterSeverityModels %>% 
  rowwise() %>%
  mutate(simulatedSample = ifelse(
    (clusterLength == 1) && (
      (totalSeveritySimulationData %>%
         filter(Country == unlist(cluster)) %>%
         pull(selectedModel.y)) == "LogNormal"
    ), 
    list((totalSeveritySimulationData %>%
            filter(Country == unlist(cluster)) %>% 
            pull(simulation))[[1]]$totalSeverity),
    list(rlnorm(iterations, meanlog = model$estimate[1], sdlog = model$estimate[2])))
  )
