# Implementing functions for the Pareto and truncated Pareto distributions

# Introducing functions relevant to the fitting of a truncated Pareto distributions

source(paste0(scriptDirectory, "/Scripts/floodModelling/paretoDistributions/paretoGeneralStatistics.R"))


# Introducing functions relevant to the assessment of the fit of Pareto and truncated Pareto distributions

source(paste0(scriptDirectory, "/Scripts/floodModelling/paretoDistributions/paretoPlots.R"))


# Combining the functions sourced above to fit a truncated Pareto distribution

# A function proceeding with all the successive steps necessary for the fitting of a truncated Pareto distribution

truncatedParetoFitter <- function(plotTitle, randomSample, newtonRaphsonIterations, pLevel = 0.0005, minimal_kStar = 10, usingAdmissibleAlpha = TRUE, minimalAdmissibleAlpha = 10^-9, truncationTestRejectionLevel = 0.05, allowingInfiniteT = FALSE) {
  
  n <- length(randomSample)
  maxTau <- min(randomSample, na.rm = TRUE)
  minT <- max(randomSample, na.rm = TRUE)
  
  statisticsRkn <- c()
  statisticsHkn <- c()
  estimatorsAlpha <- c()
  admissibleEstimatorsAlpha <- c()
  estimatorsTau <- c()
  admissibleEstimatorsTau <- c()
  estimatorsD <- c()
  admissibleEstimatorsD <- c()
  estimatorsQ <- c()
  estimatorsQzero <- c()
  estimatorsT <- c()
  statisticsE <- c()
  truncationTestsAqLevel <- c()
  truncationTestsApValue <- c()
  truncationTestsBqLevel <- c()
  truncationTestsBpValue <- c()
  
  print(paste0("Computing the estimators for k from 1 to ", (n-1)))
  progress <- txtProgressBar(min = 1, max = (n-1), style = 3)
  
  for (k in 1:(n-1)) {
    
    R <- statisticR(randomSample, k)
    statisticsRkn <- c(statisticsRkn, R)
    
    H <- statisticHill(randomSample, k)
    statisticsHkn <- c(statisticsHkn, H)
    
    truncationTestA <- truncationTestA(randomSample, k, R, H) 
    truncationTestsAqLevel <- c(truncationTestsAqLevel, truncationTestA[[1]])
    truncationTestsApValue <- c(truncationTestsApValue, truncationTestA[[2]])
    
    truncationTestB <-  truncationTestB(randomSample, k, H)
    truncationTestsBqLevel <- c(truncationTestsBqLevel, truncationTestB[[1]])
    truncationTestsBpValue <- c(truncationTestsBpValue, truncationTestB[[2]])
    
    alpha <- estimatorAlpha(newtonRaphsonIterations, H, R)
    estimatorsAlpha <- c(estimatorsAlpha, alpha)
    
    admissibleAlpha <- max(alpha, minimalAdmissibleAlpha)
    admissibleEstimatorsAlpha <- c(admissibleEstimatorsAlpha, admissibleAlpha)
    
    if (usingAdmissibleAlpha) {
      alpha <- admissibleAlpha
    }
    
    tau <- estimatorTau(randomSample, k, alpha)
    estimatorsTau <- c(estimatorsTau, tau)
    
    admissibleEstimatorsTau <- c(admissibleEstimatorsTau,  min(tau, maxTau, na.rm = TRUE))
    
    D <- estimatorD (randomSample, k, alpha, R) 
    estimatorsD <- c(estimatorsD, D)
    
    admissibleD <- max(D, 0)
    admissibleEstimatorsD <- c(admissibleEstimatorsD, admissibleD)
    
    Q <- estimatorQ(randomSample, k, alpha, admissibleD, 0.01, isRough = TRUE)
    estimatorsQ <- c(estimatorsQ, Q)
    
    Qzero <- estimatorQ(randomSample, k, alpha, admissibleD, pLevel, isRough = TRUE)
    estimatorsQzero <- c(estimatorsQzero, Qzero)
    
    estimatorT <- estimatorT(Qzero, minT, allowingInfiniteT) 
    estimatorsT <- c(estimatorsT, estimatorT)
    
    E <- statisticE(randomSample, k, alpha)
    statisticsE <- c(statisticsE, E)
    
    setTxtProgressBar(progress, k)
    
  }
  
  estimationData <- tibble(
    k = 1:(n-1), 
    Rkn = statisticsRkn, 
    Hkn = statisticsHkn, 
    Ekn = statisticsE, 
    alpha = estimatorsAlpha,
    admissibleAlpha = admissibleEstimatorsAlpha, 
    tau = estimatorsTau, 
    admissibleTau = admissibleEstimatorsTau, 
    D = estimatorsD, 
    admissibleD = admissibleEstimatorsD, 
    Q = estimatorsQ, 
    Qzero = estimatorsQzero, 
    estimatorT = estimatorsT, 
    testAqLevel = truncationTestsAqLevel,
    testApValue = truncationTestsApValue,
    testBqLevel = truncationTestsBqLevel,
    testBpValue = truncationTestsBpValue
  )
  
  kStarSelectionResult <- selector_kStar(randomSample, estimationData, minimal_kStar)
  
  kStar <- kStarSelectionResult$kStar
  estimationData$cor <- unlist(kStarSelectionResult$cor)
  
  paretoQQPlot <- paretoQQPlot(plotTitle, randomSample)
  
  truncatedParetoQQPlot <- truncatedParetoQQPlot(plotTitle, randomSample, estimatorsD[kStar])
  
  dataAlphaVSInverseHillPlotData <- data.frame(
    k = 1:(n-1),
    alpha = admissibleEstimatorsAlpha, 
    inverseHill = 1/statisticsHkn
  )
  
  dataAlphaVSInverseHillPlot <- dataAlphaVSInverseHillPlot(plotTitle, dataAlphaVSInverseHillPlotData, kStar)
  
  estimatorDPlot <- estimatorDPlot(plotTitle, estimationData, kStar) 
  
  estimatorQPlot <- estimatorQPlot(plotTitle, estimationData, kStar, max(randomSample), min(randomSample)) 
  
  estimatorTPlot <- estimatorTPlot(plotTitle, estimationData, kStar, max(randomSample), min(randomSample))
  
  statisticalTestsPlot <- statisticalTestsPlot(plotTitle, estimationData, kStar)
  
  plotsList <- list(paretoQQ = paretoQQPlot, 
                    truncatedParetoQQ = truncatedParetoQQPlot, 
                    estimatorAlphaVSInverseHill = dataAlphaVSInverseHillPlot, 
                    estimatorD = estimatorDPlot, 
                    estimatorQ = estimatorQPlot, 
                    estimatorT = estimatorTPlot, 
                    statisticalTest = statisticalTestsPlot)
  
  estimates <- c(lower = estimationData$admissibleTau[kStar], upper = estimationData$estimatorT[kStar], shape = estimationData$admissibleAlpha[kStar])
  
  stdErrors <- c(mean = NA, sd = NA)
  
  likelihood <- 1
  
  for (observation in randomSample) {
    likelihood <- likelihood * dtruncpareto(observation, estimates["lower"], estimates["upper"], estimates["shape"])
  }
  
  logLikelihood <- log(likelihood)
  
  fitdistObject <- list(
    estimate = estimates,
    sd = stdErrors,
    loglik = logLikelihood
  )
  
  goodnessOfFitStatistics <- list("Kolmogorov-Smirnov statistic" = ks.test(randomSample, "ptruncpareto", lower = estimates["lower"], upper = estimates["upper"], shape = estimates["shape"]),
                                  "Cramer-von Mises statistic" = cvm.test(randomSample, "ptruncpareto", lower = estimates["lower"], upper = estimates["upper"], shape = estimates["shape"]), 
                                  "Anderson-Darling statistic" = ad.test(randomSample, "ptruncpareto", lower = estimates["lower"], upper = estimates["upper"], shape = estimates["shape"])
  )
  
  goodnessOfFitCriteria <- list("Akaike's Information Criterion" = (-2*logLikelihood + 6),
                                "Bayesian Information Criterion" = (-2*logLikelihood + 3*log(length(randomSample)))
  )
  
  gof <- list("Statistics" = goodnessOfFitStatistics, "Criteria" = goodnessOfFitCriteria)
  
  truncatedParetoFit <- list(fitdist = fitdistObject, goodnessOfFits = gof)
  
  estimates <- c(scale = estimationData$admissibleTau[kStar], shape = (1/estimationData$Hkn[kStar]))
  
  stdErrors <- c(mean = NA, sd = NA)
  
  likelihood <- 1
  
  for (observation in randomSample) {
    likelihood <- likelihood * dpareto(observation, estimates["scale"], estimates["shape"])
  }
  
  logLikelihood <- log(likelihood)
  
  fitdistObject <- list(
    estimate = estimates,
    sd = stdErrors,
    loglik = logLikelihood
  )
  
  goodnessOfFitStatistics <- list("Kolmogorov-Smirnov statistic" = ks.test(randomSample, "ppareto", scale = estimates["scale"], shape = estimates["shape"]),
                                  "Cramer-von Mises statistic" = cvm.test(randomSample, "ppareto", scale = estimates["scale"], shape = estimates["shape"]),
                                  "Anderson-Darling statistic" = ad.test(randomSample, "ppareto", scale = estimates["scale"], shape = estimates["shape"])
  )
  
  goodnessOfFitCriteria <- list("Akaike's Information Criterion" = (-2*logLikelihood + 6),
                                "Bayesian Information Criterion" = (-2*logLikelihood + 3*log(length(randomSample)))
  )
  
  gof <- list("Statistics" = goodnessOfFitStatistics, "Criteria" = goodnessOfFitCriteria)
  
  paretoFit <- list(fitdist = fitdistObject, goodnessOfFits = gof)
  
  # result <- list(fitdist = fitdistObject, goodnessOfFits = gof, plots = plotsList)
  
  result <- list(pareto = paretoFit, truncatedPareto = truncatedParetoFit, plots = plotsList)

  return(result)
}



