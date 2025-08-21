# Introducing a function to generate the relevant plots to evaluate the quality of the frequency models

frequencyModelPlots <- function(countryName, poisson, zipoisson, negativeBinomial, zinegativeBinomial, zeroAsMinObs) {
  
  # Retrieving the sample
  
  originalData <- tibble(
    observedSample = poisson[[1]][[1]]$data
  )
  
  
  # Evaluating the number of observations
  
  numberOfObservations <- nrow(originalData)
  
  
  # Evaluating the minimal and maximal observations
  
  minObs <- min(originalData$observedSample)
  if (zeroAsMinObs) {
    minObs <- 0
  }
  maxObs <- max(originalData$observedSample) 
  
  
  # Generate the array of observable points 
  
  points <- minObs:maxObs
  
  
  # Transforming the sample to produce the empirical probability mass function
  
  empiricalData <- originalData %>% group_by(observedSample) %>% 
    summarise(observationCount = dplyr::n()) %>% 
    rowwise() %>% 
    mutate(empiricalPMF = observationCount / numberOfObservations) %>% 
    ungroup() %>% 
    mutate(empiricalCDF = cumsum(empiricalPMF)) 
  
  
  # Generating the theoretical probability mass and cumulative distribution functions of the fitted models
  
  theoreticalData <- tibble(
    n = points,
    poissonPMF = dpois(points, poisson[[1]][[1]]$estimate),
    zipoissonPMF = dzipois(points, zipoisson[[1]][[1]]$fit$estimate[1], zipoisson[[1]][[1]]$fit$estimate[2]),
    negativeBinomialPMF = dnbinom(points, size = negativeBinomial[[1]][[1]]$estimate[1], mu = negativeBinomial[[1]][[1]]$estimate[2]),
    zinegativeBinomialPMF = dzinbinom(points, zinegativeBinomial[[1]][[1]]$fit$estimate[1], zinegativeBinomial[[1]][[1]]$fit$estimate[2], zinegativeBinomial[[1]][[1]]$fit$estimate[3]),
    poissonCDF = ppois(points, poisson[[1]][[1]]$estimate),
    zipoissonCDF = pzipois(points, zipoisson[[1]][[1]]$fit$estimate[1], zipoisson[[1]][[1]]$fit$estimate[2]),
    negativeBinomialCDF = pnbinom(points, size = negativeBinomial[[1]][[1]]$estimate[1], mu = negativeBinomial[[1]][[1]]$estimate[2]),
    zinegativeBinomialCDF = pzinbinom(points, zinegativeBinomial[[1]][[1]]$fit$estimate[1], zinegativeBinomial[[1]][[1]]$fit$estimate[2], zinegativeBinomial[[1]][[1]]$fit$estimate[3])
  )
  
  
  # Generating the plot comparison of the empirical and theoretical probability mass functions 
  
  pmfPlot <- ggplot(empiricalData, aes(x = observedSample, y = empiricalPMF)) +
    geom_col(color="grey", fill="lightgrey", alpha=0.5) +
    geom_smooth(color="black", se=FALSE, size = 0.5, linetype = 1) +
    geom_line(data = theoreticalData, aes(x = n, y = poissonPMF, color="Poisson", linetype="Poisson")) +
    geom_line(data = theoreticalData, aes(x = n, y = zipoissonPMF, color="Zero Inflated Poisson", linetype="Zero Inflated Poisson")) +
    geom_line(data = theoreticalData, aes(x = n, y = negativeBinomialPMF, color="Negative Binomial", linetype="Negative Binomial")) + 
    geom_line(data = theoreticalData, aes(x = n, y = zinegativeBinomialPMF, color="Zero Inflated Negative Binomial", linetype="Zero Inflated Negative Binomial")) +
    labs(
      title = countryName, 
      x = "Yearly floods",
      y = "PMF"
    ) +
    scale_color_manual(name = "Distribution", values = c("Poisson" = "blue", "Zero Inflated Poisson" = "green", "Negative Binomial" = "red", "Zero Inflated Negative Binomial" = "purple")) +  
    scale_linetype_manual(name = "Distribution", values = c("Poisson" = "longdash", "Zero Inflated Poisson" = "dashed", "Negative Binomial" = "dotted", "Zero Inflated Negative Binomial" = "dotdash")) + 
    theme_minimal() +
    theme(
      aspect.ratio = 1,
      plot.title = element_text(hjust = 0.5, face = "bold", size = 10),
      plot.subtitle = element_text(hjust = 0.5),
      plot.caption = element_text(hjust = 0.5),
      legend.position = "bottom",
      axis.title = element_text(size = 8), 
      legend.title = element_text(size = 8),
      legend.text = element_text(size = 8)
    )
  
  
  # Generating the plot comparison of the empirical and theoretical cumulative distribution functions 

  cdfPlot <- ggplot(empiricalData, aes(x = observedSample, y = empiricalCDF)) +
    geom_step(color="black", fill = "grey80", alpha = 0.6, color = NA) +
    geom_step(data = theoreticalData, aes(x = n, y = poissonCDF, color="Poisson", linetype="Poisson")) +
    geom_step(data = theoreticalData, aes(x = n, y = zipoissonCDF, color="Zero Inflated Poisson", linetype="Zero Inflated Poisson")) +
    geom_step(data = theoreticalData, aes(x = n, y = negativeBinomialCDF, color="Negative Binomial", linetype="Negative Binomial")) +
    geom_step(data = theoreticalData, aes(x = n, y = zinegativeBinomialCDF, color="Zero Inflated Negative Binomial", linetype="Zero Inflated Negative Binomial")) +
    labs(
      title = countryName, 
      x = "Yearly floods",
      y = "CDF"
    ) +
    scale_color_manual(name = "Distribution", values = c("Poisson" = "blue", "Zero Inflated Poisson" = "green", "Negative Binomial" = "red", "Zero Inflated Negative Binomial" = "purple")) +  
    scale_linetype_manual(name = "Distribution", values = c("Poisson" = "longdash", "Zero Inflated Poisson" = "dashed", "Negative Binomial" = "dotted", "Zero Inflated Negative Binomial" = "dotdash")) +
    theme_minimal() + 
    theme(
      aspect.ratio = 1,
      plot.title = element_text(hjust = 0.5, face ="bold", size = 10),
      plot.subtitle = element_text(hjust = 0.5),
      plot.caption = element_text(hjust = 0.5),
      legend.position = "bottom",
      axis.title = element_text(size = 8), 
      legend.title = element_text(size = 8),
      legend.text = element_text(size = 8)
    )
  
  plotList <- list(pmf = pmfPlot, cdf = cdfPlot)
  
  return(plotList)
}
