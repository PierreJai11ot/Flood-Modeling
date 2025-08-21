# Flood total severity modelling 

# Sourcing additional relevant continuous distributions 

source(paste0(scriptDirectory, "/Scripts/floodModelling/paretoDistributions.R"))


# Initializing the structure hosting severity models

totalSeverityModels <- tibble(
  Country = character(),       
  LogNormal = list(),
  Weibull = list(),
  Pareto = list(), 
  TruncatedPareto = list(), 
  Plots = list())


# Defining a function to fit severity models for a group of countries 

source(paste0(scriptDirectory, "/Scripts/floodModelling/severityModelsFittingFunction.R"))


# Fitting the frequency models for each country

for (currentCountry in relevantCountries$Country) {
  totalSeverityModels <- severityModelsFit(dataHANZE, retainedLossMetric, totalSeverityModels, currentCountry, c(currentCountry), totalSeverity = TRUE)
}


# Fitting a severity model for all considered countries

totalSeverityModels <- severityModelsFit(dataHANZE, retainedLossMetric, totalSeverityModels, "All Considered Countries", relevantCountries$Country, totalSeverity = TRUE)


# Storing the fitted models

save(totalSeverityModels, file = file.path(paste0(resultsDirectory, "/totalSeverityModelsFit.RData")))


#  Generating plots relevant for the graphical analysis of the severity models

# Introducing a function to generate the relevant plots to evaluate the quality of the severity models

source(paste0(scriptDirectory, "/Scripts/floodModelling/severityModelsPlotsGenerator.R"))


# Generating the evaluation plots

totalSeverityModels <- totalSeverityModels %>% 
  rowwise() %>% 
  mutate(plotList = list(severityModelPlots(Country, list(LogNormal), list(Weibull), list(Pareto), list(TruncatedPareto), list(Plots), zeroAsMinObs = TRUE, totalSeverity = TRUE)))


# Splitting the plots

batch1 <- totalSeverityModels$plotList[1:12]

batch2 <- totalSeverityModels$plotList[13:24]


# Combining and naming the combined plots

combinedSeverityPlotList <- list()

combinedPlotsInstuctions <- tibble(
  tag = c("pdf", "cdf", "cdfLog", "qq", "qqLog", "pp", "paretoQQ", "truncatedParetoQQ", "estimatorAlphaVSInverseHill", "estimatorD", "estimatorQ", "estimatorT", "statisticalTest"), 
  title = c("Empirical and theoretical PDFs of total severity models", 
            "Empirical and theoretical CDFs of total severity models", 
            "Empirical and theoretical log-scale CDFs of total severity models",
            "Q-Q Plot for total severity models", 
            "Q-Q Plot in log scale for total severity models",
            "P-P Plot for total severity models", 
            "Pareto Q-Q plot (total severity)",
            "Truncated Pareto Q-Q plot (total severity)",
            "Inverse Hill statistic and shape estimator against extreme value subset size (total severity)", 
            "Estimator of the odds ratio against the extreme value subset size (total severity)", 
            "Estimator of the extreme quantile against the extreme value subset size (total severity)", 
            "Estimator of the upper parameter T against the extreme value subset size (total severity)", 
            "Statistical truncation tests for against the extreme value subset size (total severity)"
  )
)


# Generating and storing the combined evaluation plots

numberOfColumns <- 3

for (i in 1:nrow(combinedPlotsInstuctions)) {
  plotBatch1 <- plotArrayCombiner (batch1, combinedPlotsInstuctions$tag[i], combinedPlotsInstuctions$title[i], numberOfColumns, TRUE)
  plotBatch2 <- plotArrayCombiner (batch2, combinedPlotsInstuctions$tag[i], combinedPlotsInstuctions$title[i], numberOfColumns, TRUE)
  
  ggsave(file.path(paste0(plotDirectory, "/", combinedPlotsInstuctions$title[i], " - Part 1.pdf")), width = 21, height = 29.7, units = "cm", plot = plotBatch1)
  
  ggsave(file.path(paste0(plotDirectory, "/", combinedPlotsInstuctions$title[i], " - Part 2.pdf")), width = 21, height = 29.7, units = "cm", plot = plotBatch2)
  
  combinedSeverityPlotList <- c(combinedSeverityPlotList, list(list(plotBatch1, plotBatch2)))
}


rm(alphaEstimatorComparisionPlotData, dataAlphaVSInverseHillPlot, estimatorAlpha, estimatorD, estimatorDPlot, estimatorQ, estimatorQAlternative, estimatorQPlot, estimatorT, estimatorTau, estimatorTPlot,
   newtonRaphsonFunction, newtonRaphsonFunctionDerivative, newtonRaphsonIteration, newtonRaphsonStepper, paretoQQPlot, paretoQQPlotData, selector_kStar, statisticalTestsPlot, 
   statisticE, statisticHill, statisticR, truncatedParetoFitter, truncatedParetoQQPlot, truncatedParetoQQPlotData, truncationTestA, truncationTestB)

rm(currentCountry, i, numberOfColumns)

rm(batch1, batch2, plotBatch1, plotBatch2, combinedPlotsInstuctions, combinedFrequencyCdfPlots, combinedFrequencyPmfPlots, combinedSeverityPlotList)

rm(severityModelsFit, severityModelPlots)
