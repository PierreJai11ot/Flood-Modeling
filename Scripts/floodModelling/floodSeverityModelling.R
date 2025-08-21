# Flood severity modelling 

# Implementing additional relevant continuous distributions 

source(paste0(scriptDirectory, "/Scripts/floodModelling/paretoDistributions.R"))


# Initializing the structure hosting severity models

severityModels <- tibble(
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
  severityModels <- severityModelsFit(dataHANZE, retainedLossMetric, severityModels, currentCountry, c(currentCountry))
}


# Fitting a severity model for all considered countries

severityModels <- severityModelsFit(dataHANZE, retainedLossMetric, severityModels, "All Considered Countries", relevantCountries$Country)


# Storing the fitted models

save(severityModels, file = file.path(paste0(resultsDirectory, "/severityModelsFit.RData")))


#  Generating plots relevant for the graphical analysis of the severity models

# Introducing a function to generate the relevant plots to evaluate the quality of the severity models

source(paste0(scriptDirectory, "/Scripts/floodModelling/severityModelsPlotsGenerator.R"))


# Generating the evaluation plots

severityModels <- severityModels %>% 
  rowwise() %>% 
  mutate(plotList = list(severityModelPlots(Country, list(LogNormal), list(Weibull), list(Pareto), list(TruncatedPareto), list(Plots), zeroAsMinObs = TRUE)))


# Splitting the plots

batch1 <- severityModels$plotList[1:12]

batch2 <- severityModels$plotList[13:24]


# Combining and naming the combined plots

combinedSeverityPlotList <- list()

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
