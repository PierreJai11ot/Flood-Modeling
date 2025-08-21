# Monte Carlo simulation of the selected models

# Setting seed 

set.seed(2025)


# Defining a function to run a given number of simulations for a given model 

source(paste0(scriptDirectory, "/Scripts/monteCarloSimulation/monteCarloSimulationFunction.R"))


# Running the simulations for our selected models 

simulationData <- combinedModels %>%
  rowwise() %>%
  mutate(
    simulation = list(
      monteCarloSimulation(
        frequencyModel = Model.x, 
        frequencyModelName = selectedModel.x, 
        severityModel = Model.y, 
        severityModelName = selectedModel.y, 
        iterations = iterations
      ) %>%
        mutate(SimulationIndex = row_number()) %>%
        dplyr::select(SimulationIndex, frequency, severity, totalSeverity)
    )
  ) %>%
  ungroup()

pureFrequencySimulationData <- combinedModels %>%
  rowwise() %>%
  mutate(
    simulation = list(
      monteCarloSimulation(
        frequencyModel = Model.x, 
        frequencyModelName = selectedModel.x, 
        severityModel = Model.y, 
        severityModelName = "Pure Frequency Simulation", 
        iterations = iterations
      ) %>%
        mutate(SimulationIndex = row_number()) %>%
        dplyr::select(SimulationIndex, frequency)
    )
  ) %>%
  ungroup()

pureSeveritySimulationData <- combinedModels %>%
  rowwise() %>%
  mutate(
    simulation = list(
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
  ) %>%
  ungroup()

totalSeveritySimulationData <- totalSeverityModels %>%
  rowwise() %>%
  mutate(
    simulation = list(
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
  ) %>%
  ungroup()


# Generating diagnosis plots 

empiricalYearlyLossData <- dataHANZE %>% 
  group_by(Country, Year) %>% 
  summarise(
    totalYearlyLoss = sum(!!as.symbol(retainedLossMetric), na.rm = TRUE),
    .groups = "drop"
  ) %>%
  complete(Country, Year = full_seq(startingYear:endingYear, 1), fill = list(totalYearlyLoss = 0))

allConsideredCountriesYearlyLossData <- empiricalYearlyLossData  %>% 
  rowwise() %>%
  mutate(totalYearlyLoss = totalYearlyLoss * 
           (severityNormalizationDataHANZE %>%
              rename(Country2 = Country) %>%
              rename(Year2 = Year) %>%
              filter(Country2 == Country) %>%
              filter(Year2 == Year) %>%
              pull(totalVulnerability))
         / sum(severityNormalizationDataHANZE %>%
              rename(Year2 = Year) %>%
              filter(Year2 == Year) %>%
              pull(totalVulnerability))
  ) %>%
  group_by(Year) %>% 
  summarise(totalYearlyLoss = sum(totalYearlyLoss)) %>% 
  mutate(Country = "All Considered Countries")

empiricalYearlyLossData <- bind_rows(empiricalYearlyLossData, allConsideredCountriesYearlyLossData) %>%
  arrange(Country, Year)

simulatedYearlyLossData <- simulationData %>%
  unnest(simulation)  %>%
  dplyr::select(Country, SimulationIndex, totalSeverity)

simulationPlots <- tibble(Country = c(setdiff(sort(unique(empiricalYearlyLossData$Country)), "All Considered Countries"), "All Considered Countries"))


# Defining and calling a function to generate relevant plots

source(paste0(scriptDirectory, "/Scripts/monteCarloSimulation/monteCarloSimulationPlotsGenerator.R"))

simulationPlotList <- monteCarloSimulationPlotsGenerator(simulationPlots$Country, empiricalYearlyLossData, simulatedYearlyLossData) 


# Defining and calling a function to generate box plots

source(paste0(scriptDirectory, "/Scripts/monteCarloSimulation/monteCarloBoxPlot.R"))

simulationPlotList <- monteCarloBoxPlotGenerator(simulationPlotList, dataHANZE, simulationData, pureFrequencySimulationData, pureSeveritySimulationData, totalSeveritySimulationData)
  

# Splitting the plots for page formatting

batch1 <- simulationPlotList$Plots[1:12]

batch2 <- simulationPlotList$Plots[13:24]


# Combining and naming the generated evaluation plots

numberOfColumns <- 3

combinedSimulationPlotList <- list()

combinedPlotsInstuctions <- tibble(
  tag = c("pdf", "cdf", "QQ"), 
  title = c("Empirical PDFs of the observed and simulated normalized yearly losses", 
            "Empirical CDFs of the observed and simulated normalized yearly losses", 
            "Q-Q Plot for the observed and simulated normalized yearly losses"
            ),
  columns = numberOfColumns*c(1, 1, 1)
)


# Storing the combined evaluation plots

for (i in 1:nrow(combinedPlotsInstuctions)) {
  plotBatch1 <- plotArrayCombiner (batch1, combinedPlotsInstuctions$tag[i], combinedPlotsInstuctions$title[i], combinedPlotsInstuctions$columns[i], TRUE)
  plotBatch2 <- plotArrayCombiner (batch2, combinedPlotsInstuctions$tag[i], combinedPlotsInstuctions$title[i], combinedPlotsInstuctions$columns[i], TRUE)
  
  ggsave(file.path(paste0(plotDirectory, "/", combinedPlotsInstuctions$title[i], " - Part 1.pdf")), width = 21, height = 29.7, units = "cm", plot = plotBatch1)
  ggsave(file.path(paste0(plotDirectory, "/", combinedPlotsInstuctions$title[i], " - Part 2.pdf")), width = 21, height = 29.7, units = "cm", plot = plotBatch2)
  
  combinedSimulationPlotList <- c(combinedSimulationPlotList, list(list(plotBatch1, plotBatch2)))
}


# Splitting the plots for page formatting

batch1 <- simulationPlotList$Plots[1:8]

batch2 <- simulationPlotList$Plots[9:16]

batch3 <- simulationPlotList$Plots[17:24]


# Combining and naming the generated evaluation plots

numberOfColumns <- 2

combinedPlotsInstuctions <- tibble(
  tag = c("boxPlots"), 
  title = c("Box plots for observed and simulated variables"),
  columns = numberOfColumns
)


# Storing the combined evaluation plots

for (i in 1:nrow(combinedPlotsInstuctions)) {
  plotBatch1 <- plotArrayCombiner (batch1, combinedPlotsInstuctions$tag[i], combinedPlotsInstuctions$title[i], combinedPlotsInstuctions$columns[i], TRUE)
  plotBatch2 <- plotArrayCombiner (batch2, combinedPlotsInstuctions$tag[i], combinedPlotsInstuctions$title[i], combinedPlotsInstuctions$columns[i], TRUE)
  plotBatch3 <- plotArrayCombiner (batch3, combinedPlotsInstuctions$tag[i], combinedPlotsInstuctions$title[i], combinedPlotsInstuctions$columns[i], TRUE)
  
  ggsave(file.path(paste0(plotDirectory, "/", combinedPlotsInstuctions$title[i], " - Part 1.pdf")), width = 21, height = 29.7, units = "cm", plot = plotBatch1)
  ggsave(file.path(paste0(plotDirectory, "/", combinedPlotsInstuctions$title[i], " - Part 2.pdf")), width = 21, height = 29.7, units = "cm", plot = plotBatch2)
  ggsave(file.path(paste0(plotDirectory, "/", combinedPlotsInstuctions$title[i], " - Part 3.pdf")), width = 21, height = 29.7, units = "cm", plot = plotBatch3)
  
  
  combinedSimulationPlotList <- c(combinedSimulationPlotList, list(list(plotBatch1, plotBatch2, plotBatch3)))
}


# Introducing a function to generate a table to evaluated the fitted models 

source(paste0(scriptDirectory, "/Scripts/monteCarloSimulation/evaluationTable.R"))


# Generating the evaluation tables

evalutationTables <- evaluationTableGenerator(dataHANZE, simulationData, pureFrequencySimulationData, pureSeveritySimulationData, totalSeveritySimulationData)


# Storing the evaluation tables

writeLines(evalutationTables$frequency, file.path(paste0(scriptDirectory, "/Texts"), "FrequencyModelEvaluationTable.txt"))
writeLines(evalutationTables$severity1, file.path(paste0(scriptDirectory, "/Texts"), "SeverityModelEvaluationTable1.txt"))
writeLines(evalutationTables$severity2, file.path(paste0(scriptDirectory, "/Texts"), "SeverityModelEvaluationTable2.txt"))
writeLines(evalutationTables$severity3, file.path(paste0(scriptDirectory, "/Texts"), "SeverityModelEvaluationTable3.txt"))
writeLines(evalutationTables$compound1, file.path(paste0(scriptDirectory, "/Texts"), "CompoundModelEvaluationTable1.txt"))
writeLines(evalutationTables$compound2, file.path(paste0(scriptDirectory, "/Texts"), "CompoundModelEvaluationTable2.txt"))
writeLines(evalutationTables$compound3, file.path(paste0(scriptDirectory, "/Texts"), "CompoundModelEvaluationTable3.txt"))
writeLines(evalutationTables$totalSeverity1, file.path(paste0(scriptDirectory, "/Texts"), "TotalSeverityModelEvaluationTable1.txt"))
writeLines(evalutationTables$totalSeverity2, file.path(paste0(scriptDirectory, "/Texts"), "TotalSeverityModelEvaluationTable2.txt"))
writeLines(evalutationTables$totalSeverity3, file.path(paste0(scriptDirectory, "/Texts"), "TotalSeverityModelEvaluationTable3.txt"))

rm(batch1, batch2, batch3, combinedPlotsInstuctions, combinedSimulationPlotList, plotBatch1, plotBatch2, plotBatch3, simulationPlotList, simulationPlots)

rm(i, numberOfColumns)
