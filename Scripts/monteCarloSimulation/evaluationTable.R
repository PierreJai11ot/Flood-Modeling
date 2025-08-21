# Introducing a function to generate a table to evaluated the fitted models 

evaluationTableGenerator <- function(empiricalData, combinedSimluationData, frequencySimulationData, severitySimulationData, totalSeveritySimulationData) {
  
  empiricalFrequencyData <- empiricalData %>% 
    group_by(Country) %>% 
    count(Year, sort = TRUE) %>%
    complete(Year = full_seq(startingYear:endingYear, 1), fill = list(n=0)) %>% rename(NumberOfFloods = n)
  
  allCountries <- empiricalFrequencyData %>% 
    group_by(Year) %>% 
    summarise(NumberOfFloods = sum(NumberOfFloods)) %>% 
    mutate(Country = "All Considered Countries")
  
  empiricalFrequencyData <- bind_rows(empiricalFrequencyData, allCountries)
    
  usedColumnSym <- sym(retainedLossMetric)
  
  empiricalSeverityData <- empiricalData %>% 
    rename(normalizedLoss = usedColumnSym) %>% 
    dplyr::select(Country, Year, normalizedLoss)
  
  allCountries <- empiricalSeverityData %>% 
    rowwise() %>% 
    mutate(normalizedLoss = normalizedLoss * (
      severityNormalizationDataHANZE %>%
        rename(Country2 = Country) %>%
        rename(Year2 = Year) %>%
        filter(Country2 == Country) %>%
        filter(Year2 == Year) %>%
        pull(totalVulnerability)
    ) / (
      sum(severityNormalizationDataHANZE %>%
            rename(Year2 = Year) %>%
            filter(Year2 == Year) %>%
            pull(totalVulnerability))
    )
    ) %>%
    dplyr::select(Year, normalizedLoss) %>% 
    mutate(Country = "All Considered Countries")
  
  empiricalSeverityData <- bind_rows(empiricalSeverityData, allCountries)
  
  empiricalCompoundData <- empiricalData %>% 
    rename(normalizedLoss = usedColumnSym) %>% 
    group_by(Country, Year) %>%
    summarise(totalYearlyLoss = sum(normalizedLoss)) %>%
    complete(Year = full_seq(startingYear:endingYear, 1), fill = list(totalYearlyLoss=0))
  
  allCountries <- empiricalCompoundData %>% 
    rowwise() %>% 
    mutate(totalYearlyLoss = totalYearlyLoss * (
      severityNormalizationDataHANZE %>%
        rename(Country2 = Country) %>%
        rename(Year2 = Year) %>%
        filter(Country2 == Country) %>%
        filter(Year2 == Year) %>%
        pull(totalVulnerability)
    ) / (
      sum(severityNormalizationDataHANZE %>%
            rename(Year2 = Year) %>%
            filter(Year2 == Year) %>%
            pull(totalVulnerability))
    )
    ) %>%
    group_by(Year) %>% 
    summarise(totalYearlyLoss = sum(totalYearlyLoss)) %>% 
    mutate(Country = "All Considered Countries")
  
  empiricalCompoundData <- bind_rows(empiricalCompoundData, allCountries)
  
  empiricalTotalSeverityData <- empiricalData %>% 
    rename(normalizedLoss = usedColumnSym) %>% 
    dplyr::select(Country, Year, normalizedLoss) %>% 
    group_by(Country, Year) %>%
    summarise(totalLoss = sum(normalizedLoss)) %>%
    ungroup() %>%
    rename(normalizedLoss = totalLoss)
  
  allCountries <- empiricalTotalSeverityData %>% 
    rowwise() %>% 
    mutate(normalizedLoss = normalizedLoss * (
      severityNormalizationDataHANZE %>%
        rename(Country2 = Country) %>%
        rename(Year2 = Year) %>%
        filter(Country2 == Country) %>%
        filter(Year2 == Year) %>%
        pull(totalVulnerability)
    ) / (
      sum(severityNormalizationDataHANZE %>%
            rename(Year2 = Year) %>%
            filter(Year2 == Year) %>%
            pull(totalVulnerability))
    )
    ) %>%
    dplyr::select(Year, normalizedLoss) %>% 
    group_by(Year) %>%
    summarise(totalLoss = sum(normalizedLoss)) %>%
    ungroup() %>%
    rename(normalizedLoss = totalLoss) %>% 
    mutate(Country = "All Considered Countries")
  
  empiricalTotalSeverityData <- bind_rows(empiricalTotalSeverityData, allCountries)
  
  tableData <- tibble(
    CountryName = combinedSimluationData$Country
  )
  
  frequencyDistributionNamesConversion <- tibble(
    name = c("Poisson", "NegativeBinomial", "ZeroInflatedPoisson", "ZeroInflatedNegativeBinomial"), 
    nickname = c("PO", "NB", "ZP", "ZN")
  )
  
  severityDistributionNamesConversion <- tibble(
    name = c("LogNormal", "TruncatedPareto", "Weibull", "Pareto"), 
    nickname = c("LN", "TP", "WE", "PA")
  )
  
  frequencyTableData <- tableData %>%
    rowwise() %>%
    mutate(ISO = ifelse(CountryName == "All Considered Countries", "AC", filter(relevantCountries, Country == CountryName)$CountryISO[1]),
           FrequencyDistribution = filter(combinedSimluationData, Country == CountryName)$selectedModel.x[1]
           ) %>%
    mutate(
      FrequencyDistribution = filter(frequencyDistributionNamesConversion, name == FrequencyDistribution)$nickname[1],
      
      FrequencyEmpirical = list(filter(empiricalFrequencyData, Country == CountryName)$NumberOfFloods),
      FrequencySimulation = list(filter(frequencySimulationData, Country == CountryName)$simulation[[1]]$frequency), 
           
      meanFrequencyEmpirical = round(mean(FrequencyEmpirical), 2),
      meanFrequencySimulated = round(mean(FrequencySimulation), 2),
           
      standardDeviationFrequencyEmpirical = round(sd(FrequencyEmpirical), 2),
      standardDeviationFrequencySimulated = round(sd(FrequencySimulation), 2),
           
      coeffVariationFrequencyEmpirical = round(standardDeviationFrequencyEmpirical/meanFrequencyEmpirical, 2),
      coeffVariationFrequencySimulated = round(standardDeviationFrequencySimulated/meanFrequencySimulated, 2),
           
      medianFrequencyEmpirical = round(median(FrequencyEmpirical), 2),
      medianFrequencySimulated = round(median(FrequencySimulation), 2)
      ) %>%
    ungroup() %>%
    dplyr::select(-FrequencyEmpirical, -FrequencySimulation) %>%
    dplyr::select(-CountryName)%>%
    rename('Country' = ISO, 
           'Dist.' = FrequencyDistribution, 
           '$\\underset{\\text{Emp.}}{\\text{Mean}}$' = meanFrequencyEmpirical, 
           '$\\underset{\\text{Sim.}}{\\text{Mean}}$' = meanFrequencySimulated, 
           '$\\underset{\\text{Emp.}}{\\sigma}$' = standardDeviationFrequencyEmpirical,
           '$\\underset{\\text{Sim.}}{\\sigma}$' = standardDeviationFrequencySimulated, 
           '$\\underset{\\text{Emp.}}{\\text{CV}}$' = coeffVariationFrequencyEmpirical,
           '$\\underset{\\text{Sim.}}{\\text{CV}}$' = coeffVariationFrequencySimulated,
           '$\\underset{\\text{Emp.}}{\\text{Med.}}$' = medianFrequencyEmpirical,
           '$\\underset{\\text{Sim.}}{\\text{Med.}}$' = medianFrequencySimulated
           )
  
  
  severityTableData <- tableData %>%
    rowwise() %>%
    mutate(ISO = ifelse(CountryName == "All Considered Countries", "AC", filter(relevantCountries, Country == CountryName)$CountryISO[1])) %>%
    mutate(
      SeverityDistribution = filter(combinedSimluationData, Country == CountryName)$selectedModel.y[1],
      SeverityDistribution = filter(severityDistributionNamesConversion, name == SeverityDistribution)$nickname[1],
      
      SeveritySimulation = list(filter(severitySimulationData, Country == CountryName)$simulation[[1]]$totalSeverity),
      SeverityEmpirical = list(filter(empiricalSeverityData, Country == CountryName)$normalizedLoss),
      
      meanSeverityEmpirical = mean(SeverityEmpirical),
      meanSeveritySimulated = mean(SeveritySimulation),
      
      standardDeviationSeverityEmpirical = sd(SeverityEmpirical),
      standardDeviationSeveritySimulated = sd(SeveritySimulation),

      coeffVariationSeverityEmpirical = standardDeviationSeverityEmpirical / meanSeverityEmpirical,
      coeffVariationSeveritySimulated = standardDeviationSeveritySimulated / meanSeveritySimulated,
      
      medianSeverityEmpirical = median(SeverityEmpirical),
      medianSeveritySimulated = median(SeveritySimulation),
      
      skewnessSeverityEmpirical = e1071::skewness(SeverityEmpirical),
      skewnessSeveritySimulated = e1071::skewness(SeveritySimulation),
      
      kurtosisSeverityEmpirical = e1071::kurtosis(SeverityEmpirical),
      kurtosisSeveritySimulated = e1071::kurtosis(SeveritySimulation),
      
      # Formatting 
      
      meanSeverityEmpirical = formatC(meanSeverityEmpirical, format = "e", digits = 2),
      meanSeveritySimulated = formatC(meanSeveritySimulated, format = "e", digits = 2),
      
      standardDeviationSeverityEmpirical = formatC(sd(SeverityEmpirical), format = "e", digits = 2),
      standardDeviationSeveritySimulated = formatC(sd(SeveritySimulation), format = "e", digits = 2),
      
      coeffVariationSeverityEmpirical = round(coeffVariationSeverityEmpirical, 3),
      coeffVariationSeveritySimulated = round(coeffVariationSeveritySimulated, 3),
      
      medianSeverityEmpirical = formatC(medianSeverityEmpirical, format = "e", digits = 2),
      medianSeveritySimulated = formatC(medianSeveritySimulated, format = "e", digits = 2),
      
      skewnessSeverityEmpirical = formatC(skewnessSeverityEmpirical, 3),
      skewnessSeveritySimulated = formatC(skewnessSeveritySimulated, 3),
      
      kurtosisSeverityEmpirical = round(kurtosisSeverityEmpirical, 3),
      kurtosisSeveritySimulated = formatC(kurtosisSeveritySimulated, 3)
    ) %>%
    ungroup() %>%
    dplyr::select(-SeveritySimulation, -SeverityEmpirical)  %>%
    dplyr::select(-CountryName)%>%
    rename('Country' = ISO, 
           'Dist.' = SeverityDistribution, 
           '$\\underset{\\text{Emp.}}{\\text{Mean}}$' = meanSeverityEmpirical, 
           '$\\underset{\\text{Sim.}}{\\text{Mean}}$' = meanSeveritySimulated, 
           '$\\underset{\\text{Emp.}}{\\sigma}$' = standardDeviationSeverityEmpirical,
           '$\\underset{\\text{Sim.}}{\\sigma}$' = standardDeviationSeveritySimulated, 
           '$\\underset{\\text{Emp.}}{\\text{CV}}$' = coeffVariationSeverityEmpirical,
           '$\\underset{\\text{Sim.}}{\\text{CV}}$' = coeffVariationSeveritySimulated,
           '$\\underset{\\text{Emp.}}{\\text{Med.}}$' = medianSeverityEmpirical,
           '$\\underset{\\text{Sim.}}{\\text{Med.}}$' = medianSeveritySimulated,
           '$\\underset{\\text{Emp.}}{\\text{Skew}}$' = skewnessSeverityEmpirical,
           '$\\underset{\\text{Sim.}}{\\text{Skew}}$' = skewnessSeveritySimulated,
           '$\\underset{\\text{Emp.}}{\\text{Kurt}}$' = kurtosisSeverityEmpirical,
           '$\\underset{\\text{Sim.}}{\\text{Kurt}}$' = kurtosisSeveritySimulated
    )
  
  compoundTableData <- tableData %>%
    rowwise() %>%
    mutate(
      ISO = ifelse(CountryName == "All Considered Countries", "AC",
                   filter(relevantCountries, Country == CountryName)$CountryISO[1]),
      
      FrequencyDistribution = filter(combinedSimluationData, Country == CountryName)$selectedModel.x[1],
      SeverityDistribution = filter(combinedSimluationData, Country == CountryName)$selectedModel.y[1]
      ) %>%
    mutate(
      FrequencyDistribution = filter(frequencyDistributionNamesConversion, name == FrequencyDistribution)$nickname[1],
      SeverityDistribution = filter(severityDistributionNamesConversion, name == SeverityDistribution)$nickname[1],
      CompoundDistribution = paste(FrequencyDistribution, "-", SeverityDistribution),
      
      CompoundEmpirical = list(filter(empiricalCompoundData, Country == CountryName)$totalYearlyLoss),
      CompoundSimulation = list(filter(combinedSimluationData, Country == CountryName)$simulation[[1]]$totalSeverity),
      
      meanCompoundEmpirical = mean(CompoundEmpirical),
      meanCompoundSimulated = mean(CompoundSimulation),
      
      standardDeviationCompoundEmpirical = sd(CompoundEmpirical),
      standardDeviationCompoundSimulated = sd(CompoundSimulation),
      
      coeffVariationCompoundEmpirical = standardDeviationCompoundEmpirical / meanCompoundEmpirical,
      coeffVariationCompoundSimulated = standardDeviationCompoundSimulated / meanCompoundSimulated,
      
      medianCompoundEmpirical = median(CompoundEmpirical),
      medianCompoundSimulated = median(CompoundSimulation),
      
      skewnessCompoundEmpirical = e1071::skewness(CompoundEmpirical),
      skewnessCompoundSimulated = e1071::skewness(CompoundSimulation),
      
      kurtosisCompoundEmpirical = e1071::kurtosis(CompoundEmpirical),
      kurtosisCompoundSimulated = e1071::kurtosis(CompoundSimulation),
      
      # Formatting 
      
      meanCompoundEmpirical = formatC(meanCompoundEmpirical, format = "e", digits = 2),
      meanCompoundSimulated = formatC(meanCompoundSimulated, format = "e", digits = 2),
      
      standardDeviationCompoundEmpirical = formatC(standardDeviationCompoundEmpirical, format = "e", digits = 2),
      standardDeviationCompoundSimulated = formatC(standardDeviationCompoundSimulated, format = "e", digits = 2),
      
      coeffVariationCompoundEmpirical = round(coeffVariationCompoundEmpirical, 3),
      coeffVariationCompoundSimulated = round(coeffVariationCompoundSimulated, 3),
      
      medianCompoundEmpirical = formatC(medianCompoundEmpirical, format = "e", digits = 2),
      medianCompoundSimulated = formatC(medianCompoundSimulated, format = "e", digits = 2),
      
      skewnessCompoundEmpirical = formatC(skewnessCompoundEmpirical, 3),
      skewnessCompoundSimulated = formatC(skewnessCompoundSimulated, 3),
      
      kurtosisCompoundEmpirical = round(kurtosisCompoundEmpirical, 3),
      kurtosisCompoundSimulated = formatC(kurtosisCompoundSimulated, 3)
    ) %>%
    ungroup() %>%
    dplyr::select(-CompoundSimulation, -CompoundEmpirical, -FrequencyDistribution, -SeverityDistribution) %>%
    dplyr::select(-CountryName)%>%
    rename('Country' = ISO, 
           'Dist.' = CompoundDistribution, 
           '$\\underset{\\text{Emp.}}{\\text{Mean}}$' = meanCompoundEmpirical, 
           '$\\underset{\\text{Sim.}}{\\text{Mean}}$' = meanCompoundSimulated, 
           '$\\underset{\\text{Emp.}}{\\sigma}$' = standardDeviationCompoundEmpirical,
           '$\\underset{\\text{Sim.}}{\\sigma}$' = standardDeviationCompoundSimulated, 
           '$\\underset{\\text{Emp.}}{\\text{CV}}$' = coeffVariationCompoundEmpirical,
           '$\\underset{\\text{Sim.}}{\\text{CV}}$' = coeffVariationCompoundSimulated,
           '$\\underset{\\text{Emp.}}{\\text{Med.}}$' = medianCompoundEmpirical,
           '$\\underset{\\text{Sim.}}{\\text{Med.}}$' = medianCompoundSimulated,
           '$\\underset{\\text{Emp.}}{\\text{Skew}}$' = skewnessCompoundEmpirical,
           '$\\underset{\\text{Sim.}}{\\text{Skew}}$' = skewnessCompoundSimulated,
           '$\\underset{\\text{Emp.}}{\\text{Kurt}}$' = kurtosisCompoundEmpirical,
           '$\\underset{\\text{Sim.}}{\\text{Kurt}}$' = kurtosisCompoundSimulated
    )
  
  totalSeverityTableData <- tableData %>%
    rowwise() %>%
    mutate(ISO = ifelse(CountryName == "All Considered Countries", "AC", filter(relevantCountries, Country == CountryName)$CountryISO[1])) %>%
    mutate(
      SeverityDistribution = filter(totalSeveritySimulationData, Country == CountryName)$selectedModel.y[1],
      SeverityDistribution = filter(severityDistributionNamesConversion, name == SeverityDistribution)$nickname[1],
      
      SeveritySimulation = list(filter(totalSeveritySimulationData, Country == CountryName)$simulation[[1]]$totalSeverity),
      SeverityEmpirical = list(filter(empiricalTotalSeverityData, Country == CountryName)$normalizedLoss),
      
      meanSeverityEmpirical = mean(SeverityEmpirical),
      meanSeveritySimulated = mean(SeveritySimulation),
      
      standardDeviationSeverityEmpirical = sd(SeverityEmpirical),
      standardDeviationSeveritySimulated = sd(SeveritySimulation),
      
      coeffVariationSeverityEmpirical = standardDeviationSeverityEmpirical / meanSeverityEmpirical,
      coeffVariationSeveritySimulated = standardDeviationSeveritySimulated / meanSeveritySimulated,
      
      medianSeverityEmpirical = median(SeverityEmpirical),
      medianSeveritySimulated = median(SeveritySimulation),
      
      skewnessSeverityEmpirical = e1071::skewness(SeverityEmpirical),
      skewnessSeveritySimulated = e1071::skewness(SeveritySimulation),
      
      kurtosisSeverityEmpirical = e1071::kurtosis(SeverityEmpirical),
      kurtosisSeveritySimulated = e1071::kurtosis(SeveritySimulation),
      
      # Formatting 
      
      meanSeverityEmpirical = formatC(meanSeverityEmpirical, format = "e", digits = 2),
      meanSeveritySimulated = formatC(meanSeveritySimulated, format = "e", digits = 2),
      
      standardDeviationSeverityEmpirical = formatC(sd(SeverityEmpirical), format = "e", digits = 2),
      standardDeviationSeveritySimulated = formatC(sd(SeveritySimulation), format = "e", digits = 2),
      
      coeffVariationSeverityEmpirical = round(coeffVariationSeverityEmpirical, 3),
      coeffVariationSeveritySimulated = round(coeffVariationSeveritySimulated, 3),
      
      medianSeverityEmpirical = formatC(medianSeverityEmpirical, format = "e", digits = 2),
      medianSeveritySimulated = formatC(medianSeveritySimulated, format = "e", digits = 2),
      
      skewnessSeverityEmpirical = formatC(skewnessSeverityEmpirical, 3),
      skewnessSeveritySimulated = formatC(skewnessSeveritySimulated, 3),
      
      kurtosisSeverityEmpirical = round(kurtosisSeverityEmpirical, 3),
      kurtosisSeveritySimulated = formatC(kurtosisSeveritySimulated, 3)
    ) %>%
    ungroup() %>%
    dplyr::select(-SeveritySimulation, -SeverityEmpirical)  %>%
    dplyr::select(-CountryName)%>%
    rename('Country' = ISO, 
           'Dist.' = SeverityDistribution, 
           '$\\underset{\\text{Emp.}}{\\text{Mean}}$' = meanSeverityEmpirical, 
           '$\\underset{\\text{Sim.}}{\\text{Mean}}$' = meanSeveritySimulated, 
           '$\\underset{\\text{Emp.}}{\\sigma}$' = standardDeviationSeverityEmpirical,
           '$\\underset{\\text{Sim.}}{\\sigma}$' = standardDeviationSeveritySimulated, 
           '$\\underset{\\text{Emp.}}{\\text{CV}}$' = coeffVariationSeverityEmpirical,
           '$\\underset{\\text{Sim.}}{\\text{CV}}$' = coeffVariationSeveritySimulated,
           '$\\underset{\\text{Emp.}}{\\text{Med.}}$' = medianSeverityEmpirical,
           '$\\underset{\\text{Sim.}}{\\text{Med.}}$' = medianSeveritySimulated,
           '$\\underset{\\text{Emp.}}{\\text{Skew}}$' = skewnessSeverityEmpirical,
           '$\\underset{\\text{Sim.}}{\\text{Skew}}$' = skewnessSeveritySimulated,
           '$\\underset{\\text{Emp.}}{\\text{Kurt}}$' = kurtosisSeverityEmpirical,
           '$\\underset{\\text{Sim.}}{\\text{Kurt}}$' = kurtosisSeveritySimulated
    )
  
  
  
  # tableCleaner <- function(tableString) {
  #   cleanedString <- gsub("\\\\toprule", "\\\\hline", tableString)
  #   cleanedString <- gsub("\\\\midrule", "\\\\hline", cleanedString)
  #   cleanedString <- gsub("\\\\bottomrule", "\\\\hline", cleanedString)
  #   cleanedString <- gsub("\\\\addlinespace", "", cleanedString)
  #   return(cleanedString)
  # }
  
  frequencyCaption <- "Comparison of empirical and simulated flood frequency distributions by country"
  frequencyTable <- kable(frequencyTableData, format = "latex", booktabs = TRUE, caption = frequencyCaption, escape = FALSE)
  # frequencyTable <- kable(frequencyTableData, format = "latex", booktabs = TRUE, escape = FALSE, caption = frequencyCaption) %>% kable_styling(latex_options = c("hold_position"))
  
  frequencyTable <- tableCleaner(frequencyTable)
  
  
  severityBatch1 <- severityTableData %>% 
    dplyr::select('Country', 
                  'Dist.', 
                  '$\\underset{\\text{Emp.}}{\\text{Mean}}$', 
                  '$\\underset{\\text{Sim.}}{\\text{Mean}}$',
                  '$\\underset{\\text{Emp.}}{\\sigma}$',
                  '$\\underset{\\text{Sim.}}{\\sigma}$',
                  '$\\underset{\\text{Emp.}}{\\text{CV}}$',
                  '$\\underset{\\text{Sim.}}{\\text{CV}}$')
  
  severityCaption1 <- "Comparison of empirical and simulated flood severity distributions by country - Part 1"
  severityTable1 <- kable(severityBatch1, format = "latex", booktabs = TRUE, caption = severityCaption1, escape = FALSE)
  severityTable1 <- tableCleaner(severityTable1)
  
  severityBatch2 <- severityTableData %>% 
    dplyr::select('Country',
                  'Dist.',
                  '$\\underset{\\text{Emp.}}{\\text{Med.}}$',
                  '$\\underset{\\text{Sim.}}{\\text{Med.}}$',
                  '$\\underset{\\text{Emp.}}{\\text{Skew}}$',
                  '$\\underset{\\text{Sim.}}{\\text{Skew}}$',
                  '$\\underset{\\text{Emp.}}{\\text{Kurt}}$',
                  '$\\underset{\\text{Sim.}}{\\text{Kurt}}$')

  severityCaption2 <- "Comparison of empirical and simulated flood severity distributions by country - Part 2"
  severityTable2 <- kable(severityBatch2, format = "latex", booktabs = TRUE, caption = severityCaption2, escape = FALSE)
  severityTable2 <- tableCleaner(severityTable2)
  
  severityBatch3 <- severityTableData %>% 
    dplyr::select('Country',
                  'Dist.',
                  '$\\underset{\\text{Emp.}}{\\text{Mean}}$', 
                  '$\\underset{\\text{Sim.}}{\\text{Mean}}$',
                  '$\\underset{\\text{Emp.}}{\\sigma}$',
                  '$\\underset{\\text{Sim.}}{\\sigma}$',
                  '$\\underset{\\text{Emp.}}{\\text{CV}}$',
                  '$\\underset{\\text{Sim.}}{\\text{CV}}$',
                  '$\\underset{\\text{Emp.}}{\\text{Med.}}$',
                  '$\\underset{\\text{Sim.}}{\\text{Med.}}$',
                  '$\\underset{\\text{Emp.}}{\\text{Skew}}$',
                  '$\\underset{\\text{Sim.}}{\\text{Skew}}$',
                  '$\\underset{\\text{Emp.}}{\\text{Kurt}}$',
                  '$\\underset{\\text{Sim.}}{\\text{Kurt}}$')
  
  severityCaption3 <- "Comparison of empirical and simulated flood severity distributions by country"
  severityTable3 <- kable(severityBatch3, format = "latex", booktabs = TRUE, caption = severityCaption3, escape = FALSE)
  severityTable3 <- tableCleaner(severityTable3)
  
  compoundBatch1 <- compoundTableData %>% 
    dplyr::select('Country',
                  'Dist.',
                  '$\\underset{\\text{Emp.}}{\\text{Mean}}$',
                  '$\\underset{\\text{Sim.}}{\\text{Mean}}$',
                  '$\\underset{\\text{Emp.}}{\\sigma}$',
                  '$\\underset{\\text{Sim.}}{\\sigma}$',
                  '$\\underset{\\text{Emp.}}{\\text{CV}}$',
                  '$\\underset{\\text{Sim.}}{\\text{CV}}$'
    )
  
  compoundCaption1 <- "Comparison of empirical and simulated yearly flood severity distributions by country - Part 1"
  compoundTable1 <- kable(compoundBatch1, format = "latex", booktabs = TRUE, caption = compoundCaption1, escape = FALSE)
  compoundTable1 <- tableCleaner(compoundTable1)
  
  compoundBatch2 <- compoundTableData %>% 
    dplyr::select('Country',
                  'Dist.',
                  '$\\underset{\\text{Emp.}}{\\text{Med.}}$',
                  '$\\underset{\\text{Sim.}}{\\text{Med.}}$',
                  '$\\underset{\\text{Emp.}}{\\text{Skew}}$',
                  '$\\underset{\\text{Sim.}}{\\text{Skew}}$',
                  '$\\underset{\\text{Emp.}}{\\text{Kurt}}$',
                  '$\\underset{\\text{Sim.}}{\\text{Kurt}}$'
    )
  
  compoundCaption2 <- "\\small Comparison of empirical and simulated yearly flood severity distributions by country - Part 2"
  compoundTable2 <- kable(compoundBatch2, format = "latex", booktabs = TRUE, caption = compoundCaption2, escape = FALSE)
  compoundTable2 <- tableCleaner(compoundTable2)
  
  compoundBatch3 <- compoundTableData %>% 
    dplyr::select('Country',
                  'Dist.',
                  '$\\underset{\\text{Emp.}}{\\text{Mean}}$',
                  '$\\underset{\\text{Sim.}}{\\text{Mean}}$',
                  '$\\underset{\\text{Emp.}}{\\sigma}$',
                  '$\\underset{\\text{Sim.}}{\\sigma}$',
                  '$\\underset{\\text{Emp.}}{\\text{CV}}$',
                  '$\\underset{\\text{Sim.}}{\\text{CV}}$',
                  '$\\underset{\\text{Emp.}}{\\text{Med.}}$',
                  '$\\underset{\\text{Sim.}}{\\text{Med.}}$',
                  '$\\underset{\\text{Emp.}}{\\text{Skew}}$',
                  '$\\underset{\\text{Sim.}}{\\text{Skew}}$',
                  '$\\underset{\\text{Emp.}}{\\text{Kurt}}$',
                  '$\\underset{\\text{Sim.}}{\\text{Kurt}}$'
    )
  
  compoundCaption3 <- "\\small Comparison of empirical and simulated yearly flood severity distributions by country"
  compoundTable3 <- kable(compoundBatch3, format = "latex", booktabs = TRUE, caption = compoundCaption3, escape = FALSE)
  compoundTable3 <- tableCleaner(compoundTable3)
  
  
  totalSeverityBatch1 <- totalSeverityTableData %>% 
    dplyr::select('Country', 
                  'Dist.', 
                  '$\\underset{\\text{Emp.}}{\\text{Mean}}$', 
                  '$\\underset{\\text{Sim.}}{\\text{Mean}}$',
                  '$\\underset{\\text{Emp.}}{\\sigma}$',
                  '$\\underset{\\text{Sim.}}{\\sigma}$',
                  '$\\underset{\\text{Emp.}}{\\text{CV}}$',
                  '$\\underset{\\text{Sim.}}{\\text{CV}}$')
  
  totalSeverityCaption1 <- "Comparison of empirical and simulated total flood severity distributions by country - Part 1"
  totalSeverityTable1 <- kable(totalSeverityBatch1, format = "latex", booktabs = TRUE, caption = totalSeverityCaption1, escape = FALSE)
  totalSeverityTable1 <- tableCleaner(totalSeverityTable1)
  
  totalSeverityBatch2 <- totalSeverityTableData %>% 
    dplyr::select('Country',
                  'Dist.',
                  '$\\underset{\\text{Emp.}}{\\text{Med.}}$',
                  '$\\underset{\\text{Sim.}}{\\text{Med.}}$',
                  '$\\underset{\\text{Emp.}}{\\text{Skew}}$',
                  '$\\underset{\\text{Sim.}}{\\text{Skew}}$',
                  '$\\underset{\\text{Emp.}}{\\text{Kurt}}$',
                  '$\\underset{\\text{Sim.}}{\\text{Kurt}}$')
  
  totalSeverityCaption2 <- "Comparison of empirical and simulated total flood severity distributions by country - Part 2"
  totalSeverityTable2 <- kable(totalSeverityBatch2, format = "latex", booktabs = TRUE, caption = totalSeverityCaption2, escape = FALSE)
  totalSeverityTable2 <- tableCleaner(totalSeverityTable2)
  
  totalSeverityBatch3 <- totalSeverityTableData %>% 
    dplyr::select('Country',
                  'Dist.',
                  '$\\underset{\\text{Emp.}}{\\text{Mean}}$', 
                  '$\\underset{\\text{Sim.}}{\\text{Mean}}$',
                  '$\\underset{\\text{Emp.}}{\\sigma}$',
                  '$\\underset{\\text{Sim.}}{\\sigma}$',
                  '$\\underset{\\text{Emp.}}{\\text{CV}}$',
                  '$\\underset{\\text{Sim.}}{\\text{CV}}$',
                  '$\\underset{\\text{Emp.}}{\\text{Med.}}$',
                  '$\\underset{\\text{Sim.}}{\\text{Med.}}$',
                  '$\\underset{\\text{Emp.}}{\\text{Skew}}$',
                  '$\\underset{\\text{Sim.}}{\\text{Skew}}$',
                  '$\\underset{\\text{Emp.}}{\\text{Kurt}}$',
                  '$\\underset{\\text{Sim.}}{\\text{Kurt}}$')
  
  totalSeverityCaption3 <- "Comparison of empirical and simulated total flood severity distributions by country"
  totalSeverityTable3 <- kable(totalSeverityBatch3, format = "latex", booktabs = TRUE, caption = totalSeverityCaption3, escape = FALSE)
  totalSeverityTable3 <- tableCleaner(totalSeverityTable3)
  
  
  evalutationTables <- list(frequency = frequencyTable,
                            severity1 = severityTable1, 
                            severity2 = severityTable2,
                            severity3 = severityTable3,
                            compound1 = compoundTable1, 
                            compound2 = compoundTable2,
                            compound3 = compoundTable3,
                            totalSeverity1 = totalSeverityTable1, 
                            totalSeverity2 = totalSeverityTable2,
                            totalSeverity3 = totalSeverityTable3
  )

  return(evalutationTables)
}
