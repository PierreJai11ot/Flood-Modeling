# Tail risk assessment

# Introducing functions to estimate the value at risk and expected shortfall using the simulation data 

valueAtRisk <- function(sample, level = 0.995) {
  orderStat <- sort(sample)
  cutoff <- ceiling(level*length(sample)) 
  valueAtRisk <- orderStat[cutoff]
  return(valueAtRisk)
}


expectedShortfall <- function(sample, level = 0.99) {
  orderStat <- sort(sample)
  n <- length(sample)
  cutoff <- ceiling(level*n)
  expectedShortfall <- mean(orderStat[cutoff:n])
  return(expectedShortfall)
} 

# Adding the All Considered Countries rows to the normalization data for ease of use

severityNormalizationDataHANZE <- severityNormalizationDataHANZE %>%
  filter(CountryCode != "AC")

allCountriesNormalizationDataHANZE <- severityNormalizationDataHANZE %>%
  group_by(Year) %>% 
  summarise(
    totalVulnerability = sum(totalVulnerability), 
    areaWeighedAverageVulnerability = sum(areaWeighedAverageVulnerability*countryArea)/sum(countryArea), 
    countryArea = sum(countryArea)
  ) %>%
  mutate(Country = "All Considered Countries", 
         CountryCode = "AC")

severityNormalizationDataHANZE <- bind_rows(
  severityNormalizationDataHANZE, 
  allCountriesNormalizationDataHANZE
)



# Retrieve the maximum observation

maxObservationFetcher <- function(data, country, isISO, isYearlyTotal) {
  
  if (country == "AC" || country == "All Considered Countries") {

    filteredData <- data %>% 
      rename(Year2 = Year) %>%
      rowwise() %>%
      mutate(normalizedLossTotalVulnerability = 
               `Losses (2020 euro)`/ pull(filter(filter(severityNormalizationDataHANZE, Year == Year2), CountryCode == "AC"), totalVulnerability)) %>%
      rename(Year = Year2)
      
  }
  
  if (country != "AC" && country != "All Considered Countries") {
    if (isISO) {
      filteredData <- data %>%
        filter(CountryISO == country)
    }
    if (! isISO) {
      filteredData <- data %>%
        filter(Country == country)
    }
    
  }
  
  if (isYearlyTotal) {
    filteredData <- filteredData %>%
      group_by(Year) %>%
      summarise(normalizedLossTotalVulnerability = sum(normalizedLossTotalVulnerability))
  }
  
  maximalObservation <- max(filteredData$normalizedLossTotalVulnerability)
  
  return(maximalObservation)
}


# Computing the risk measures

# Simple loss model case

totalSeveritySimulationData <- totalSeveritySimulationData %>%
  rename(CountryName = Country) %>%
  rowwise() %>%
  mutate(
    areaWeighedAverageVulnerability2020 = severityNormalizationDataHANZE %>%
      filter(Country == CountryName, Year == 2020) %>%
      pull(areaWeighedAverageVulnerability),
    totalVulnerability2020 = severityNormalizationDataHANZE %>%
      filter(Country == CountryName, Year == 2020) %>%
      pull(totalVulnerability),
    simulation2020Euros = ifelse(retainedLossMetric == "normalizedLossTotalVulnerability", 
                                 list(simulation$totalSeverity * totalVulnerability2020),
                                 list(simulation$totalSeverity * areaWeighedAverageVulnerability2020)),
    VaRSev = valueAtRisk(simulation$totalSeverity),
    ESSev = expectedShortfall(simulation$totalSeverity), 
    VaR2020Euros = valueAtRisk(simulation2020Euros),
    ES2020Euros = expectedShortfall(simulation2020Euros), 
  ) %>%
  mutate(maxSev = maxObservationFetcher(dataHANZE, CountryName, F, T), 
         max2020Euros = ifelse(retainedLossMetric == "normalizedLossTotalVulnerability", 
                               (maxSev * totalVulnerability2020),
                               (maxSev * areaWeighedAverageVulnerability2020))) %>%
  ungroup() %>%
  rename(Country = CountryName)


# Simple loss model with Bernoulli filter case: 

bernoulliFrequencyData <- dataHANZE %>%
  group_by(Country, Year) %>%
  summarise(occurences = dplyr::n())  %>%
  dplyr::select(Country, Year) %>%
  group_by(Country) %>%
  summarise(yearsWithOccurences = dplyr::n())  %>%
  add_row(Country = "All Considered Countries", yearsWithOccurences = length(unique(dataHANZE$Year))) %>%
  mutate(frequency = yearsWithOccurences / (2020-1950 + 1)) %>%
  rename(CountryTag = Country)


bernoulliTotalSeveritySimulationData <- totalSeveritySimulationData %>%
  rename(CountryName = Country) %>%
  rowwise() %>%
  mutate(
    bernoulliSimulation = list(rbern(iterations, prob = bernoulliFrequencyData %>%
                                       filter(CountryTag == CountryName) %>%
                                       pull(frequency))),
    simulationSevVector = list(simulation$totalSeverity*unlist(bernoulliSimulation)),
    areaWeighedAverageVulnerability2020 = severityNormalizationDataHANZE %>%
      filter(Country == CountryName, Year == 2020) %>%
      pull(areaWeighedAverageVulnerability),
    totalVulnerability2020 = severityNormalizationDataHANZE %>%
      filter(Country == CountryName, Year == 2020) %>%
      pull(totalVulnerability),
    simulation2020Euros = ifelse(retainedLossMetric == "normalizedLossTotalVulnerability", 
                                 list(unlist(simulationSevVector) * totalVulnerability2020),
                                 list(unlist(simulationSevVector) * areaWeighedAverageVulnerability2020)),
    VaRSev = valueAtRisk(simulationSevVector),
    ESSev = expectedShortfall(simulationSevVector), 
    VaR2020Euros = valueAtRisk(simulation2020Euros),
    ES2020Euros = expectedShortfall(simulation2020Euros), 
  ) %>%
  mutate(maxSev = maxObservationFetcher(dataHANZE, CountryName, F, T), 
         max2020Euros = ifelse(retainedLossMetric == "normalizedLossTotalVulnerability", 
                               (maxSev * totalVulnerability2020),
                               (maxSev * areaWeighedAverageVulnerability2020))) %>%
  ungroup() %>%
  rename(Country = CountryName)


# Compound case 

simulationData <- simulationData %>%
  rename(CountryName = Country) %>%
  rowwise() %>%
  mutate(
    areaWeighedAverageVulnerability2020 = severityNormalizationDataHANZE %>%
      filter(Country == CountryName, Year == 2020) %>%
      pull(areaWeighedAverageVulnerability),
    totalVulnerability2020 = severityNormalizationDataHANZE %>%
      filter(Country == CountryName, Year == 2020) %>%
      pull(totalVulnerability),
    simulation2020Euros = ifelse(retainedLossMetric == "normalizedLossTotalVulnerability", 
                                 list(simulation$totalSeverity * totalVulnerability2020),
                                 list(simulation$totalSeverity * areaWeighedAverageVulnerability2020)),
    VaRSev = valueAtRisk(simulation$totalSeverity),
    ESSev = expectedShortfall(simulation$totalSeverity), 
    VaR2020Euros = valueAtRisk(simulation2020Euros),
    ES2020Euros = expectedShortfall(simulation2020Euros), 
  ) %>%
  mutate(maxSev = maxObservationFetcher(dataHANZE, CountryName, F, T), 
         max2020Euros = ifelse(retainedLossMetric == "normalizedLossTotalVulnerability", 
                               (maxSev * totalVulnerability2020),
                               (maxSev * areaWeighedAverageVulnerability2020))) %>%
  ungroup() %>%
  rename(Country = CountryName)


# Introduction of a function to generate risk measure tables

source(paste0(scriptDirectory, "/Scripts/tailRiskAssessment/tableGenerator.R"))


# Generating and storing the risk measure tables 

writeLines(tailRiskAssessmentTableGenerator(totalSeveritySimulationData, "Risk Measures for the Simple Loss Model"), 
           file.path(paste0(scriptDirectory, "/Texts"), "Risk Measures for the Simple Loss Model.txt"))

writeLines(tailRiskAssessmentTableGenerator(bernoulliTotalSeveritySimulationData, "Risk Measures for the Simple Loss Model Mixture"), 
           file.path(paste0(scriptDirectory, "/Texts"), "Risk Measures for the Simple Loss Model Mixture.txt"))

writeLines(tailRiskAssessmentTableGenerator(simulationData, "Risk Measures for the Compound Model"), 
           file.path(paste0(scriptDirectory, "/Texts"), "Risk Measures for the Compound Model.txt"))
