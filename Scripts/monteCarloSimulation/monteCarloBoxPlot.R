# Defining a function to generate box plots

monteCarloBoxPlotGenerator <- function(simulationPlotList, empiricalData, combinedSimluationData, frequencySimulationData, severitySimulationData, totalSeveritySimulationData) {
  
  empiricalFrequencyData <- empiricalData %>% 
    group_by(Country) %>% 
    count(Year, sort = TRUE) %>%
    complete(Year = full_seq(startingYear:endingYear, 1), fill = list(n=0)) %>% rename(NumberOfFloods = n)
  
  usedColumnSym <- sym(retainedLossMetric)
  
  empiricalSeverityData <- empiricalData %>% 
    rename(normalizedLoss = usedColumnSym) %>% 
    dplyr::select(Country, Year, normalizedLoss)
  
  empiricalCompoundData <- empiricalData %>% 
    rename(normalizedLoss = usedColumnSym) %>% 
    group_by(Country, Year) %>%
    summarise(totalYearlyLoss = sum(normalizedLoss)) %>%
    complete(Year = full_seq(startingYear:endingYear, 1), fill = list(totalYearlyLoss=0))
  
  empiricalTotalSeverityData <- empiricalData %>% 
    rename(normalizedLoss = usedColumnSym) %>% 
    dplyr::select(Country, Year, normalizedLoss) %>% 
    group_by(Country, Year) %>% 
    summarise(totalLoss = sum(normalizedLoss))  %>% 
    rename(normalizedLoss = totalLoss)
  
  
  for (country in simulationPlotList$Country) {
    
    if(country != "All Considered Countries"){
      empiricalFrequency <- filter(empiricalFrequencyData, Country == country)$NumberOfFloods 
      
      empiricalSeverity <- filter(empiricalSeverityData, Country == country)$normalizedLoss
      
      empiricalCompound <- filter(empiricalCompoundData, Country == country)$totalYearlyLoss
      
      empiricalTotalSeverity <- filter(empiricalTotalSeverityData, Country == country)$normalizedLoss
    }
    
    if(country == "All Considered Countries"){
      empiricalFrequency <- empiricalFrequencyData %>%
        group_by(Year) %>%
        summarise(aggregateNumberOfFloods = sum(NumberOfFloods))
      
      empiricalFrequency <- empiricalFrequency$aggregateNumberOfFloods
      
      empiricalSeverity <- empiricalSeverityData %>%
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
        pull(normalizedLoss)
  
      empiricalCompound <- empiricalCompoundData %>%
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
        summarise(aggregateTotalYearlyLoss = sum(totalYearlyLoss))
      
      empiricalCompound <- empiricalCompound$aggregateTotalYearlyLoss
      
      empiricalTotalSeverityData <- empiricalTotalSeverityData %>% 
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
        group_by(Year) %>% 
        summarise(totalLoss = sum(normalizedLoss))  %>% 
        rename(normalizedLoss = totalLoss)
    }

    simulatedFrequency <- filter(frequencySimulationData, Country == country)$simulation[[1]]$frequency
      
    simulatedSeverity <- filter(severitySimulationData, Country == country)$simulation[[1]]$totalSeverity

    simulatedCompound <- filter(combinedSimluationData, Country == country)$simulation[[1]]$totalSeverity
    
    simulatedTotalSeverity <- filter(totalSeveritySimulationData, Country == country)$simulation[[1]]$totalSeverity
    
    frequencyData <- bind_rows(
      tibble(Value = empiricalFrequency, Source = "Empirical"),
      tibble(Value = simulatedFrequency, Source = "Simulated")
    )
    
    severityData <- bind_rows(
      tibble(Value = empiricalSeverity, Source = "Empirical"),
      tibble(Value = simulatedSeverity, Source = "Simulated")
    )
    
    compoundData <- bind_rows(
      tibble(Value = empiricalCompound, Source = "Empirical"),
      tibble(Value = simulatedCompound, Source = "Simulated")
    )
    
    totalSeverityData <- bind_rows(
      tibble(Value = empiricalTotalSeverity, Source = "Empirical"),
      tibble(Value = simulatedTotalSeverity, Source = "Simulated")
    )
    
    aspectRatio <- 8 # 8 for 3 plots
    
    frequencyBoxPlot <- ggplot(frequencyData, aes(x = Source, y = Value, fill = Source)) +
      geom_boxplot(outlier.size = 0.5, linewidth = 0.5) +
      scale_fill_manual(name = "Data source", values = c("Empirical" = "pink", "Simulated" = "lightblue")) +
      labs(
        title = "Frequency",
        y = "Yearly floods",
        fill = "Data source"
        ) +
      theme_minimal() +
      theme(
        aspect.ratio = aspectRatio,
        plot.title = element_text(hjust = 0.5, face = "bold", size = 6),
        legend.position = "none",
        axis.title = element_text(size = 6),
        axis.text.y = element_text(angle = 45, vjust = 1, hjust=1, size = 6),
        axis.text.x = element_text(angle = 45, vjust = 1, hjust=1, size = 5),
        axis.title.x = element_blank()
      )
    
    severityBoxPlot <- ggplot(severityData, aes(x = Source, y = Value, fill = Source)) +
      geom_boxplot(outlier.size = 0.5, linewidth = 0.5) +
      scale_fill_manual(name = "Data source", values = c("Empirical" = "pink", "Simulated" = "lightblue")) +
      labs(
        title = "Severity",
        y = "Normalized loss in log scale",
        fill = "Data source"
      ) +
      scale_y_log10() +
      theme_minimal() +
      theme(
        aspect.ratio = aspectRatio,
        plot.title = element_text(hjust = 0.5, face = "bold", size = 6),
        legend.position = "none",
        axis.title = element_text(size = 6),
        axis.text.y = element_text(angle = 45, vjust = 1, hjust=1, size = 6),
        axis.text.x = element_text(angle = 45, vjust = 1, hjust=1, size = 5),
        axis.title.x = element_blank()
      )
    
    combinedBoxPlot <- ggplot(compoundData, aes(x = Source, y = Value, fill = Source)) +
      geom_boxplot(outlier.size = 0.5, linewidth = 0.5) +
      scale_fill_manual(name = "Data source", values = c("Empirical" = "pink", "Simulated" = "lightblue")) +
      labs(
        title = "Compound",
        y = "Normalized yearly loss in log scale",
        fill = "Data source"
      ) +
      scale_y_log10() +
      theme_minimal() +
      theme(
        aspect.ratio = aspectRatio,
        plot.title = element_text(hjust = 0.5, face = "bold", size = 6),
        legend.position = "none",
        axis.title = element_text(size = 6),
        axis.text.y = element_text(angle = 45, vjust = 1, hjust=1, size = 6),
        axis.text.x = element_text(angle = 45, vjust = 1, hjust=1, size = 5),
        axis.title.x = element_blank()
      )
    
    totalSeverityBoxPlot <- ggplot(totalSeverityData, aes(x = Source, y = Value, fill = Source)) +
      geom_boxplot(outlier.size = 0.5, linewidth = 0.5) +
      scale_fill_manual(name = "Data source", values = c("Empirical" = "pink", "Simulated" = "lightblue")) +
      labs(
        title = "Total severity",
        y = "Normalized total loss in log scale",
        fill = "Data source"
      ) +
      scale_y_log10() +
      theme_minimal() +
      theme(
        aspect.ratio = aspectRatio,
        plot.title = element_text(hjust = 0.5, face = "bold", size = 6),
        legend.position = "none",
        axis.title = element_text(size = 6),
        axis.text.y = element_text(angle = 45, vjust = 1, hjust=1, size = 6),
        axis.text.x = element_text(angle = 45, vjust = 1, hjust=1, size = 5),
        axis.title.x = element_blank()
      )
    
    
    combinedBoxPlots <- (frequencyBoxPlot | severityBoxPlot | combinedBoxPlot | totalSeverityBoxPlot) +
      plot_layout(ncol = 4, nrow = 1, guides = "collect") +
      plot_annotation(
        title = country,
        theme = theme(
          plot.title = element_text(size = 10, face = "bold", hjust = 0.5),
          legend.position = "bottom"
        )
      ) + 
      theme(
        aspect.ratio = 8 # 8 for 3 plots
      )
    
    combinedBoxPlots <- wrap_elements(combinedBoxPlots)
    
    simulationPlotList <- simulationPlotList %>%
      mutate(Plots = map2(Country, Plots, ~ if (.x == country) { append(.y, list(boxPlots = combinedBoxPlots))} 
                          else {.y}))
    
  }
  
  return(simulationPlotList)
}
