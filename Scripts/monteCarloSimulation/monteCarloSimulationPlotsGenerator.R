# Defining a function to generate relevant plots

monteCarloSimulationPlotsGenerator <- function(countryList, empiricalData, simulatedData) {

  plots <- c()
  
  for(country in countryList) {
    
    empiricalVector <- filter(empiricalData, Country == country)$totalYearlyLoss
    simulatedVector <- filter(simulatedData, Country == country)$totalSeverity    
    
    if(country != "All Considered Countries"){
      breaks <- pretty(log10(c(empiricalVector, simulatedVector)), n = 30)
      breaks <- 10^breaks
      
      yearlyLossPdfPlot <- ggplot() +
        geom_histogram(data = tibble(value = empiricalVector, Source = "Aggregated simulations"), aes(x = value, y = ..density.., fill = Source, color = Source), position = "identity",alpha = 0.5, breaks = breaks, linewidth = 0.5) +
        geom_histogram(data = tibble(value = empiricalVector), aes(x = value, y = ..density..), fill = "white", color = "white", position = "identity",alpha = 1, breaks = breaks, linewidth = 0.5) +
        geom_histogram(data = tibble(value = empiricalVector, Source = "Empirical"), aes(x = value, y = ..density.., fill = Source, color = Source), position = "identity",alpha = 0.5, breaks = breaks, linewidth = 0.5) +
        geom_histogram(data = tibble(value = simulatedVector, Source = "Simulated"), aes(x = value, y = ..density.., fill = Source, color = Source), position = "identity", alpha = 0.5, breaks = breaks, linewidth = 0.5 ) +
        geom_density(data = tibble(value = empiricalVector, Source = "Empirical"), aes(x = value, color = Source), adjust = 1.5, size = 1.1, linewidth = 0.5) +
        geom_density(data = tibble(value = simulatedVector, Source = "Simulated"), aes(x = value, color = Source), adjust = 1.5, size = 1.1, linewidth = 0.5) +
        geom_vline(xintercept = mean(empiricalVector), linetype = "longdash", color = "red", linewidth = 0.5) +
        geom_vline(xintercept = mean(simulatedVector), linetype = "longdash", color = "blue", linewidth = 0.5) +
        scale_x_log10() +
        scale_color_manual(name = "Data source", values = c("Simulated" = "red", "Empirical" = "blue", "Aggregated simulations" = "darkgreen")) +
        scale_fill_manual(name = "Data source", values = c("Simulated" = "pink", "Empirical" = "lightblue", "Aggregated simulations" = "lightgreen")) +
        labs(
          title = country,
          x = "Flood severity",
          y = "PDF", 
          fill = "Data source",
          color = "Data source"
        ) +
        theme_minimal() +
        theme(
          aspect.ratio = 1,
          plot.title = element_text(hjust = 0.5, face ="bold", size = 10),
          plot.subtitle = element_text(hjust = 0.5),
          plot.caption = element_text(hjust = 0.5),
          legend.position = "bottom",
          axis.title = element_text(size = 8), 
          axis.text.x = element_text(angle = 45, vjust = 1, hjust=1, size = 6),
          axis.text.y = element_text(size = 6),
          legend.title = element_text(size = 8),
          legend.text = element_text(size = 8)
        )
      
      empiricalEmpiricalCdf <- ecdf(empiricalVector)
      simulatedEmpiricalCdf <- ecdf(simulatedVector)
      
      xValues <- sort(unique(c(empiricalVector, simulatedVector)))
      
      yearlyLossCdfPlotData <- tibble(
        x = rep(xValues, 3),
        CDF = c(empiricalEmpiricalCdf(xValues), empiricalEmpiricalCdf(xValues), simulatedEmpiricalCdf(xValues)),
        Source = rep(c("Aggregated simulations", "Empirical", "Simulated"), each = length(xValues))
      )
      
      yearlyLossCdfPlot <- ggplot(yearlyLossCdfPlotData, aes(x = x, y = CDF, color = Source)) +
        geom_line(linewidth = 0.5) +
        scale_x_log10() +
        scale_color_manual(name = "Data source", values = c("Empirical" = "red", "Simulated" = "blue", "Aggregated simulations" = "green")) +
        labs(
          title = country,
          x = "Flood severity",
          y = "CDF"
        ) +
        theme_minimal() +
        theme(
          aspect.ratio = 1,
          plot.title = element_text(hjust = 0.5, face = "bold", size = 10),
          plot.subtitle = element_text(hjust = 0.5),
          plot.caption = element_text(hjust = 0.5),
          legend.position = "bottom",
          axis.title = element_text(size = 8), 
          axis.text.x = element_text(angle = 45, vjust = 1, hjust=1, size = 6),
          axis.text.y = element_text(size = 6),
          legend.title = element_text(size = 8),
          legend.text = element_text(size = 8)
        )
      
      
      n <- length(empiricalVector)
      m <- length(simulatedVector)
      adjustedLengthSimulatedVector <- c()
      indices <- c()
      
      for(i in 1:n) {
        adjustedLengthSimulatedVector <- c(adjustedLengthSimulatedVector, sort(simulatedVector)[round(m*i/n)])
        indices <- c(indices, round(m*i/n))
      }
      
      yearlyLossQQPlotData <- tibble(
        Empirical = sort(empiricalVector),
        Simulated = sort(adjustedLengthSimulatedVector)
      )
      
      yearlyLossQQPlotData <- yearlyLossQQPlotData %>% filter(Empirical > 0, Simulated > 0)
      
      minQuantile <- min(yearlyLossQQPlotData$Empirical, yearlyLossQQPlotData$Simulated, na.rm = TRUE)
      maxQuantile <- max(yearlyLossQQPlotData$Empirical, yearlyLossQQPlotData$Simulated, na.rm = TRUE)
      
      referenceLine <- tibble(x = c(minQuantile, maxQuantile), y = c(minQuantile, maxQuantile))
      
      yearlyLossLogQQPlot <- ggplot() +
        geom_point(data = yearlyLossQQPlotData, aes(x = Simulated, y = Empirical, color = "Aggregated simulations", shape = "Aggregated simulations"), size =0.5) +
        geom_point(data = yearlyLossQQPlotData, aes(x = Simulated, y = Empirical), color = "white", shape = 2, size =0.5) +
        geom_line(data = referenceLine, aes(x = x, y = y), color = "grey", linewidth = 1) +
        geom_point(data = yearlyLossQQPlotData, aes(x = Simulated, y = Empirical, color = "Simulated", shape = "Simulated"), size =0.5) +   
        labs(
          title = country,
          x = "Simulated Quantiles in log scale",
          y = "Empirical Quantiles in log scale"
        ) +
        scale_color_manual(name = "Data source", values = c("Simulated" = "red", "Aggregated simulations" = "green")) +  
        scale_shape_manual(name = "Data source", values = c("Simulated" = 1, "Aggregated simulations" = 2)) + 
        scale_x_log10() +
        scale_y_log10() +
        theme_minimal() +
        theme(
          aspect.ratio = 1,
          plot.title = element_text(hjust = 0.5, face ="bold", size = 10),
          plot.subtitle = element_text(hjust = 0.5),
          plot.caption = element_text(hjust = 0.5),
          legend.position = "bottom",
          axis.title = element_text(size = 8), 
          axis.text.x = element_text(angle = 45, vjust = 1, hjust=1, size = 6),
          axis.text.y = element_text(size = 6),
          legend.title = element_text(size = 8),
          legend.text = element_text(size = 8)
        )    
    }
    
    if(country == "All Considered Countries"){
      
      aggregatedSimulationsVector <- (simulatedData %>%
                                        filter(Country != "All Considered Countries") %>%
                                        rowwise() %>%
                                        mutate(totalSeverity = totalSeverity * 
                                                 (severityNormalizationDataHANZE %>%
                                                 rename(Country2 = Country) %>%
                                                 filter(Country2 == Country) %>%
                                                 filter(Year == 2020) %>%
                                                 pull(totalVulnerability))
                                               / sum(severityNormalizationDataHANZE %>%
                                                    filter(Year == 2020) %>%
                                                    pull(totalVulnerability))
                                                 ) %>%
                                        group_by(SimulationIndex) %>%
                                        summarise(totalSeverity = sum(totalSeverity)))$totalSeverity
      
      # aggregatedSimulationsVector <- (simulatedData %>%
      #   filter(Country != "All Considered Countries") %>%
      #   group_by(SimulationIndex) %>%
      #   summarise(totalSeverity = sum(totalSeverity)))$totalSeverity
        
        # summarise(group_by(filter(simulatedData, Country != "All Considered Countries"), SimulationIndex), totalSeverity = sum(totalSeverity))$totalSeverity
      
      breaks <- pretty(log10(c(empiricalVector, simulatedVector)), n = 30)
      breaks <- 10^breaks
      
      yearlyLossPdfPlot <- ggplot() +
        geom_histogram(data = tibble(value = empiricalVector, Source = "Empirical"), aes(x = value, y = ..density.., fill = Source, color = Source), position = "identity",alpha = 0.5, breaks = breaks, linewidth = 0.5) +
        geom_histogram(data = tibble(value = simulatedVector, Source = "Simulated"), aes(x = value, y = ..density.., fill = Source, color = Source), position = "identity", alpha = 0.5, breaks = breaks, linewidth = 0.5 ) +
        geom_histogram(data = tibble(value = aggregatedSimulationsVector, Source = "Aggregated simulations"), aes(x = value, y = ..density.., fill = Source, color = Source), position = "identity", alpha = 0.5, breaks = breaks, linewidth = 0.5 ) +
        geom_density(data = tibble(value = empiricalVector, Source = "Empirical"), aes(x = value, color = Source), adjust = 1.5, size = 1.1, linewidth = 0.5) +
        geom_density(data = tibble(value = simulatedVector, Source = "Simulated"), aes(x = value, color = Source), adjust = 1.5, size = 1.1, linewidth = 0.5) +
        geom_density(data = tibble(value = aggregatedSimulationsVector, Source = "Aggregated simulations"), aes(x = value, color = Source), adjust = 1.5, size = 1.1, linewidth = 0.5) +
        geom_vline(xintercept = mean(empiricalVector), linetype = "longdash", color = "red", linewidth = 0.5) +
        geom_vline(xintercept = mean(simulatedVector), linetype = "longdash", color = "blue", linewidth = 0.5) +
        geom_vline(xintercept = mean(aggregatedSimulationsVector), linetype = "longdash", color = "darkgreen", linewidth = 0.5) +
        scale_x_log10() +
        scale_color_manual(name = "Data source", values = c("Simulated" = "red", "Empirical" = "blue", "Aggregated simulations" = "darkgreen")) +
        scale_fill_manual(name = "Data source", values = c("Simulated" = "pink", "Empirical" = "lightblue", "Aggregated simulations" = "lightgreen")) +
        labs(
          title = country,
          x = "Flood severity",
          y = "PDF"
        ) +
        theme_minimal() +
        theme(
          aspect.ratio = 1,
          plot.title = element_text(hjust = 0.5, face ="bold", size = 10),
          plot.subtitle = element_text(hjust = 0.5),
          plot.caption = element_text(hjust = 0.5),
          legend.position = "none",
          axis.title = element_text(size = 8), 
          axis.text.x = element_text(angle = 45, vjust = 1, hjust=1, size = 6),
          axis.text.y = element_text(size = 6),
          legend.title = element_text(size = 8),
          legend.text = element_text(size = 8)
        )
      
      empiricalEmpiricalCdf <- ecdf(empiricalVector)
      simulatedEmpiricalCdf <- ecdf(simulatedVector)
      aggregatedSimulationsEmpiricalCdf <- ecdf(aggregatedSimulationsVector)
      
      xValues <- sort(unique(c(empiricalVector, simulatedVector, aggregatedSimulationsVector)))
      
      yearlyLossCdfPlotData <- tibble(
        x = rep(xValues, 3),
        CDF = c(empiricalEmpiricalCdf(xValues), simulatedEmpiricalCdf(xValues), aggregatedSimulationsEmpiricalCdf(xValues)),
        Source = rep(c("Empirical", "Simulated", "Aggregated simulations"), each = length(xValues))
      )
      
      yearlyLossCdfPlot <- ggplot(yearlyLossCdfPlotData, aes(x = x, y = CDF, color = Source)) +
        geom_line(linewidth = 0.5) +
        scale_x_log10() +
        scale_color_manual(name = "Data source", values = c("Empirical" = "red", "Simulated" = "blue", "Aggregated simulations" = "green")) +
        labs(
          title = country,
          x = "Flood severity",
          y = "CDF"
        ) +
        theme_minimal() +
        theme(
          aspect.ratio = 1,
          plot.title = element_text(hjust = 0.5, face = "bold", size = 10),
          plot.subtitle = element_text(hjust = 0.5),
          plot.caption = element_text(hjust = 0.5),
          legend.position = "none",
          axis.title = element_text(size = 8), 
          axis.text.x = element_text(angle = 45, vjust = 1, hjust=1, size = 6),
          axis.text.y = element_text(size = 6),
          legend.title = element_text(size = 8),
          legend.text = element_text(size = 8)
        )
      
      
      n <- length(empiricalVector)
      m <- length(simulatedVector)
      k <- length(aggregatedSimulationsVector)
      adjustedLengthSimulatedVector <- c()
      adjustedLengthAggregatedSimulationsVector <- c()
      indices <- c()
      
      for(i in 1:n) {
        adjustedLengthSimulatedVector <- c(adjustedLengthSimulatedVector, sort(simulatedVector)[round(m*i/n)])
        adjustedLengthAggregatedSimulationsVector <- c(adjustedLengthAggregatedSimulationsVector, sort(aggregatedSimulationsVector)[round(k*i/n)])
      }
      
      yearlyLossQQPlotData <- tibble(
        Empirical = sort(empiricalVector),
        Simulated = sort(adjustedLengthSimulatedVector),
        Aggregated = sort(adjustedLengthAggregatedSimulationsVector)
      )
      
      yearlyLossQQPlotData <- yearlyLossQQPlotData %>% filter(Empirical > 0, Simulated > 0, Aggregated > 0)
      
      minQuantile <- min(yearlyLossQQPlotData$Empirical, yearlyLossQQPlotData$Simulated, yearlyLossQQPlotData$Aggregated, na.rm = TRUE)
      maxQuantile <- max(yearlyLossQQPlotData$Empirical, yearlyLossQQPlotData$Simulated, yearlyLossQQPlotData$Aggregated, na.rm = TRUE)
      
      referenceLine <- tibble(x = c(minQuantile, maxQuantile), y = c(minQuantile, maxQuantile))
      
      yearlyLossLogQQPlot <- ggplot() +
        geom_line(data = referenceLine, aes(x = x, y = y), color = "grey", linewidth = 1) +
        geom_point(data = yearlyLossQQPlotData, aes(x = Simulated, y = Empirical, color = "Simulated", shape = "Simulated"), size =0.5, alpha = 0.75) +   
        geom_point(data = yearlyLossQQPlotData, aes(x = Simulated, y = Aggregated, color = "Aggregated simulations", shape = "Aggregated simulations"), size =0.5, alpha = 0.75) +   
        labs(
          title = country,
          x = "Simulated Quantiles in log scale",
          y = "Empirical Quantiles in log scale"
        ) +
        scale_color_manual(name = "Data source", values = c("Simulated" = "red", "Aggregated simulations" = "green")) +  
        scale_shape_manual(name = "Data source", values = c("Simulated" = 1, "Aggregated simulations" = 2)) + 
        scale_x_log10() +
        scale_y_log10() +
        theme_minimal() +
        theme(
          aspect.ratio = 1,
          plot.title = element_text(hjust = 0.5, face ="bold", size = 10),
          plot.subtitle = element_text(hjust = 0.5),
          plot.caption = element_text(hjust = 0.5),
          legend.position = "none",
          axis.title = element_text(size = 8), 
          axis.text.x = element_text(angle = 45, vjust = 1, hjust=1, size = 6),
          axis.text.y = element_text(size = 6),
          legend.title = element_text(size = 8),
          legend.text = element_text(size = 8)
        )    
    }
    
    plots[[country]] <- list(pdf = yearlyLossPdfPlot, cdf = yearlyLossCdfPlot, QQ = yearlyLossLogQQPlot)
  }
  
  plotList <- tibble(
    Country = names(plots),
    Plots = plots
  )

  return(plotList)
}
