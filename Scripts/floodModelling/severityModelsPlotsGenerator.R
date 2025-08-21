# Introducing a function to generate the relevant plots to evaluate the quality of the severity models

severityModelPlots <- function(countryName, logNormal, weibull, pareto, truncatedPareto, plots, zeroAsMinObs, totalSeverity = FALSE) {
  
  severityLabel <- "Flood severity"
  
  if (totalSeverity) {
    severityLabel <- "Total flood severity"
  }
  
  numberOfPoints <- 5000
  
  originalData <- tibble(
    observedSample = logNormal[[1]][[1]]$data
  )
  
  numberOfObservations <- nrow(originalData)
  
  minObs <- min(originalData$observedSample)
  if (zeroAsMinObs) {
    # 10 is relatively close to 0 for the available data with minimum greater than 5000, picking it as a substitute to 0 permits to avoid issues in log scale 
    minObs <- 10
  }
  maxObs <- max(originalData$observedSample) 
  
  if (numberOfObservations <= 100 ){
    binSize <- (maxObs - minObs)/ceiling(log2(numberOfObservations) + 1) # Sturges' Rule
  }
  if (numberOfObservations > 100 ) {
    binSize <- 2*IQR(originalData$observedSample)/(numberOfObservations^(1/3)) # Freedman–Diaconis
  }
  
  empiricalData <- originalData
  
  points <- seq(minObs, maxObs, length.out = numberOfPoints)
  
  theoreticalData <- tibble(
    x = points,
    logNormalPDF = dlnorm(points, meanlog = logNormal[[1]][[1]]$estimate[1], sdlog = logNormal[[1]][[1]]$estimate[2]),
    logNormalCDF = plnorm(points, meanlog = logNormal[[1]][[1]]$estimate[1], sdlog = logNormal[[1]][[1]]$estimate[2]),
    weibullPDF = dweibull(points, shape = weibull[[1]][[1]]$estimate[1], scale = weibull[[1]][[1]]$estimate[2]),
    weibullCDF = pweibull(points, shape = weibull[[1]][[1]]$estimate[1], scale = weibull[[1]][[1]]$estimate[2]),
    paretoPDF = dpareto(points, shape = pareto[[1]][[1]]$estimate["shape"], scale = pareto[[1]][[1]]$estimate["scale"]),
    paretoCDF = ppareto(points, shape = pareto[[1]][[1]]$estimate["shape"], scale = pareto[[1]][[1]]$estimate["scale"]),
    truncatedParetoPDF = dtruncpareto(points, lower = truncatedPareto[[1]][[1]]$estimate["lower"], upper = truncatedPareto[[1]][[1]]$estimate["upper"], shape = truncatedPareto[[1]][[1]]$estimate["shape"]),
    truncatedParetoCDF = ptruncpareto(points, lower = truncatedPareto[[1]][[1]]$estimate["lower"], upper = truncatedPareto[[1]][[1]]$estimate["upper"], shape = truncatedPareto[[1]][[1]]$estimate["shape"]),
  )
  
  pdfPlot <- ggplot(empiricalData, aes(x = observedSample)) +
    geom_histogram(aes(y = ..density..), binwidth = binSize, color="grey", fill="lightgrey", alpha=0.5) + 
    geom_density(outline.type = "upper", color="black") +
    geom_line(data = theoreticalData, aes(x = x, y = logNormalPDF, color = "Log-Normal")) + 
    geom_line(data = theoreticalData, aes(x = x, y = weibullPDF, color = "Weibull")) +
    geom_line(data = theoreticalData, aes(x = x, y = paretoPDF, color = "Pareto")) +
    geom_line(data = theoreticalData, aes(x = x, y = truncatedParetoPDF, color = "Truncated Pareto")) +
    labs(
      # title = "Empirical and theoretical PDFs",
      title = countryName,
      x = severityLabel,
      y = "PDF", 
      color = "Distribution"
    ) +
    scale_color_manual(values = c("Log-Normal" = "blue", "Weibull" = "red", "Pareto" = "green", "Truncated Pareto" = "purple")) +  
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
  
  empiricalData <- empiricalData %>% group_by(observedSample) %>%
    summarise(observationCount = dplyr::n()) %>%
    rowwise() %>%
    mutate(observationFrequency = observationCount / numberOfObservations) %>%
    ungroup() %>%
    mutate(empiricalCDF = cumsum(observationFrequency)) %>%
    mutate(censoredEmpiricalCDF = cumsum(observationFrequency)*numberOfObservations/(numberOfObservations + 1))
  
  if (zeroAsMinObs) {
    empiricalData <- add_row(empiricalData,
                             observedSample = 0, 
                             observationCount = 0,
                             observationFrequency = 0,
                             empiricalCDF = 0,
                             censoredEmpiricalCDF = NA)
  }
  
  cdfPlot <- ggplot(empiricalData, aes(x = observedSample, y = empiricalCDF)) +
    geom_step(color="black", lineend = "square") +
    geom_line(data = theoreticalData, aes(x = x, y = logNormalCDF, color = "Log-Normal", linetype = "Log-Normal")) +
    geom_line(data = theoreticalData, aes(x = x, y = weibullCDF, color = "Weibull", linetype = "Weibull")) +
    geom_line(data = theoreticalData, aes(x = x, y = paretoCDF, color = "Pareto", linetype = "Pareto")) +
    geom_line(data = theoreticalData, aes(x = x, y = truncatedParetoCDF, color = "Truncated Pareto", linetype = "Truncated Pareto")) +
    labs(
      # title = "Empirical and theoretical CDFs",
      title = countryName,
      x = severityLabel,
      y = "CDF"
    ) +
    scale_color_manual(name = "Distribution", values = c("Log-Normal" = "blue", "Weibull" = "red", "Pareto" = "green", "Truncated Pareto" = "purple")) +  
    scale_linetype_manual(name = "Distribution", values = c("Log-Normal" = "longdash", "Weibull" = "dashed", "Pareto" = "dotted", "Truncated Pareto" = "dotdash")) + 
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
  
  
  pointsLog <- 10^seq(log10(minObs), log10(maxObs), length.out=numberOfPoints)
  
  # exp(log(10)*seq(log10(minObs), log10(maxObs), length.out = numberOfPoints)) # more adequate spacing of points for log scale represntations
  
  theoreticalDataLog <- tibble(
    x = pointsLog,
    logNormalPDF = dlnorm(pointsLog, meanlog = logNormal[[1]][[1]]$estimate[1], sdlog = logNormal[[1]][[1]]$estimate[2]),
    logNormalCDF = plnorm(pointsLog, meanlog = logNormal[[1]][[1]]$estimate[1], sdlog = logNormal[[1]][[1]]$estimate[2]),
    weibullPDF = dweibull(pointsLog, shape = weibull[[1]][[1]]$estimate[1], scale = weibull[[1]][[1]]$estimate[2]),
    weibullCDF = pweibull(pointsLog, shape = weibull[[1]][[1]]$estimate[1], scale = weibull[[1]][[1]]$estimate[2]), 
    paretoPDF = dpareto(pointsLog, shape = pareto[[1]][[1]]$estimate["shape"], scale = pareto[[1]][[1]]$estimate["scale"]),
    paretoCDF = ppareto(pointsLog, shape = pareto[[1]][[1]]$estimate["shape"], scale = pareto[[1]][[1]]$estimate["scale"]),
    truncatedParetoPDF = dtruncpareto(pointsLog, lower = truncatedPareto[[1]][[1]]$estimate["lower"], upper = truncatedPareto[[1]][[1]]$estimate["upper"], shape = truncatedPareto[[1]][[1]]$estimate["shape"]),
    truncatedParetoCDF = ptruncpareto(pointsLog, lower = truncatedPareto[[1]][[1]]$estimate["lower"], upper = truncatedPareto[[1]][[1]]$estimate["upper"], shape = truncatedPareto[[1]][[1]]$estimate["shape"]),
  )
  
  cdfLogPlot <- ggplot(empiricalData, aes(x = observedSample, y = empiricalCDF)) +
    geom_step(color="black", lineend = "square") +
    geom_line(data = theoreticalDataLog, aes(x = x, y = logNormalCDF, color = "Log-Normal", linetype = "Log-Normal")) +
    geom_line(data = theoreticalDataLog, aes(x = x, y = weibullCDF, color = "Weibull", linetype = "Weibull")) +
    geom_line(data = theoreticalDataLog, aes(x = x, y = paretoCDF, color = "Pareto", linetype = "Pareto")) +
    geom_line(data = theoreticalDataLog, aes(x = x, y = truncatedParetoCDF, color = "Truncated Pareto", linetype = "Truncated Pareto")) +
    labs(
      # title = "Empirical and theoretical CDFs",
      title = countryName,
      x = paste0(severityLabel, " in log scale"),
      y = "CDF"
    ) +
    scale_color_manual(name = "Distribution", values = c("Log-Normal" = "blue", "Weibull" = "red", "Pareto" = "green", "Truncated Pareto" = "purple")) +  
    scale_linetype_manual(name = "Distribution", values = c("Log-Normal" = "longdash", "Weibull" = "dashed", "Pareto" = "dotted", "Truncated Pareto" = "dotdash")) + 
    scale_x_log10() +
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
  
  empiricalData <- empiricalData %>% 
    filter(observationCount != 0 ) %>% 
    ungroup() %>%
    rowwise() %>%
    mutate(logNormalQuantile = qlnorm(censoredEmpiricalCDF, meanlog = logNormal[[1]][[1]]$estimate[1], sdlog = logNormal[[1]][[1]]$estimate[2])) %>% 
    mutate(weibullQuantile = qweibull(censoredEmpiricalCDF, shape = weibull[[1]][[1]]$estimate[1], scale = weibull[[1]][[1]]$estimate[2])) %>% 
    mutate(paretoQuantile = qpareto(censoredEmpiricalCDF, shape = pareto[[1]][[1]]$estimate["shape"], scale = pareto[[1]][[1]]$estimate["scale"])) %>% 
    mutate(truncatedParetoQuantile = qtruncpareto(censoredEmpiricalCDF, lower = truncatedPareto[[1]][[1]]$estimate["lower"], upper = truncatedPareto[[1]][[1]]$estimate["upper"], shape = truncatedPareto[[1]][[1]]$estimate["shape"])) %>% 
    mutate(logNormalProba = plnorm(observedSample, meanlog = logNormal[[1]][[1]]$estimate[1], sdlog = logNormal[[1]][[1]]$estimate[2])) %>% 
    mutate(weibullProba = pweibull(observedSample, shape = weibull[[1]][[1]]$estimate[1], scale = weibull[[1]][[1]]$estimate[2])) %>%
    mutate(paretoProba = ppareto(observedSample, shape = pareto[[1]][[1]]$estimate["shape"], scale = pareto[[1]][[1]]$estimate["scale"])) %>% 
    mutate(truncatedParetoProba = ptruncpareto(observedSample, lower = truncatedPareto[[1]][[1]]$estimate["lower"], upper = truncatedPareto[[1]][[1]]$estimate["upper"], shape = truncatedPareto[[1]][[1]]$estimate["shape"])) 
  
  minQuantile = min(empiricalData$observedSample, empiricalData$logNormalQuantile, empiricalData$weibullQuantile, empiricalData$paretoQuantile, empiricalData$truncatedParetoQuantile, na.rm = TRUE)
  maxQuantile = max(empiricalData$observedSample, empiricalData$logNormalQuantile, empiricalData$weibullQuantile, empiricalData$paretoQuantile, empiricalData$truncatedParetoQuantile, na.rm = TRUE)
  
  alphaPoints <- 0.75
  sizePoints <- 0.5
  pointShapes <- c(1,2,3,4)
  
  QQPlot <- ggplot() +
    geom_segment(aes(x = minQuantile, y = minQuantile, xend = maxQuantile, yend = maxQuantile), color = "grey", size = 1) +
    geom_point(data = empiricalData, aes(x = logNormalQuantile, y = observedSample, color = "Log-Normal", shape = "Log-Normal"), alpha = alphaPoints, size = sizePoints) +
    geom_point(data = empiricalData, aes(x = weibullQuantile, y = observedSample, color = "Weibull", shape = "Weibull"), alpha = alphaPoints, size = sizePoints) +
    geom_point(data = empiricalData, aes(x = paretoQuantile, y = observedSample, color = "Pareto", shape = "Pareto"), alpha = alphaPoints, size = sizePoints) +
    geom_point(data = empiricalData, aes(x = truncatedParetoQuantile, y = observedSample, color = "Truncated Pareto", shape = "Truncated Pareto"), alpha = alphaPoints, size = sizePoints) +
    # geom_abline(slope = 1, xintercept = minQuantile, yintercept = minQuantile, color = "grey") +
    labs(
      # title = "Q-Q plot",
      title = countryName,
      x = "Theoretical quantiles",
      y = "Empirical quantiles", 
      color = "Distribution",
      shape = "Distribution"
    ) +
    scale_color_manual(values = c("Log-Normal" = "blue", "Weibull" = "red", "Pareto" = "green", "Truncated Pareto" = "purple")) +  
    scale_shape_manual(values = c("Log-Normal" = 1, "Weibull" = 2, "Pareto" = 5, "Truncated Pareto" = 0)) + 
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
  
  logQQPlot <- ggplot() +
    geom_segment(aes(x = minQuantile, y = minQuantile, xend = maxQuantile, yend = maxQuantile), color = "grey", size = 1) +
    geom_point(data = empiricalData, aes(x = logNormalQuantile, y = observedSample, color = "Log-Normal", shape = "Log-Normal"), alpha = alphaPoints, size = sizePoints) +
    geom_point(data = empiricalData, aes(x = weibullQuantile, y = observedSample, color = "Weibull", shape = "Weibull"), alpha = alphaPoints, size = sizePoints) +
    geom_point(data = empiricalData, aes(x = paretoQuantile, y = observedSample, color = "Pareto", shape = "Pareto"), alpha = alphaPoints, size = sizePoints) +
    geom_point(data = empiricalData, aes(x = truncatedParetoQuantile, y = observedSample, color = "Truncated Pareto", shape = "Truncated Pareto"), alpha = alphaPoints, size = sizePoints) +
    # geom_abline(slope = 1, xintercept = minQuantile, yintercept = minQuantile, color = "grey") +
    labs(
      # title = "Q-Q plot",
      title = countryName,
      x = "Theoretical quantiles in log scale",
      y = "Empirical quantiles in log scale", 
      color = "Distribution",
      shape = "Distribution"
    ) +
    scale_color_manual(values = c("Log-Normal" = "blue", "Weibull" = "red",  "Pareto" = "green", "Truncated Pareto" = "purple")) +  
    scale_shape_manual(values = c("Log-Normal" = pointShapes[1], "Weibull" = pointShapes[2], "Pareto" = 5, "Truncated Pareto" = 0)) + 
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
  
  PPPlot <- ggplot() +
    geom_segment(aes(x = 0, y = 0, xend = 1, yend = 1), color = "grey", size = 1) +
    geom_point(data = empiricalData, aes(x = logNormalProba, y = censoredEmpiricalCDF, color = "Log-Normal", shape = "Log-Normal"), alpha = alphaPoints, size = sizePoints) +
    geom_point(data = empiricalData, aes(x = weibullProba, y = censoredEmpiricalCDF, color = "Weibull", shape = "Weibull"), alpha = alphaPoints, size = sizePoints) +
    geom_point(data = empiricalData, aes(x = paretoProba, y = censoredEmpiricalCDF, color = "Pareto", shape = "Pareto"), alpha = alphaPoints, size = sizePoints) +
    geom_point(data = empiricalData, aes(x = truncatedParetoProba, y = censoredEmpiricalCDF, color = "Truncated Pareto", shape = "Truncated Pareto"), alpha = alphaPoints, size = sizePoints) +
    labs(
      title = countryName,
      x = "Theoretical cumulative distribution",
      y = "Empirical cumulative distribution", 
      color = "Distribution",
      shape = "Distribution"
    ) +
    scale_color_manual(values = c("Log-Normal" = "blue", "Weibull" = "red", "Pareto" = "green", "Truncated Pareto" = "purple")) +  
    scale_shape_manual(values = c("Log-Normal" = 1, "Weibull" = 2, "Pareto" = 5, "Truncated Pareto" = 0)) + 
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
  
  plotList <- list(pdf = pdfPlot, cdf = cdfPlot, cdfLog = cdfLogPlot, qq = QQPlot, qqLog = logQQPlot, pp = PPPlot, 
                   paretoQQ = plots[[1]]$paretoQQ, 
                   truncatedParetoQQ = plots[[1]]$truncatedParetoQQ, 
                   estimatorAlphaVSInverseHill = plots[[1]]$estimatorAlphaVSInverseHill, 
                   estimatorD = plots[[1]]$estimatorD, 
                   estimatorQ = plots[[1]]$estimatorQ, 
                   estimatorT = plots[[1]]$estimatorT, 
                   statisticalTest = plots[[1]]$statisticalTest)
  
  return(plotList)
}
