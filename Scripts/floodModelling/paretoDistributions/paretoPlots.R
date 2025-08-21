# Introducing functions relevant to the assessment of the fit of Pareto and truncated Pareto distributions


# A function generating the data for the Pareto QQ Plot

paretoQQPlotData <- function(randomSample) {
  n <- length(randomSample)
  data <- tibble(
    x = numeric(), 
    y = numeric()
  )
  for (j in 1:n) {
    x = log(sort(randomSample)[n-j+1])
    y = log(j/n)
    data <- add_row(data,
                    x = x,
                    y = y)
  }
  return(data)
}


# A function generating the Pareto QQ Plot

paretoQQPlot <- function(plotTitle, randomSample) {
  paretoQQPlot <- ggplot(paretoQQPlotData(randomSample), aes(x = x, y = y)) +
    geom_point(color = "red", alpha = 0.75, size = 0.5, shape = 1) +
    geom_smooth(aes(color = "Linear"), method = "lm", se = FALSE, linetype = 2, linewidth = 0.5) +
    geom_smooth(aes(color = "Locally linear smoothing"), linewidth = 0.5, se = F) + 
    labs(
      title = plotTitle, 
      x = TeX("$\\log(X_{n-k+1, n})$"),
      y = TeX("$k / n$"),
      color = "Method"
    ) +
    scale_color_manual(values = c("Locally linear smoothing" = "blue", "Linear" = "black")) +  
    theme_minimal() +
    theme(
      aspect.ratio = 1,
      plot.title = element_text(hjust = 0.5, face ="bold", size = 10),
      plot.subtitle = element_text(hjust = 0.5),
      plot.caption = element_text(hjust = 0.5),
      legend.position = "bottom",
      axis.title = element_text(size = 8), 
      axis.text.x = element_text(angle = 0, vjust = 1, hjust=1, size = 6),
      axis.text.y = element_text(size = 6),
      legend.title = element_text(size = 8),
      legend.text = element_text(size = 8)
    )
  return(paretoQQPlot)
}


# A function generating the data for the truncated Pareto QQ Plot

truncatedParetoQQPlotData <- function(randomSample, DkStar) {
  n <- length(randomSample)
  data <- tibble(
    x = numeric(), 
    y = numeric()
  )
  for (j in 1:n) {
    data <- add_row(data,
                    x = log(sort(randomSample)[n-j+1]),
                    y = log(DkStar + j/n))
  }
  return(data)
}


# A function generating the truncated Pareto QQ Plot

truncatedParetoQQPlot <- function(plotTitle, randomSample, DkStar) {
  truncatedParetoQQPlot <- ggplot(truncatedParetoQQPlotData(randomSample, DkStar), aes(x = x, y = y)) +
    geom_point(color = "red", alpha = 0.75, size = 0.5, shape = 1) +
    geom_smooth(aes(color = "Linear"), method = "lm", se = FALSE, linetype = 2, linewidth = 0.5 ) +
    geom_smooth(aes(color = "Locally linear smoothing"), linewidth = 0.5, se = F) + 
    labs(
      title = plotTitle, 
      x = TeX("$\\log(X_{n-k+1, n})$"),
      y = TeX("$\\log(\\hat{D}_{T, k^*, n} + k/n)$"),
      color = "Method"
    ) +
    scale_color_manual(values = c("Locally linear smoothing" = "blue", "Linear" = "black")) +  
    theme_minimal() + 
    theme(
      aspect.ratio = 1,
      plot.title = element_text(hjust = 0.5, face ="bold", size = 10),
      plot.subtitle = element_text(hjust = 0.5),
      plot.caption = element_text(hjust = 0.5),
      legend.position = "bottom",
      axis.title = element_text(size = 8), 
      axis.text.x = element_text(angle = 0, vjust = 1, hjust=1, size = 6),
      axis.text.y = element_text(size = 6),
      legend.title = element_text(size = 8),
      legend.text = element_text(size = 8)
    )
  return(truncatedParetoQQPlot)
}


# A function generating the data for a plot comparing the alpha estimator with the inverse of the Hill statistic as a function of the number of considered sample values

alphaEstimatorComparisionPlotData <- function(randomSample, newtonRaphsonIterations) {
  n <- length(randomSample)
  data <- tibble(
    x = c(), 
    y = c(),
    z = c()
  )
  for (k in 1:n) {
    data <- add_row(data,
                    x = k,
                    y = 1/statisticHill(randomSample, k),
                    z = estimatorsAlpha(randomSample, k, newtonRaphsonIterations)[newtonRaphsonIterations])
  }
  return(data)
}


# A function generating a plot comparing the alpha estimator with the inverse of the Hill statistic as a function of the number of considered sample values

dataAlphaVSInverseHillPlot <- function(plotTitle, dataAlphaVSInverseHillPlotData, kStar) {
  
  kStarAlpha <- data.frame(x = kStar, y = dataAlphaVSInverseHillPlotData$alpha[kStar])
  labelAlpha <- paste0("$\\hat{\\alpha}^T_{", kStar, ",n}$")
  yAlpha <-dataAlphaVSInverseHillPlotData$alpha[kStar]
  
  kStarInverseHill <- data.frame(x = kStar, y = dataAlphaVSInverseHillPlotData$inverseHill[kStar])
  labelInverseHill <- paste0("$1/H_{", kStar, ",n}$")
  yInverseHill <- dataAlphaVSInverseHillPlotData$inverseHill[kStar]
  
  dataAlphaVSInverseHillPlot <- ggplot(dataAlphaVSInverseHillPlotData, aes(x = k, y = inverseHill)) +
    geom_vline(xintercept = kStar, linetype="dashed", color = "black") + 
    geom_line(aes(x = k, y = inverseHill, color = "inverseHill")) + 
    geom_point(data = kStarInverseHill, aes(x=x, y=y), color = "blue") +
    geom_line(aes(x = k, y = alpha, color = "alpha")) + 
    geom_point(data = kStarAlpha, aes(x=x, y=y), color = "red") +
    ggrepel::geom_label_repel(data = kStarInverseHill, 
                              aes(x = x, y = y, 
                                  label = sprintf("frac(1, H['k*=%.0f, n']) == %.2f", x, y)),
                              color = "blue", 
                              hjust = 'right',
                              vjust = 'top', 
                              box.padding = 0.3, 
                              point.padding = 0.5, 
                              segment.color = "blue", 
                              size = 2, 
                              parse = TRUE) +
    ggrepel::geom_label_repel(data = kStarAlpha, 
                              aes(x = x, y = y, 
                                  label = sprintf("hat(alpha)['k*=%.0f, n']^T == %.2f", x, y)),
                              color = "red", 
                              hjust = 'left',
                              vjust = 'bottom', 
                              box.padding = 0.3, 
                              point.padding = 0.5, 
                              segment.color = "red", 
                              size = 2, 
                              parse = TRUE) +
    labs(
      title = plotTitle,
      x = "k",
      y = TeX("$\\hat{\\alpha}^T_{k,n}, \\ 1/H_{k,n}$"),
      color = "Estimator"
    ) +
    scale_color_manual(values = c("inverseHill" = "blue","alpha" = "red"), 
                       labels = c("inverseHill" = TeX("$\\ 1/H_{k,n}$"), "alpha" = TeX("$\\hat{\\alpha}^T_{k,n}$") )) +  
    theme_minimal() + 
    theme(
      aspect.ratio = 1,
      plot.title = element_text(hjust = 0.5, face ="bold", size = 10),
      plot.subtitle = element_text(hjust = 0.5),
      plot.caption = element_text(hjust = 0.5),
      legend.position = "bottom",
      axis.title = element_text(size = 8), 
      axis.text.x = element_text(angle = 0, vjust = 1, hjust=1, size = 6),
      axis.text.y = element_text(size = 6),
      legend.title = element_text(size = 8),
      legend.text = element_text(size = 8)
    )
  
  return(dataAlphaVSInverseHillPlot)
}


# A function generating a plot comparing the p-values of the test statistics distinguishing between the Pareto and truncated Pareto distributions, and between the rough or light truncation

statisticalTestsPlot <- function(plotTitle, estimationData, kStar) {
  
  kStarData <- data.frame(x = kStar, y = estimationData$testApValue[kStar], z = estimationData$testBpValue[kStar])
  
  statisticalTestsPlot <- ggplot(estimationData, aes(x = k, y = testApValue)) +
    geom_vline(xintercept = kStar, linetype="dashed", color = "black") + 
    geom_hline(yintercept = 0.1, linetype=2, color = "black") + 
    geom_hline(yintercept = 0.05, linetype=3, color = "black") + 
    geom_line(aes(x = k, y = testApValue, color = "testA")) + 
    geom_line(aes(x = k, y = testBpValue, color = "testB")) + 
    ggrepel::geom_label_repel(data = kStarData, 
                              aes(x = x, y = z, 
                                  label = sprintf("p['B, k*=%.0f'] == %.2f", x, z)),
                              color = "blue", 
                              hjust = 'right',
                              vjust = 'top', 
                              box.padding = 0.3, 
                              point.padding = 0.5, 
                              segment.color = "blue", 
                              size = 2, 
                              parse = TRUE) +
    ggrepel::geom_label_repel(data = kStarData, 
                              aes(x = x, y = y, 
                                  label = sprintf("p['A, k*=%.0f'] == %.2f", x, y)),
                              color = "red", 
                              hjust = 'left',
                              vjust = 'bottom', 
                              box.padding = 0.3, 
                              point.padding = 0.5, 
                              segment.color = "red", 
                              size = 2, 
                              parse = TRUE) +
    labs(
      title = plotTitle, 
      x = "k",
      y = "observed p-value",
      color = "Test"
    ) +
    scale_color_manual(values = c("testA" = "red","testB" = "blue"), 
                       labels = c("testA" = "A - Pareto or truncated Pareto", "testB" = "B - Rough or light truncation")) +  
    scale_y_continuous(breaks = c(0.05, 0.1, pretty(range(estimationData$testApValue), n = 5)), 
                       labels = function(b) {ifelse(b %in% c(0.05, 0.1), as.character(b), b)}) +
    theme_minimal() + 
    theme(
      aspect.ratio = 1,
      plot.title = element_text(hjust = 0.5, face ="bold", size = 10),
      plot.subtitle = element_text(hjust = 0.5),
      plot.caption = element_text(hjust = 0.5),
      legend.position = "bottom",
      axis.title = element_text(size = 8), 
      axis.text.x = element_text(angle = 0, vjust = 1, hjust=1, size = 6),
      axis.text.y = element_text(size = 6),
      legend.title = element_text(size = 8),
      legend.text = element_text(size = 8)
    )
  
  return(statisticalTestsPlot)
}


# A function generating a plot of the estimator of the odds ratio D as a function of the number of considered sample values

estimatorDPlot <- function(plotTitle, estimationData, kStar) {
  
  DkStarData <- data.frame(x = kStar, y = estimationData$admissibleD[kStar])
  
  estimatorDPlot <- ggplot(estimationData, aes(x = k, y = admissibleD)) +
    geom_vline(xintercept = kStar, linetype="dashed", color = "black") + 
    geom_line(aes(x = k, y = admissibleD), color = "red") + 
    geom_point(data = DkStarData, aes(x=x, y=y), color = "red") +
    ggrepel::geom_label_repel(data = DkStarData, 
                              aes(x = x, y = y, 
                                  label = sprintf("hat(D)['T, k*=%.0f, n']^(0) == %.2f", x, y)),
                              color = "red", 
                              hjust = 'right',
                              vjust = 'top', 
                              box.padding = 0.3, 
                              point.padding = 0.5, 
                              segment.color = "red", 
                              size = 2, 
                              parse = TRUE) +
    labs(
      title = plotTitle, 
      x = "k",
      y = TeX("$\\hat{D}_{T,k,n}^{(0)}$"), 
    ) +
    coord_cartesian(ylim = c(0, 1)) +
    theme_minimal() + 
    theme(
      aspect.ratio = 1,
      plot.title = element_text(hjust = 0.5, face ="bold", size = 10),
      plot.subtitle = element_text(hjust = 0.5),
      plot.caption = element_text(hjust = 0.5),
      legend.position = "bottom",
      axis.title = element_text(size = 8), 
      axis.text.x = element_text(angle = 0, vjust = 1, hjust=1, size = 6),
      axis.text.y = element_text(size = 6),
      legend.title = element_text(size = 8),
      legend.text = element_text(size = 8)
    )
  
  return(estimatorDPlot)
}


# A function generating a plot of the estimator of the 0.01 quantile q as a function of the number of considered sample values

estimatorQPlot <- function(plotTitle, estimationData, kStar, maxObservation, minObservation) {
  
  QkStarData <- data.frame(x = kStar, y = estimationData$Q[kStar])
  
  estimatorQPlot <- ggplot(estimationData, aes(x = k, y = Q)) +
    geom_vline(xintercept = kStar, linetype="dashed", color = "black") + 
    geom_hline(yintercept = maxObservation, linetype=2, color = "black") + 
    annotate("text", x = -Inf, y = maxObservation, label = "sample maximum", hjust = 0, vjust = -1, size = 2.5) +
    geom_hline(yintercept = minObservation, linetype=3, color = "black") + 
    annotate("text", x = -Inf, y = minObservation, label = "sample minimum", hjust = 0, vjust = +1.5, size = 2.5) +
    geom_line(aes(x = k, y = Q), color = "red") + 
    geom_point(data = QkStarData, aes(x=x, y=y), color = "red") +
    ggrepel::geom_label_repel(data = QkStarData, 
                              aes(x = x, y = y, 
                                  label = sprintf("hat(q)['0.01, k*=%.0f, n']^T == %.2f", x, y)),
                              color = "red", 
                              hjust = 'right',
                              vjust = 'top', 
                              box.padding = 0.3, 
                              point.padding = 0.5, 
                              segment.color = "red", 
                              size = 2, 
                              parse = TRUE) +
    labs(
      title = plotTitle, 
      x = "k",
      y = TeX("$\\hat{q}_{0.01,k,n}$"), 
    ) +
    theme_minimal() + 
    theme(
      aspect.ratio = 1,
      plot.title = element_text(hjust = 0.5, face ="bold", size = 10),
      plot.subtitle = element_text(hjust = 0.5),
      plot.caption = element_text(hjust = 0.5),
      legend.position = "bottom",
      axis.title = element_text(size = 8), 
      axis.text.x = element_text(angle = 0, vjust = 1, hjust=1, size = 6),
      axis.text.y = element_text(size = 6),
      legend.title = element_text(size = 8),
      legend.text = element_text(size = 8)
    )
  
  return(estimatorQPlot)
}


# A function generating a plot of the estimator of upper parameter T as a function of the number of considered sample values

estimatorTPlot <- function(plotTitle, estimationData, kStar, maxObservation, minObservation) {
  
  TkStarData <- data.frame(x = kStar, y = estimationData$estimatorT[kStar], z = estimationData$Qzero[kStar])
  
  estimatorTPlot <- ggplot(estimationData, aes(x = k, y = estimatorT)) +
    geom_vline(xintercept = kStar, linetype="dashed", color = "black") + 
    geom_hline(yintercept = maxObservation, linetype=2, color = "black") + 
    annotate("text", x = -Inf, y = maxObservation, label = "sample maximum", hjust = 0, vjust = -1, size = 2.5) +
    geom_hline(yintercept = minObservation, linetype=3, color = "black") + 
    annotate("text", x = -Inf, y = minObservation, label = "sample minimum", hjust = 0, vjust = +1.5, size = 2.5) +
    geom_line(aes(x = k, y = estimatorT, color = "estimatorT"), color = "red") + 
    geom_line(aes(x = k, y = estimatorT, color = "estimatorT"), color = "red") + 
    geom_point(data = TkStarData, aes(x=x, y=y), color = "red") +
    ggrepel::geom_label_repel(data = TkStarData, 
                              aes(x = x, y = y, 
                                  label = sprintf("hat(T)['k*=%.0f, n'] == %.2f", x, y)),
                              color = "red", 
                              hjust = 'right',
                              vjust = 'top', 
                              box.padding = 0.3, 
                              point.padding = 0.5, 
                              segment.color = "red", 
                              size = 2, 
                              parse = TRUE) +
    labs(
      title = plotTitle, 
      x = "k",
      y = TeX("$\\hat{T}_{k,n}, \\hat{q}_{p,k,n}$"), 
    ) +
    theme_minimal() + 
    theme(
      aspect.ratio = 1,
      plot.title = element_text(hjust = 0.5, face ="bold", size = 10),
      plot.subtitle = element_text(hjust = 0.5),
      plot.caption = element_text(hjust = 0.5),
      legend.position = "bottom",
      axis.title = element_text(size = 8), 
      axis.text.x = element_text(angle = 0, vjust = 1, hjust=1, size = 6),
      axis.text.y = element_text(size = 6),
      legend.title = element_text(size = 8),
      legend.text = element_text(size = 8)
    )
  
  return(estimatorTPlot)
}
