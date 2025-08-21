# Introducing a function to combine all the plots of a given kind for all considered countries

plotArrayCombiner <- function(plotListList, plotIndex, globalTitle, limitNumber, limitIsColumns) {
  
  # Evaluate the appropriate formatting
  
  if (limitIsColumns) {
    columns <- limitNumber
    rows <- ceiling(length(plotListList)/limitNumber)
  }
  if (! limitIsColumns) {
    rows <- limitNumber
    columns <- ceiling(length(plotListList)/limitNumber)
  }
  
  # Initialize and increment the list of plots to combine
  
  relevantPlotList <- list()
  
  for (i in 1:length(plotListList)) {
    relevantPlotList[[i]] <- plotListList[[i]][[plotIndex]]
  }
  
  
  # Generate and return the combined plots
  
  combinedPlots <- Reduce(`|`, relevantPlotList) + 
    plot_layout(ncol = columns, nrow = rows, guides = "collect") +
    plot_annotation(
      title = globalTitle,
      theme = theme(
        plot.title = element_text(size = 16, face = "bold", hjust = 0.5),  # Center and style title
        legend.position = "bottom"
      )
    )
  
  return(combinedPlots)
}