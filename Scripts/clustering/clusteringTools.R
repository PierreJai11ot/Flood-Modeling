# Tools for the different clustering strategies

# Introducing a function to aid the normalization of losses

clusterNormalizationDataFetcher <- function(cluster, normalizationData, year = 2020, isISO = FALSE) {
  
  if(isISO){
    normalizationData <- normalizationData %>%
      rename(Cntry = Country) %>% 
      rename(Country = CountryCode)
  }
  
  unlistedCluster <- unlist(cluster)
  
  totalArea <- 0
  totalAreaWeighedAverageVulnerability <- 0 
  totalTotalVulnerability <- 0 
  
  for(country in unlistedCluster) {
    
    countryArea <- normalizationData %>% 
      filter(Year == year, Country == country) %>%
      pull(countryArea)
    
    countryAreaWeighedAverageVulnerability <- normalizationData %>%
      filter(Year == year, Country == country) %>%
      pull(areaWeighedAverageVulnerability)
    
    countryTotalVulnerability <- normalizationData %>%
      filter(Year == year, Country == country) %>%
      pull(totalVulnerability)
    
    totalArea <- totalArea + countryArea
    
    totalAreaWeighedAverageVulnerability <- totalAreaWeighedAverageVulnerability + (countryAreaWeighedAverageVulnerability * countryArea)
    
    totalTotalVulnerability <- totalTotalVulnerability + countryTotalVulnerability
  }
  
  totalAreaWeighedAverageVulnerability <- totalAreaWeighedAverageVulnerability / totalArea
  
  result <- list(clusterArea = totalArea, clusterAreaWeighedAverageVulnerability = totalAreaWeighedAverageVulnerability, clusterTotalTotalVulnerability = totalTotalVulnerability)
  
  # print("NEWCALL")
  # print(cluster)
  # print(paste0("HERE RES: ", result))
  
  return(result)
}


# Introducing a function that edits the flood loss data to produce appropriately normalized yearly loss aggregates per considered cluster 

clusterDataGenerator <- function(data, normalizationData, clusterList, commonYearsOnly = TRUE, isISO = FALSE, isCompound = FALSE) {
  
  clusterList <- clusterList[lengths(clusterList) > 0]
  
  editedData <- data 
  
  if(isISO) {
    editedData <- editedData %>%
      rename(Cntry = Country) %>%
      rename(Country = CountryISO)
  }
  
  newDataSet <- tibble()
  
  if(! isCompound){

    editedData <- editedData %>% 
      group_by(Country, Year) %>% 
      summarise(totalYearlyLossPerCountry = sum(`Losses (2020 euro)`, na.rm = TRUE),.groups = "drop", 
                countryArea = first(countryArea),
                areaWeighedAverageVulnerability = first(areaWeighedAverageVulnerability), 
                totalVulnerability = first(totalVulnerability)
      ) %>%
      ungroup()
    
    for (i in 1:length(clusterList)) {
      cluster <- unlist(clusterList[i])
      
      clusterData <- editedData %>% 
        filter(Country %in% cluster)
      
      if(commonYearsOnly) {
        clusterData <- clusterData %>% 
          group_by(Year) %>%
          mutate(rowsInYear = dplyr::n()) %>%
          ungroup() %>%
          filter(rowsInYear == length(cluster))
      }
      
      clusterData <- clusterData %>%
        group_by(Year) %>% 
        summarise(totalYearlyLoss = sum(totalYearlyLossPerCountry, na.rm = TRUE),.groups = "drop"
        ) %>%
        mutate(cluster = list(cluster), 
               clusterID = i) %>%
        relocate(cluster, .before = 1)  %>%
        relocate(clusterID, .before = 1)  
      
      clusterData <- clusterData %>%
        rowwise() %>%
        mutate(normalization = list(clusterNormalizationDataFetcher(cluster, normalizationData, Year, isISO)))  %>%
        mutate(clusterArea = normalization$clusterArea,
               areaWeighedAverageVulnerability = normalization$clusterAreaWeighedAverageVulnerability,
               totalVulnerability = normalization$clusterTotalTotalVulnerability
        ) %>%
        mutate(normalizedLossAreaWeighedAverageVulnerability = totalYearlyLoss / areaWeighedAverageVulnerability,
               normalizedLossTotalVulnerability = totalYearlyLoss / totalVulnerability)
      
      newDataSet <- bind_rows(newDataSet, clusterData)
    }
  }
  
  if(isCompound){
    
    for (i in 1:length(clusterList)) {
      cluster <- unlist(clusterList[i])
      
      clusterData <- editedData %>% 
        filter(Country %in% cluster)
      
      if(commonYearsOnly) {
        clusterData <- clusterData %>% 
          group_by(Year) %>%
          mutate(rowsInYear = dplyr::n()) %>%
          ungroup() %>%
          filter(rowsInYear == length(cluster))
      }
      
      clusterData <- clusterData %>%
        mutate(cluster = list(cluster), 
               clusterID = i) %>%
        relocate(cluster, .before = 1)  %>%
        relocate(clusterID, .before = 1)  
      
      clusterData <- clusterData %>%
        rowwise() %>%
        mutate(normalization = list(clusterNormalizationDataFetcher(cluster, normalizationData, Year, isISO)))  %>%
        mutate(clusterArea = normalization$clusterArea,
               areaWeighedAverageVulnerability = normalization$clusterAreaWeighedAverageVulnerability,
               totalVulnerability = normalization$clusterTotalTotalVulnerability
        ) %>%
        mutate(normalizedLossAreaWeighedAverageVulnerability = `Losses (2020 euro)` / areaWeighedAverageVulnerability,
               normalizedLossTotalVulnerability = `Losses (2020 euro)` / totalVulnerability)
      
      newDataSet <- bind_rows(newDataSet, clusterData)
      }

  }
  
  
  
  return(newDataSet)
}


# Correlation based clustering 

# Introducing a function generating the Pearson and Spearman rank correlation tables

correlationMatrixGenerator <- function(dataSet, normalizationData, nonNegativeEntriesOnly = TRUE, bothNonNegative = FALSE, 
                                       type = "pearson", dropFirstColumn = TRUE, normalizationMethod = "clusterPair") {
  
  countries <- colnames(dataSet)
  
  if(dropFirstColumn) {
    countries <- countries[-1]
  }
  
  correlationMatrix <- matrix(NA, nrow = length(countries), ncol = length(countries),
                              dimnames = list(countries, countries))
  
  years <- dataSet$Year
  
  for (x in 1:(length(countries)-1)) {
    for (y in x:length(countries)) {
      
      i <- countries[x]
      j <- countries[y]
      
      # print(paste0("A: ", i))
      # print(paste0("B: ", j))
      
      
      countryA <- dataSet %>% pull(!!sym(i))
      countryB <- dataSet %>% pull(!!sym(j))
      
      # print(paste0("Vector A: ", countryA))
      # print(paste0("Vector B: ", countryB))
      # print("EXIT1")
      
      if (nonNegativeEntriesOnly) {
        if (bothNonNegative) {
          indices <- which(countryA > 0 & countryB > 0)
        }
        if (! bothNonNegative) {
          indices <- which(countryA > 0 | countryB > 0)
        }
      }
      if (!nonNegativeEntriesOnly) {
        indices <- 1:length(countryA)
      }
      
      # print(paste0("Indices: ", paste0(indices)))
      
      
      if (normalizationMethod == "clusterPair") {
        
        # print(paste0("OK: ", 2))
        
        cluster <- append(i, j)
        for(index in indices) {
          normalizationMetrics <- clusterNormalizationDataFetcher(cluster, normalizationData, year = years[index], isISO = TRUE)
          
          # print(paste0("YEAR: ", years[index]))
          # print(paste0("A index: ", countryA[index]))
          # print(paste0("B index: ", countryB[index]))
          
          if(retainedLossMetric == "normalizedLossTotalVulnerability") {
            countryA[index] <- countryA[index] / normalizationMetrics$clusterTotalTotalVulnerability
            countryB[index] <- countryB[index] / normalizationMetrics$clusterTotalTotalVulnerability
          }
          
          if(! retainedLossMetric == "normalizedLossTotalVulnerability") {
            countryA[index] <- countryA[index] / normalizationMetrics$clusterAreaWeighedAverageVulnerability
            countryB[index] <- countryB[index] / normalizationMetrics$clusterAreaWeighedAverageVulnerability
          }
          
          # print(paste0("AFTER: ", years[index]))
          # print(paste0("A index: ", countryA[index]))
          # print(paste0("B index: ", countryB[index]))

        }
      }
      
      if (normalizationMethod == "individual") {
        
        for(index in indices) {
          normalizationMetricsA <- clusterNormalizationDataFetcher(i, normalizationData, year = years[index], isISO = TRUE)
          normalizationMetricsB <- clusterNormalizationDataFetcher(j, normalizationData, year = years[index], isISO = TRUE)
          
          if(retainedLossMetric == "normalizedLossTotalVulnerability") {
            countryA[index] <- countryA[index] / normalizationMetricsA$clusterTotalTotalVulnerability
            countryB[index] <- countryB[index] / normalizationMetricsB$clusterTotalTotalVulnerability
          }
          
          if(! retainedLossMetric == "normalizedLossTotalVulnerability") {
            countryA[index] <- countryA[index] / normalizationMetricsA$clusterAreaWeighedAverageVulnerability
            countryB[index] <- countryB[index] / normalizationMetricsB$clusterAreaWeighedAverageVulnerability
          }
          
        }
      }
      if (length(indices) >= 2) {
        # print(paste0("COMPUTED", i, j))
        correlationMatrix[i, j] <- cor(countryA[indices], countryB[indices], method = type)
        correlationMatrix[j, i] <- cor(countryA[indices], countryB[indices], method = type)
      } 
    }
  }
  
  diag(correlationMatrix) <- 1
  
  colnames(correlationMatrix) <- countries
  rownames(correlationMatrix) <- countries
  
  return(correlationMatrix)
}


# Introducing a function to convert a correlation matrix into a dissimilarity matrix 

correlationToDissimilarityConverter <- function(correlationMatrix, type) {
  
  # The aim being to cluster countries unlikely to have events at the same time, similarity would correspond to low correlation
  if (type == "Half1PlusCorr") {
    dissimilarityMatrix <- (1 + correlationMatrix)/2
  }
  if (type == "Half1MinusCorr") {
    dissimilarityMatrix <- (1 - correlationMatrix)/2
  }
  
  if (type == "1MinusAbsCorr") {
    dissimilarityMatrix <- 1 - abs(correlationMatrix)
  }
  if (type == "AbsCorr") {
    dissimilarityMatrix <- abs(correlationMatrix)
  }
  
  # To have a reflexive dissimilarity, the dissimilarity of any object with itself must be zero 
  
  for (i in 1:nrow(dissimilarityMatrix)) {
    dissimilarityMatrix[i,i] <- 0 
  }
  
  return(dissimilarityMatrix)
}


# Introducing a function to generate dendograms for hierarchical clustering processes 

dendogramGenerator <- function(hierarchicalClustrer, numberOfClusters, colorScheme, title, xlabel = " ", ylabel = " ") {
  
  
  # hierarchicalClustrer <- hierarchicalClustering
  # numberOfClusters <- 6
  # colorScheme <- c("blue", "red", "yellow", "green", "purple", "turquoise")
  # title <- "TITLEPLACEHOLDER"
  # xlabel <-  " "
  # ylabel <- "height"
  
  
  dendogram <- as.dendrogram(hierarchicalClustrer) %>%
    set(
      "branches_k_color",
      k = numberOfClusters)
  
  tipOrder <- order.dendrogram(dendogram)
  
  dendogram <- as.ggdend(dendogram)
  
  segments <- dendogram$segments
  labels <- dendogram$labels
  
  clusters <- cutree(hierarchicalClustrer, k = numberOfClusters)
  orderedLables <- names(clusters)[tipOrder]
  orderedClusters <- clusters[tipOrder]
  
  clusterColors <- colorScheme
  clusterFill <- clusterColors[orderedClusters]
  
  labels$cluster <- orderedClusters
  labels$fill <- clusterFill
  
  dendogramPlot <- ggplot() +
    geom_segment(data = segments,
                 aes(x = x, y = y, xend = xend, yend = yend, color = col), lineend = "round", linewidth = 1) +
    geom_point(data = labels, aes(x = x, y = y), shape = 21, size = 7, stroke = 0.5
               , fill = "white"
               ) +
    geom_text(data = labels, aes(x = x , y = y, label = label), size = 3, color = "black") +
    coord_flip() +
    scale_y_reverse() +
    theme_minimal() +
    labs(
      title = title,
      x = xlabel,
      y = ylabel
    ) +
    theme_minimal() +
    theme(
      aspect.ratio = 1,
      plot.title = element_text(hjust = 0.5, face ="bold", size = 10),
      plot.subtitle = element_text(hjust = 0.5),
      plot.caption = element_text(hjust = 0.5),
      legend.position = "none",
      axis.title = element_text(size = 8), 
      axis.text.x = element_text(angle = 0, vjust = 1, hjust=1, size = 6),
      axis.text.y = element_text(size = 6),
      legend.title = element_text(size = 8),
      legend.text = element_text(size = 8)
    )
  
  return(dendogramPlot)
}


# Introducing a function to convert a list of groups with labeled entries into a list of list of labels

clusterConverter <- function(clustering) {
  
  clusters <- split(names(clustering), clustering)
    
  return(clusters)
}


# Introducing a function to generate tables comparing the risk metrics of clusters and their components 

clusterTableDataGenerator <- function(individualSimulationDataL, clusterSimulationDataL, isISO) {
  # 
  # individualSimulationData <- totalSeveritySimulationData
  # clusterSimulationData <- retainedClusterSeverityModels
  # isISO <- F
  # 
  # individualSimulationDataL <- totalSeveritySimulationData
  # clusterSimulationDataL <- group2ClusterTotalSeverityModels2Data
  # isISO <- T
  
    
  table <- clusterSimulationDataL %>%
    unnest_longer(cluster) %>%
    rename(Country = cluster) %>%
    dplyr::select(ID, Country, valueAtRisk, expectedShortfall) %>%
    rename(CountryName = Country)
  

  
  if(isISO) {
    table <- table %>%
      rename(ISO = CountryName) %>%
      rowwise() %>% 
      mutate(Country = pull(filter(relevantCountries, CountryISO == ISO), Country)) 
  }
  
  if(! isISO) {
    table <- table %>%
      rowwise() %>% 
      mutate(ISO = pull(filter(relevantCountries, Country == CountryName), CountryISO)) %>%
      rename(Country = CountryName)
  }
  
  table <- table %>% 
    rename(clusterVaR = valueAtRisk, 
           clusterES = expectedShortfall) %>%
    rename(CountryTab = Country) %>%
    rowwise() %>%
    mutate(sample = list(pull(filter(individualSimulationDataL, Country == CountryTab), simulation2020Euros))) %>%
    mutate(countryVaR = valueAtRisk(unlist(sample)),
           countryES = expectedShortfall(unlist(sample)))
  
  table <- table %>% 
    mutate(ID = paste0("BaliseHLine", ID))
  
  table <- table %>% 
    mutate(countryVaR = countryVaR /1e06,
           clusterVaR = clusterVaR /1e06,
           countryES = countryES /1e06,
           clusterES = clusterES /1e06)
  
  # Formatting for the generation of the LaTeX table
  
  clusterRows <- table %>%
    group_by(ID) %>%
    group_split()
  
  finalTable <- purrr::map_dfr(clusterRows, function(clusterDataFrame) {
    clusterID <- unique(clusterDataFrame$ID)
    clusterVaR <- unique(clusterDataFrame$clusterVaR)
    clusterES <- unique(clusterDataFrame$clusterES)
    
    clusterDataFrame <- clusterDataFrame %>%
      mutate(
        Cluster = if_else(row_number() == 1, as.character(clusterID), ""),
        clusterVaR = NA_real_,
        clusterES = NA_real_
      ) %>%
      dplyr::select(Cluster, Country = CountryTab, ISO, countryVaR, clusterVaR, countryES, clusterES)
    
    summaryRows <- tibble(
      Cluster = paste0("Total Cluster ", clusterID),
      Country = NA_character_,
      ISO = NA_character_,
      countryVaR = sum(clusterDataFrame$countryVaR, na.rm = TRUE),
      clusterVaR = clusterVaR,
      countryES = sum(clusterDataFrame$countryES, na.rm = TRUE),
      clusterES = clusterES
    )
    
    bind_rows(clusterDataFrame, summaryRows)
  })
  
  totalSummaryRow <- finalTable %>%
    filter(grepl("^Total Cluster", Cluster)) %>%
    summarise(
      Cluster = "Total All Clusters",
      Country = NA_character_,
      ISO = NA_character_,
      countryVaR = sum(countryVaR, na.rm = TRUE),
      clusterVaR = sum(clusterVaR, na.rm = TRUE),
      countryES = sum(countryES, na.rm = TRUE),
      clusterES = sum(clusterES, na.rm = TRUE)
    )
  
  finalTable <- bind_rows(finalTable, totalSummaryRow)
  
  return(finalTable)
}


clusterTableLaTeXGenerator <- function(tableData, title) {
  finalTable <- tableData
  
  
  finalTable <- finalTable %>%
    mutate(
      Cluster = as.character(Cluster),
      Country = as.character(Country),
      ISO = as.character(ISO),
      countryVaR = as.character(round(countryVaR, 2)),
      clusterVaR = as.character(round(clusterVaR, 2)),
      countryES = as.character(round(countryES, 2)),
      clusterES = as.character(round(clusterES, 2))
    ) %>%
    replace_na(list(
      Cluster = "",
      Country = "",
      ISO = "",
      countryVaR = "",
      clusterVaR = "",
      countryES = "",
      clusterES = ""
    ))
  
  finalTable <- finalTable %>%
    dplyr::select(-Country) %>%
    rename(
      '{\\begin{tabular}[c]{r}  Cluster \\\\ \\hspace{1pt} \\end{tabular} \\hspace{-10pt}} ' = Cluster,
      '{\\begin{tabular}[c]{r}  Country \\\\ \\hspace{1pt} \\end{tabular} \\hspace{-10pt}} ' = ISO,
      '{\\begin{tabular}[c]{r}$\\text{VaR}_{99.5 \\%}$ \\\\ Individual\\end{tabular} \\hspace{-10pt}} ' = countryVaR,
      # '$\\text{VaR}_{99.5 \\%}$:  Individual' = countryVaR,
      '{\\begin{tabular}[c]{r}  \\\\ Cluster\\end{tabular} \\hspace{-10pt}} ' = clusterVaR,
      # 'Cluster ' = clusterVaR,
      '{\\begin{tabular}[c]{r}$\\text{ES}_{99 \\%}$ \\\\ Individual\\end{tabular} \\hspace{-10pt}} ' = countryES,
      # '$\\text{ES}_{99 \\%}$:  Individual' = countryES,
      # 'ClusterB' = clusterES,
      '{\\begin{tabular}[c]{r}  \\\\ ClusterB\\end{tabular} \\hspace{-10pt}} ' = clusterES,
    )
  
  
  
  finalTable <- finalTable %>%
    kable(format = "latex", booktabs = TRUE, escape = FALSE, align = "llrrrr",
          caption = title)
  
  finalTable <- gsub("\\\\toprule", "\\\\hline", finalTable)
  finalTable <- gsub("\\\\midrule", "", finalTable)
  finalTable <- gsub("\\\\bottomrule", "\\\\hline", finalTable)
  
  finalTable <- gsub("\\\\addlinespace", "", finalTable)
  
  finalTable <- gsub("Total Cluster BaliseHLine", "Total ", finalTable)
  
  finalTable <- gsub("BaliseHLine", "\\\\hline ", finalTable)
  
  finalTable <- gsub("Total All Clusters", "\\\\hline Total", finalTable)
  
  finalTable <- gsub("ClusterB", "Cluster", finalTable)
  
  return(finalTable )
}
