# Introducing a function to generate a tally of the number of entries per country per data set

countryTally <- function(dataEMDAT, dataHANZE) {
  
  tally <- relevantCountries %>% 
    rowwise()  %>% 
    mutate(EMDAT := sum(dataEMDAT$Country == Country))  %>% 
    mutate(largerThan = sum(dataEMDAT$Country == Country) > sum(dataHANZE$Country == Country))  %>% 
    mutate(HANZE := sum(dataHANZE$Country == Country))  %>% 
    ungroup()
  
  return (tally)
}


# Generating LaTeX code for the tally of the number of entries per country per data set

countryTallyLaTeX <- function(dataEMDAT, dataHANZE, redColoringCutoff = 0, batches = 2) {
  
  tally <- countryTally(dataEMDAT, dataHANZE)
  
  tallyLaTeX <- tally %>%
    rename(obsolete = Country) %>%
    rename(Country = CountryISO) %>%
    rowwise() %>%
    mutate(
      EMDAT = cell_spec(EMDAT, format = "latex", color = ifelse(EMDAT <= redColoringCutoff, "red", ifelse(largerThan, "darkgreen", "black"))),
      HANZE = cell_spec(HANZE, format = "latex", color = ifelse(HANZE <= redColoringCutoff, "red", ifelse(!largerThan, "darkgreen", "black")))
    ) %>%
    ungroup() %>%
    dplyr::select(Country, EMDAT, HANZE)
  
  length <- nrow(tallyLaTeX)
  batchSize <- ceiling(length / batches)
  
  batches <- c()
  
  for (i in seq(1, length, by = batchSize)) {
    currentBatch <- tallyLaTeX[i:min(i + batchSize - 1, length), ]
    if (i == 1 ){
      currentTable <- kable(t(currentBatch), format = "latex", booktabs = TRUE, caption = "Comparison of entry counts between candidate datasets", escape = FALSE)
      currentTable <- sub("\\\\bottomrule\\n\\\\end\\{tabular\\}\\n\\\\end\\{table\\}", "", currentTable)
      currentTable <- paste0(currentTable, "\\hline")
    }
    if (i!= 1 ) {
      currentTable <- kable(t(currentBatch), format = "latex", booktabs = TRUE, escape = FALSE)
      currentTable <- sub("\\\\bottomrule\\n\\\\end\\{tabular\\}", "", currentTable)
      currentTable <- sub(paste0("^\\n\\\\begin\\{tabular\\}\\{", paste(rep("l", (1+ nrow(currentBatch))), collapse =""), "\\}\\n\\\\toprule\\n"), "", currentTable)
      currentTable <- paste0(currentTable, "\\hline\n")
    }
    
    batches <- c(batches, currentTable)
  }
  

  tallyLaTeX <- paste(batches, collapse = " ")
  tallyLaTeX <- paste0(tallyLaTeX, "\n\\end{tabular}\n\\end{table}")
  
  tallyLaTeX <- gsub("\\\\toprule", "\\\\hline", tallyLaTeX)
  
  return(tallyLaTeX)
}

