# Model evaluation

# Introducing a function reformatting generated tables

source(paste0(scriptDirectory, "/Scripts/modelEvaluation/tableCleaner.R"))


# Gathering goodness of fit tests for the fitted models

# Frequency models

# Introducing a function corresponding to Pearson's Chi Squared test compatible with our custom distributions

source(paste0(scriptDirectory, "/Scripts/modelEvaluation/pearsonChiSquared.R"))


# Pearson Chi-squared test

# Generating the Pearson Chi-squared test table

frequencyPearsonChiSquared <- frequencyModels %>% 
  rowwise() %>%
  mutate(data = list(Poisson[[1]]$data)) %>% 
  mutate(poissonChi = list(pearsonChiSquaredTest(data, "Poisson", list(Poisson[[1]]$estimate)))) %>% 
  mutate(negativeBinomialChi = list(pearsonChiSquaredTest(data, "NegativeBinomial", list(NegativeBinomial[[1]]$estimate)))) %>%
  mutate(zeroInflatedPoissonChi = list(pearsonChiSquaredTest(data, "ZeroInflatedPoisson", list(ZeroInflatedPoisson[[1]]$coefficients)))) %>%
  mutate(zeroInflatedNegativeBinomialChi = list(pearsonChiSquaredTest(data, "ZeroInflatedNegativeBinomial", list(ZeroInflatedNegativeBinomial[[1]]$coefficients))))


chiSquaredStringFormatter <- function(statisticList, digits1 = 2, digits2 = 2) {
  chi <- as.double(unlist(statisticList[[1]]))
  p <- as.double(unlist(statisticList[[2]]))
  string <- paste0("(", formatC(chi, format = "e", digits = digits1), ",  ", formatC(p, format = "e", digits = digits2), ")")
  return(string)
}


frequencyPearsonChiSquared <- frequencyPearsonChiSquared %>% 
  dplyr::select(- Poisson, - ZeroInflatedPoisson, -NegativeBinomial, - ZeroInflatedNegativeBinomial, - Plots, - plotList, - data) %>%
  rename(Poisson = poissonChi, 
         'Zero Inflated Poisson' = zeroInflatedPoissonChi,
         'Negative Binomial' = negativeBinomialChi, 
         'Zero Inflated Negative Binomial' = zeroInflatedNegativeBinomialChi, 
         CountryName = Country) %>%
  mutate(Poisson = chiSquaredStringFormatter(Poisson), 
         `Zero Inflated Poisson` = chiSquaredStringFormatter(`Zero Inflated Poisson`),
         `Negative Binomial` = chiSquaredStringFormatter(`Negative Binomial`), 
         `Zero Inflated Negative Binomial` = chiSquaredStringFormatter(`Zero Inflated Negative Binomial`)) %>%
  mutate(CountryName = ifelse(CountryName == "All Considered Countries", "AC", filter(relevantCountries, Country == CountryName)$CountryISO[1])) %>%
  rename(Country = CountryName, 
         PO = Poisson,
         ZP = 'Zero Inflated Poisson',
         NB = 'Negative Binomial',
         ZN = 'Zero Inflated Negative Binomial')

frequencyPearsonChiSquaredTitle <- "Chi-squared statistic and associated p-value of frequency models"
frequencyPearsonChiSquaredTable <- kable(frequencyPearsonChiSquared, format = "latex", booktabs = TRUE, caption = frequencyPearsonChiSquaredTitle, escape = FALSE)
frequencyPearsonChiSquaredTable <- tableCleaner(frequencyPearsonChiSquaredTable)

writeLines(frequencyPearsonChiSquaredTable, file.path(paste0(scriptDirectory, "/Texts"), paste0(frequencyPearsonChiSquaredTitle, ".txt")))

rm(frequencyPearsonChiSquared, frequencyPearsonChiSquaredTable, frequencyPearsonChiSquaredTitle, chiSquaredStringFormatter)

rm(dzinbinom, pzinbinom, qzinbinom, rzinbinom, dzipois, pzipois, qzipois, rzipois,
   logLikelihoodZinbinom, logLikelihoodZipois, mleZinbinom, mleZipois, momZinbinom, momZipois,
   zinbinomFitter, zipoisFitter)

rm(pearsonChiSquaredTest)

# Akaike's Information Criterion

# Generating Akaike's Information Criterion table

frequencyAIC <- frequencyModels %>% 
  rowwise() %>%
  mutate(PO = round(Poisson[[1]]$aic, 2), 
         ZP = round(ZeroInflatedPoisson[[1]]$AIC, 2),
         NB = round(NegativeBinomial[[1]]$aic, 2), 
         ZN = round(ZeroInflatedNegativeBinomial[[1]]$AIC, 2)) %>%
  dplyr::select(- Poisson, - ZeroInflatedPoisson, -NegativeBinomial, - ZeroInflatedNegativeBinomial, - Plots, - plotList) %>%
  rename(CountryName = Country) %>%
  mutate(CountryName = ifelse(CountryName == "All Considered Countries", "AC", filter(relevantCountries, Country == CountryName)$CountryISO[1])) %>%
  rename(Country = CountryName)

frequencyAICTitle <- "Akaike's information criterion of frequency models"
frequencyAICTable <- kable(frequencyAIC, format = "latex", booktabs = TRUE, caption = frequencyAICTitle, escape = FALSE)
frequencyAICTable <- tableCleaner(frequencyAICTable)

writeLines(frequencyAICTable, file.path(paste0(scriptDirectory, "/Texts"), paste0(frequencyAICTitle, ".txt")))

rm(frequencyAIC, frequencyAICTable, frequencyAICTitle)
  

# Bayesian Information Criterion

# Generating the Bayesian Information Criterion table

frequencyBIC <- frequencyModels %>% 
  rowwise() %>%
  mutate(PO = round(Poisson[[1]]$bic, 2), 
         ZP = round(ZeroInflatedPoisson[[1]]$BIC, 2),
         NB = round(NegativeBinomial[[1]]$bic, 2), 
         ZN = round(ZeroInflatedNegativeBinomial[[1]]$BIC, 2)) %>%
  dplyr::select(- Poisson, - ZeroInflatedPoisson, -NegativeBinomial, - ZeroInflatedNegativeBinomial, - Plots, - plotList) %>%
  rename(CountryName = Country) %>%
  mutate(CountryName = ifelse(CountryName == "All Considered Countries", "AC", filter(relevantCountries, Country == CountryName)$CountryISO[1])) %>%
  rename(Country = CountryName)

frequencyBICTitle <- "Bayesian information criterion of frequency models"
frequencyBICTable <- kable(frequencyBIC, format = "latex", booktabs = TRUE, caption = frequencyBICTitle, escape = FALSE)
frequencyBICTable <- tableCleaner(frequencyBICTable)

writeLines(frequencyBICTable, file.path(paste0(scriptDirectory, "/Texts"), paste0(frequencyBICTitle, ".txt")))

rm(frequencyBIC, frequencyBICTable, frequencyBICTitle)


# Severity and total severity models 

# Listing the continuous models

continousModels <- tibble(
  models = c(list(severityModels), list(totalSeverityModels)),
  tags = c("severity models", "total severity models")
)


# Generating the goodness of fit tables for both continuous models

for(i in 1:nrow(continousModels)) {
  
  # Kolmogorov-Smirnov 
  
  # Generating the Kolmogorov-Smirnov test table
  
  severityKS <- continousModels$models[[i]] %>% 
    rowwise() %>%
    mutate(LN = LogNormal$goodnessOfFits$ks,
           WE = Weibull$goodnessOfFits$ks,
           PA = Pareto$goodnessOfFits$Statistics$`Kolmogorov-Smirnov statistic`[[1]],
           TP = TruncatedPareto$goodnessOfFits$Statistics$`Kolmogorov-Smirnov statistic`[[1]]) %>% 
    dplyr::select(-LogNormal, -Weibull, -Pareto, -TruncatedPareto, -Plots, -plotList) %>%
    rename(CountryName = Country) %>%
    mutate(CountryName = ifelse(CountryName == "All Considered Countries", "AC", filter(relevantCountries, Country == CountryName)$CountryISO[1])) %>%
    rename(Country = CountryName)
  
  severityKSTitle <- paste0("Kolmogorov-Smirnov test of ", continousModels$tags[[i]])
  severityKSTable <- kable(severityKS, format = "latex", booktabs = TRUE, caption = severityKSTitle, escape = FALSE)
  severityKSTable <- tableCleaner(severityKSTable)
   
  writeLines(severityKSTable, file.path(paste0(scriptDirectory, "/Texts"), paste0(severityKSTitle, ".txt")))
  
  rm(severityKS, severityKSTable, severityKSTitle)
  
  
  # Cramer-von Mises statistic
  
  # Generating the Cramer-von Mises statistic table
  
  severityCVM <- continousModels$models[[i]] %>% 
    rowwise() %>%
    mutate(LN = LogNormal$goodnessOfFits$cvm,
           WE = Weibull$goodnessOfFits$cvm,
           PA = Pareto$goodnessOfFits$Statistics$`Cramer-von Mises statistic`[[1]],
           TP = TruncatedPareto$goodnessOfFits$Statistics$`Cramer-von Mises statistic`[[1]]) %>% 
    dplyr::select(-LogNormal, -Weibull, -Pareto, -TruncatedPareto, -Plots, -plotList) %>%
    rename(CountryName = Country) %>%
    mutate(CountryName = ifelse(CountryName == "All Considered Countries", "AC", filter(relevantCountries, Country == CountryName)$CountryISO[1])) %>%
    rename(Country = CountryName)
  
  severityCVMTitle <- paste0("Cramer-von Mises statistic of ", continousModels$tags[[i]])
  severityCVMTable <- kable(severityCVM, format = "latex", booktabs = TRUE, caption = severityCVMTitle, escape = FALSE)
  severityCVMTable <- tableCleaner(severityCVMTable)
  
  writeLines(severityCVMTable, file.path(paste0(scriptDirectory, "/Texts"), paste0(severityCVMTitle, ".txt")))
  
  rm(severityCVM, severityCVMTable, severityCVMTitle)
  
  
  # Anderson-Darling statistic
  
  # Generating the Anderson-Darling statistic table
  
  severityAD <- continousModels$models[[i]] %>% 
    rowwise() %>%
    mutate(LN = LogNormal$goodnessOfFits$ad,
           WE = Weibull$goodnessOfFits$ad,
           PA = Pareto$goodnessOfFits$Statistics$`Anderson-Darling statistic`[[1]],
           TP = TruncatedPareto$goodnessOfFits$Statistics$`Anderson-Darling statistic`[[1]]) %>% 
    dplyr::select(-LogNormal, -Weibull, -Pareto, -TruncatedPareto, -Plots, -plotList) %>%
    rename(CountryName = Country) %>%
    mutate(CountryName = ifelse(CountryName == "All Considered Countries", "AC", filter(relevantCountries, Country == CountryName)$CountryISO[1])) %>%
    rename(Country = CountryName)
  
  severityADTitle <- paste0("Anderson-Darling statistic of ", continousModels$tags[[i]])
  severityADTable <- kable(severityAD, format = "latex", booktabs = TRUE, caption = severityADTitle, escape = FALSE)
  severityADTable <- tableCleaner(severityADTable)
  
  writeLines(severityADTable, file.path(paste0(scriptDirectory, "/Texts"), paste0(severityADTitle, ".txt")))
  
  rm(severityAD, severityADTable, severityADTitle)
  
  
  # Akaike's Information Criterion
  
  # Generating Akaike's Information Criterion table
  
  severityAIC <- continousModels$models[[i]] %>% 
    rowwise() %>%
    mutate(LN = LogNormal$goodnessOfFits$aic,
           WE = Weibull$goodnessOfFits$aic,
           PA = Pareto$goodnessOfFits$Criteria$`Akaike's Information Criterion`[[1]],
           TP = TruncatedPareto$goodnessOfFits$Criteria$`Akaike's Information Criterion`[[1]]) %>% 
    dplyr::select(-LogNormal, -Weibull, -Pareto, -TruncatedPareto, -Plots, -plotList) %>%
    rename(CountryName = Country) %>%
    mutate(CountryName = ifelse(CountryName == "All Considered Countries", "AC", filter(relevantCountries, Country == CountryName)$CountryISO[1])) %>%
    rename(Country = CountryName)
  
  severityAICTitle <- paste0("Akaike's information criterion of ", continousModels$tags[[i]])
  severityAICTable <- kable(severityAIC, format = "latex", booktabs = TRUE, caption = severityAICTitle, escape = FALSE)
  severityAICTable <- tableCleaner(severityAICTable)
  
  writeLines(severityAICTable, file.path(paste0(scriptDirectory, "/Texts"), paste0(severityAICTitle, ".txt")))
  
  rm(severityAIC, severityAICTable, severityAICTitle)
  
  
  # Bayesian Information Criterion
  
  # Generating the Bayesian Information Criterion table
  
  severityBIC <- continousModels$models[[i]] %>% 
    rowwise() %>%
    mutate(LN = LogNormal$goodnessOfFits$bic,
           WE = Weibull$goodnessOfFits$bic,
           PA = Pareto$goodnessOfFits$Criteria$`Bayesian Information Criterion`[[1]],
           TP = TruncatedPareto$goodnessOfFits$Criteria$`Bayesian Information Criterion`[[1]]) %>% 
    dplyr::select(-LogNormal, -Weibull, -Pareto, -TruncatedPareto, -Plots, -plotList) %>%
    rename(CountryName = Country) %>%
    mutate(CountryName = ifelse(CountryName == "All Considered Countries", "AC", filter(relevantCountries, Country == CountryName)$CountryISO[1])) %>%
    rename(Country = CountryName)
  
  severityBICTitle <- paste0("Bayesian information criterion of ", continousModels$tags[[i]])
  severityBICTable <- kable(severityBIC, format = "latex", booktabs = TRUE, caption = severityBICTitle, escape = FALSE)
  severityBICTable <- tableCleaner(severityBICTable)
  
  writeLines(severityBICTable, file.path(paste0(scriptDirectory, "/Texts"), paste0(severityBICTitle, ".txt")))
  
  rm(severityBIC, severityBICTable, severityBICTitle)
  
}

rm(continousModels)

rm(i)

