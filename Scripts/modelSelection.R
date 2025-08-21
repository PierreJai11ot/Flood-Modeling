# Model selection

# Manually picking the chosen models through graphical analysis

# Selecting frequency models

frequencyModelSelection <- c(
  Austria = "Poisson", 
  Belgium = "Poisson", 
  Bulgaria = "NegativeBinomial", 
  Croatia = "NegativeBinomial", 
  Cyprus = NA, 
  'Czech Republic' = "ZeroInflatedPoisson", 
  Denmark = "Poisson",
  Estonia = NA, 
  Finland = "ZeroInflatedPoisson", 
  France = "ZeroInflatedPoisson", 
  Germany = "NegativeBinomial", 
  Greece = "ZeroInflatedPoisson", 
  Hungary = "Poisson", 
  Ireland = "ZeroInflatedPoisson", 
  Italy = "NegativeBinomial",
  Latvia = NA,
  Lithuania = "Poisson", 
  Luxembourg = NA, 
  Malta = NA, 
  Norway = "NegativeBinomial", 
  Poland = "Poisson", 
  Portugal = NA, 
  Romania = "NegativeBinomial", 
  Slovakia = "Poisson", 
  Slovenia = "Poisson", 
  Spain = "NegativeBinomial", 
  Sweden = "ZeroInflatedPoisson", 
  Switzerland = "NegativeBinomial", 
  'United Kingdom' = "NegativeBinomial",
  'All Considered Countries' = "NegativeBinomial"
)


# Applying the selection

frequencyModels <- frequencyModels %>%
  mutate(selectedModel = frequencyModelSelection[Country]) %>%
  pivot_longer(cols = c(Poisson, ZeroInflatedPoisson, NegativeBinomial, ZeroInflatedNegativeBinomial), names_to = "ModelType", values_to = "Model") %>%
  filter(ModelType == selectedModel) %>%
  dplyr::select(Country, selectedModel, Model)

rm(frequencyModelSelection)

# Selecting severity models

severityModelSelection <- c(
  Austria = "LogNormal", 
  Belgium = "LogNormal", 
  Bulgaria = "LogNormal",
  Croatia = "LogNormal",
  Cyprus = NA, 
  'Czech Republic' = "LogNormal", 
  Denmark = "TruncatedPareto",
  Estonia = NA, 
  Finland = "TruncatedPareto",
  France = "Weibull", 
  Germany = "LogNormal", 
  Greece = "Weibull", 
  Hungary = "TruncatedPareto",
  Ireland = "LogNormal",
  Italy = "LogNormal",
  Latvia = NA,
  Lithuania = "Pareto", 
  Luxembourg = NA, 
  Malta = NA, 
  Norway = "LogNormal", 
  Poland = "LogNormal", 
  Portugal = NA, 
  Romania = "Weibull", 
  Slovakia = "LogNormal", 
  Slovenia = "Weibull", 
  Spain = "LogNormal",
  Sweden = "LogNormal",
  Switzerland = "LogNormal", 
  'United Kingdom' = "LogNormal",
  'All Considered Countries' = "LogNormal"
)


# Applying the selection

severityModels <- severityModels %>%
  mutate(Country = as.character(Country)) %>%
  mutate(selectedModel = severityModelSelection[Country]) %>%
  pivot_longer(cols = c(LogNormal, Weibull, Pareto, TruncatedPareto), names_to = "ModelType", values_to = "Model") %>%
  filter(ModelType == selectedModel) %>%
  dplyr::select(Country, selectedModel, Model)

rm(severityModelSelection)


# Combining the frequency and severity models

combinedModels <- frequencyModels %>%
  left_join(severityModels, by = "Country")


# Selecting total severity models

totalSeverityModelSelection <- c(
  Austria = "LogNormal", 
  Belgium = "TruncatedPareto", 
  Bulgaria = "TruncatedPareto",
  Croatia = "LogNormal",
  Cyprus = NA, 
  'Czech Republic' = "TruncatedPareto", 
  Denmark = "TruncatedPareto",
  Estonia = NA, 
  Finland = "TruncatedPareto",
  France = "Weibull", 
  Germany = "LogNormal", 
  Greece = "Weibull", 
  Hungary = "Weibull",
  Ireland = "LogNormal",
  Italy = "Weibull",
  Latvia = NA,
  Lithuania = "Pareto",
  Luxembourg = NA, 
  Malta = NA, 
  Norway = "TruncatedPareto", 
  Poland = "Weibull", 
  Portugal = NA, 
  Romania = "Weibull", 
  Slovakia = "LogNormal", 
  Slovenia = "Weibull", 
  Spain = "LogNormal",
  Sweden = "TruncatedPareto",
  Switzerland = "LogNormal", 
  'United Kingdom' = "LogNormal",
  'All Considered Countries' = "Weibull"
)


# Applying the selection

totalSeverityModels <- totalSeverityModels %>%
  mutate(Country = as.character(Country)) %>%
  mutate(selectedModel = totalSeverityModelSelection[Country]) %>%
  pivot_longer(cols = c(LogNormal, Weibull, Pareto, TruncatedPareto), names_to = "ModelType", values_to = "Model") %>%
  filter(ModelType == selectedModel) %>%
  dplyr::select(Country, selectedModel, Model)

totalSeverityModels <- frequencyModels %>%
  left_join(totalSeverityModels, by = "Country")


rm(frequencyModels, severityModels)

rm(totalSeverityModelSelection)
