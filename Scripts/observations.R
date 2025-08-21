install.packages("sessioninfo")
library(sessioninfo)

writeLines(capture.output(sessioninfo::session_info()), "session_info.txt")

test <- sessioninfo::session_info()

info <- capture.output(sessioninfo::session_info())
clean <- gsub("[\u2500-\u257F]", "-", info) 
writeLines(clean, "sessionInfoClean.txt")


length(test$packages$package)

dataVersion <- tibble(
  package1 = test$packages$package[1:50], 
  version1 = test$packages$loadedversion[1:50], 
  package2 = test$packages$package[51:100], 
  version2 = test$packages$loadedversion[51:100], 
  package3 = append(test$packages$package[101:149], " "),
  version3 = append(test$packages$loadedversion[101:149], " ")
  )

correlationCheck <- dataHANZE %>%
  group_by(CountryISO, Year) %>%
  summarize(occurences = dplyr::n(), 
            aggregateLoss = sum(normalizedLossTotalVulnerability))

countriesISOList <- sort(unique(dataHANZE$CountryISO))

allCountryComponent <- dataHANZE %>%
  group_by(Year) %>%
  summarize(occurences = dplyr::n(), 
            aggregateLoss = sum(`Losses (2020 euro)`)) %>%
  mutate(CountryISO = "AC") %>% 
  rowwise() %>% 
  mutate(normalizationFactor = sum(severityNormalizationDataHANZE %>%
                                  rename(Year2 = Year) %>%
                                  filter(Year2 == Year) %>%
                                  filter(CountryCode %in% countriesISOList) %>%
                                  pull(totalVulnerability))
         ) %>%
  rowwise() %>%
  mutate(aggregateLoss = aggregateLoss/normalizationFactor) %>%
  dplyr::select(-normalizationFactor)


newData <- bind_rows(correlationCheck, allCountryComponent) %>% 
  group_by(CountryISO) %>%
  complete(Year = 1950:2020, fill = list(occurences = 0, aggregateLoss = 0)) %>%
  ungroup()

correlationCheckTable <- tibble(
  Country = unique(newData$CountryISO)
)

correlationCheckTable <- correlationCheckTable %>% 
  rowwise() %>%
  mutate(
    frequencySample = list(newData %>%
                             filter(CountryISO == Country) %>%
                             pull(occurences)),
    severitySample = list(newData %>%
                            filter(CountryISO == Country) %>%
                            pull(aggregateLoss)),
    frequencySampleFiltered = list(unlist(frequencySample)[frequencySample != 0]), 
    severitySampleFiltered = list(unlist(severitySample)[severitySample != 0])) %>%
  rowwise() %>%
  mutate(corPearsWith = cor(frequencySample, severitySample, method = "pearson"),
         corSpearWith = cor(frequencySample, severitySample, method = "spearman"), 
         corPearsWithout = ifelse(length(unique(frequencySampleFiltered)) > 1, cor(frequencySampleFiltered, severitySampleFiltered, method = "pearson"), NA),
         corSpearWithout = ifelse(length(unique(frequencySampleFiltered)) > 1, cor(frequencySampleFiltered, severitySampleFiltered, method = "spearman"), NA))



  

newData %>%
  filter(CountryISO = Country) %>%
  pull(occurences)



# bernoulli mixture 

a2 <- simulationData  %>%
  # filter(Country != "Lithuania") %>%
  dplyr::select(- simulation, - Model.x, - Model.y, - simulation2020Euros) %>%
  mutate(varOverES = VaR2020Euros / ES2020Euros, 
         maxoverES = max2020Euros / ES2020Euros, 
         maxOverVaR = max2020Euros/ VaR2020Euros) %>%
  dplyr::select( - selectedModel.x, - selectedModel.y, - areaWeighedAverageVulnerability2020, -VaRSev, - ESSev, 
                )

a1 <- bernoulliTotalSeveritySimulationData %>%
  # filter(Country != "Lithuania") %>%
  dplyr::select(- simulation, - Model.x, - Model.y, - simulation2020Euros) %>%
  mutate(varOverES = VaR2020Euros / ES2020Euros, 
         maxoverES = max2020Euros / ES2020Euros, 
         maxOverVaR = max2020Euros/ VaR2020Euros) %>%
  dplyr::select( - selectedModel.x, - selectedModel.y, - areaWeighedAverageVulnerability2020, -VaRSev, - ESSev, 
                 - bernoulliSimulation, - simulationSevVector)

a1$VaR2020Euros / a2$VaR2020Euros
a1$ES2020Euros / a2$ES2020Euros

a1SANS <- a1 %>%
  filter(Country != "All Considered Countries")

a1AC <- a1 %>%
  filter(Country == "All Considered Countries")



cor(a1SANS$totalVulnerability2020, a1SANS$VaR2020Euros)
# 0.8318114
cor(a1SANS$totalVulnerability2020, a1SANS$ES2020Euros)
# 0.1504632
cor(a1SANS$max2020Euros, a1SANS$totalVulnerability2020)
# 0.7678662
cor(a1SANS$VaR2020Euros, a1SANS$max2020Euros)
# 0.9122758
cor(a1SANS$VaR2020Euros, a1SANS$ES2020Euros)
# 0.156472
cor(a1SANS$max2020Euros, a1SANS$ES2020Euros)
# 0.139177



max(a1SANS$varOverES)
# 1.000109
# this is finland, barely superior could be on account of monte carlo slow convergence 
# SO MOSTLY HEAVY TAIL DISTS AGAIN
min(a1SANS$varOverES)
# 0.001778917
# AGAIN ridiculous this is lithuania , all other countries reasonable 
mean(a1SANS$varOverES)
# 0.8017811
median(a1SANS$varOverES)
# 0.8687785
var(a1SANS$varOverES)
# 0.05013997


max(a1SANS$maxOverVaR)
# 2.407156
# AGAIN LARGE 
# max greater than var lithuania 2.40 spain 1.87 switzerland 1.23, poland 1.11, ireland and romania close to 1 , but all countries is 1.03, concerning 
# Seems Critical exceedance again 
# relatively small sample sizes 
# where should be once in 200 year events
# also here in case of all considered countries model 
# so perhaps not because of hydro-meteo intensity or other factor variation as theorized above 
# could be simply due to better reported high impact events over the whole sample
min(a1SANS$maxOverVaR)
# 0.4556309
# Seems much bordeline than in simple loss model case 
mean(a1SANS$maxOverVaR)
# 0.872469
# hard to tell if not conservative enough when it could be well reported exceptional events 
median(a1SANS$maxOverVaR)
# 0.74012
var(a1SANS$maxOverVaR)
# 0.2022142


a1AC$VaR2020Euros/sum(a1SANS$VaR2020Euros)
# 0.3103953
a1AC$ES2020Euros/sum(a1SANS$ES2020Euros)
# 0.126901
# As in ompoud model modest improvement 

# Largest VaR is all considered countries, however, 
# Larges ES is Lithuania then germany then All considered countries 






# GERMANY AND LITHUANIA HAVE HIGHER VAR AND ES 
#  large losses in one country are often offset by smaller losses (or no losses) in others.
# The “All Countries” model might have a higher event count, but many of those events are moderate, so the relative tail weight is reduced.
# could be recording biais 





a1 <- bernoulliTotalSeveritySimulationData %>%
  dplyr::select(- simulation, - Model.x, - Model.y, - simulation2020Euros)  %>%
  mutate(varOverES = VaR2020Euros / ES2020Euros, 
         maxOverVaR = max2020Euros/ VaR2020Euros)


a1SANS <- a1 %>%
  filter(Country != "All Considered Countries")

a1AC <- a1 %>%
  filter(Country == "All Considered Countries")


cor(a1SANS$totalVulnerability2020, a1SANS$VaR2020Euros)
cor(a1SANS$totalVulnerability2020, a1SANS$ES2020Euros)

cor(a1SANS$VaR2020Euros, a1SANS$max2020Euros)
cor(a1SANS$VaR2020Euros, a1SANS$ES2020Euros)
cor(a1SANS$max2020Euros, a1SANS$ES2020Euros)

mean(a1SANS$varOverES)
var(a1SANS$varOverES)

mean(a1SANS$maxOverVaR)
var(a1SANS$maxOverVaR)

sum(a1SANS$VaR2020Euros)/a1AC$VaR2020Euros
sum(a1SANS$ES2020Euros)/a1AC$ES2020Euros



a1 <- simulationData %>%
  dplyr::select(- simulation, - Model.x, - Model.y, - simulation2020Euros)  %>%
  mutate(varOverES = VaR2020Euros / ES2020Euros, 
         maxOverVaR = max2020Euros/ VaR2020Euros)

a1SANS <- a1 %>%
  filter(Country != "All Considered Countries")

a1AC <- a1 %>%
  filter(Country == "All Considered Countries")


cor(a1SANS$totalVulnerability2020, a1SANS$VaR2020Euros)
cor(a1SANS$totalVulnerability2020, a1SANS$ES2020Euros)

cor(a1SANS$VaR2020Euros, a1SANS$max2020Euros)
cor(a1SANS$VaR2020Euros, a1SANS$ES2020Euros)
cor(a1SANS$max2020Euros, a1SANS$ES2020Euros)

mean(a1SANS$varOverES)
var(a1SANS$varOverES)

mean(a1SANS$maxOverVaR)
var(a1SANS$maxOverVaR)

sum(a1SANS$VaR2020Euros)/a1AC$VaR2020Euros
sum(a1SANS$ES2020Euros)/a1AC$ES2020Euros




testDataHANZE <- dataHANZE$
