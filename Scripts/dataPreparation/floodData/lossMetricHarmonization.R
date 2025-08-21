# Surveying the different loss metrics available in the data sets 

# EM-DAT relevant loss related variables :
# "Total Damage ('000 US$)", "Insured Damage ('000 US$)", "Total Damage, Adjusted ('000 US$)", "Insured Damage, Adjusted ('000 US$)"
# Potentially relevant : "
# Reconstruction Costs ('000 US$)", , "Reconstruction Costs, Adjusted ('000 US$)"

lossMetricsEMDAT <- c("Total Damage ('000 US$)", "Insured Damage ('000 US$)", "Total Damage, Adjusted ('000 US$)", "Insured Damage, Adjusted ('000 US$)", 
                      "Reconstruction Costs ('000 US$)", "Reconstruction Costs, Adjusted ('000 US$)")

lossMetricsCountEMDAT <- sort(sapply(lossMetricsEMDAT, function(lossMetric) sum(!is.na(dataEMDAT[[lossMetric]]))), decreasing = T)

message(paste("The most represented loss metric in EM-DAT is", names(lossMetricsCountEMDAT[1]), " with ", lossMetricsCountEMDAT[1], " entries out of ", nrow(dataEMDAT)))

rm(lossMetricsEMDAT, lossMetricsCountEMDAT)

# HANZE relevant loss related variables : 
# "Losses (2020 euro)", "Losses (nominal value)", "Losses (original currency)"

lossMetricsHANZE <- c("Losses (2020 euro)", "Losses (nominal value)", "Losses (original currency)")

lossMetricsCountHANZE <- sort(sapply(lossMetricsHANZE, function(lossMetric) sum(!is.na(dataHANZE[[lossMetric]]))), decreasing = T)

message(paste("The most represented loss metric in HANZE is", names(lossMetricsCountHANZE[1]), " with ", lossMetricsCountHANZE[1], " entries out of ", nrow(dataHANZE)))

rm(lossMetricsHANZE, lossMetricsCountHANZE)

# Filtering out entries without the chosen loss metric

dataEMDAT <- dataEMDAT %>% filter(!is.na(`Total Damage ('000 US$)`))

dataHANZE <- dataHANZE %>% filter(!is.na(`Losses (2020 euro)`))

# Generating a tally to compare the data sets

tally <- countryTally(dataEMDAT, dataHANZE)
print(tally, n = Inf)

tallyLaTeX <- countryTallyLaTeX(dataEMDAT, dataHANZE)
writeLines(tallyLaTeX, file.path(paste0(scriptDirectory, "/Texts"), "Tally3.txt"))

rm(tally, tallyLaTeX)