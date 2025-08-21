# Filter to flood-related events - Unlike the HANZE, EM-DAT includes other types of disasters outside of the scope of our study
# In the EM-DAT documentation, relevant `Disaster Type` and  `Disaster Subtype` labels seem to be "Flood", "Glacial lake outburst flood" and "Storm surge"

# Tracking the number of entries removed by the filter

entriesCount <- nrow(dataEMDAT)


# Specifying the list of relevant labels and applying the filter

relevantDisasterLabels <- c("Flood", "Glacial lake outburst flood", "Storm surge")

dataEMDAT <- filter(dataEMDAT, ((`Disaster Type` %in% relevantDisasterLabels) | (`Disaster Subtype` %in% relevantDisasterLabels)))

rm(relevantDisasterLabels)


# Consistency check - Verifying that only relevant `Disaster Type` and  `Disaster Subtype` remain after filtering

sort(unique(dataEMDAT$`Disaster Type`))

sort(unique(dataEMDAT$`Disaster Subtype`))


# Assessing the impact of the filter

message(paste("Out of the initial", entriesCount, "entries of EM-DAT", nrow(dataEMDAT), "are flood-related"))