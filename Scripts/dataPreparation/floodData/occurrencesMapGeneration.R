# Visualization - Generating maps of the remaining occurrences

# Specifying a list of regions that are not to be plotted for the sake of the readability of the map

excludedCountriesNutsMap <- c("TR", "IS", "CY")

excludedTerritoriesNutsMap <- c("ES7",  "ES70",  "ES703", "ES704", "ES705", "ES706", "ES707", "ES708", "ES709", 
                                "FR910", "FR920", "FR930", "FR940", "FRY",   "FRY1",  "FRY10", "FRY2",  "FRY20", "FRY3",  "FRY30", "FRY4", "FRY40", "FRY5",  "FRY50", 
                                "GF",    "GL",    "MQ",
                                "NO0B1", "NO0B2", "NO0B", 
                                "PT2",   "PT20",  "PT200", "PT3",   "PT30",  "PT300",
                                "RE")


relevantCountriesNutsMap <- c("AT", "BE", "BG", "HR", "CZ", "DK", "FI", "FR", "DE", "GR", "HU", "IE", "IT", "LT", "PL", "RO", "SK", "SI", "ES", "SE", "NO", "CH", "UK")


# Loading NUTS geometries and filtering out excluded countries and territories 

baseMapData2021 <- baseMapData2021 %>%
  filter(!CNTR_CODE %in% excludedCountriesNutsMap) %>%
  filter(!NUTS_ID %in% excludedTerritoriesNutsMap)

rm(excludedCountriesNutsMap, excludedTerritoriesNutsMap, relevantCountriesNutsMap)


# Aggregating NUTS geometries at the country level and appending national flood occurrences

occurrenceNutsMapData <- baseMapData2021  %>%
  filter(LEVL_CODE == 3) %>% 
  group_by(CNTR_CODE) %>%  
  summarise(geometry = st_union(geometry)) %>%
  ungroup() %>%  
  rowwise() %>%  
  mutate(occurrences = ifelse(CNTR_CODE %in% relevantCountries$CountryISO, sum(dataHANZE$CountryISO == CNTR_CODE), NA) ) %>%  
  ungroup() %>%  
  mutate(longitude = st_coordinates(st_centroid(geometry))[, 1], 
         latitude = st_coordinates(st_centroid(geometry))[, 2]) 


# Generating the corresponding map of flood occurrences per country

occurrenceNutsMap <- ggplot(occurrenceNutsMapData) +
  geom_sf(aes(fill = occurrences), color = "black", size = 0.2) +
  geom_shadowtext(aes(x = longitude, y = latitude, label = occurrences), 
                  size = 3, fontface = "bold", 
                  color = "black", bg.color = "white", bg.r = 0.25) +
  scale_fill_viridis_c(option = "turbo", na.value = "grey90", name = "Recorded floods",
                       guide = guide_colorbar(barwidth = unit(10, "cm"), barheight = unit(0.5, "cm"))) +
  labs(title = "Recorded floods per country between 1950 and 2020", 
       # subtitle = "Placeholder",
       x = "Longitude",
       y = "Latitude"
  ) +
  theme_minimal() + 
  theme(
    aspect.ratio = 1,
    plot.title = element_text(hjust = 0.5, face ="bold", size = 10),
    plot.subtitle = element_text(hjust = 0.5),
    plot.caption = element_text(hjust = 0.5),
    axis.title = element_text(size = 8), 
    axis.text.x = element_text(angle = 0, vjust = 1, hjust=1, size = 6),
    axis.text.y = element_text(size = 6),
    legend.title = element_text(size = 8),
    legend.text = element_text(size = 8),
    legend.position = "bottom",
    legend.direction = "horizontal"
  )


# Storing the path to the maps directory

mapDirectory <- paste0(scriptDirectory, "/Maps")


# Storing the generated map

ggsave(file.path(mapDirectory, "Recorded floods per country between 1950 and 2020.pdf"), width = 21, height = 21, units = "cm", plot = occurrenceNutsMap)

rm(occurrenceNutsMapData, occurrenceNutsMap)


# Splitting the NUTS-3 regions affected by floods 

splitNuts3RegionsList <- strsplit(dataHANZE$`Regions affected (v2021)`, ";")
  

# Generating the NUTS-3 regional flood occurrences data 

relevantNuts3RegionsOccurrenceData <- tibble(
  NUTS_ID = baseMapData2021 %>%
    filter(LEVL_CODE == 3, CNTR_CODE %in% relevantCountries$CountryISO) %>%
    pull(NUTS_ID) %>%
    unique()
)

relevantNuts3RegionsOccurrenceData <- relevantNuts3RegionsOccurrenceData %>%
  rowwise() %>%
  mutate(occurrences = sum(sapply(splitNuts3RegionsList, function(regions) NUTS_ID %in% trimws(regions))))


# Aggregating NUTS-3 geometries and appending the corresponding regional flood occurrences

regionalOccurencesMapData <- baseMapData2021 %>%
  left_join(relevantNuts3RegionsOccurrenceData, by = "NUTS_ID") %>%
  rowwise()  %>%
  filter(LEVL_CODE == 3) %>%
  ungroup()  %>%
  mutate(
    longitude = st_coordinates(st_centroid(geometry))[, 1],
    latitude = st_coordinates(st_centroid(geometry))[, 2])


# Aggregating the NUTS-3 regions of countries outside of the scope of our study 

regionalOccurencesMapDataA <- regionalOccurencesMapData %>%
  filter(CNTR_CODE %in% relevantCountries$CountryISO)

regionalOccurencesMapDataB <- regionalOccurencesMapData %>%
  filter(! CNTR_CODE %in% relevantCountries$CountryISO) %>%
  group_by(CNTR_CODE) %>%
  summarise(geometry = st_union(geometry), occurrences = NA, .groups = "drop")


# Combining the edited geometries 

regionalOccurencesMapData <- bind_rows(regionalOccurencesMapDataA, regionalOccurencesMapDataB)


# Generating the corresponding map of flood occurrences per NUTS-3 region

regionalOccurrenceNutsMap <- ggplot(regionalOccurencesMapData) +
  geom_sf(aes(fill = occurrences), color = "black", size = 0.2) +
  scale_fill_viridis_c(option = "turbo", na.value = "grey90", name = "Recorded floods",
                       guide = guide_colorbar(barwidth = unit(10, "cm"), barheight = unit(0.5, "cm"))) +
  labs(title = "Recorded floods per NUTS-3 region between 1950 and 2020", 
       # subtitle = "Placeholder",
       x = "Longitude",
       y = "Latitude"
  ) +
  theme_minimal() + 
  theme(
    aspect.ratio = 1,
    plot.title = element_text(hjust = 0.5, face ="bold", size = 10),
    plot.subtitle = element_text(hjust = 0.5),
    plot.caption = element_text(hjust = 0.5),
    axis.title = element_text(size = 8), 
    axis.text.x = element_text(angle = 0, vjust = 1, hjust=1, size = 6),
    axis.text.y = element_text(size = 6),
    legend.title = element_text(size = 8),
    legend.text = element_text(size = 8),
    legend.position = "bottom",
    legend.direction = "horizontal"
  )

# Storing the generated map

ggsave(file.path(mapDirectory, "Recorded floods per NUTS-3 region between 1950 and 2020.pdf"), width = 21, height = 21, units = "cm", plot = regionalOccurrenceNutsMap)

rm(splitNuts3RegionsList, relevantNuts3RegionsOccurrenceData, regionalOccurencesMapDataA, regionalOccurencesMapDataB, regionalOccurencesMapData, regionalOccurrenceNutsMap)


# Aggregating NUTS geometries at the country level and appending the number of years with a positive recorded total flood loss

nonZeroYearsData <- dataHANZE %>% 
  group_by(Year, CountryISO) %>% 
  summarise(totalLoss = (sum(`Losses (2020 euro)`))) %>%
  ungroup() %>%
  group_by(CountryISO) %>% 
  summarise(entries = dplyr::n()) %>%
  right_join(relevantCountries, by = "CountryISO") %>%
  mutate(entries = replace_na(entries, 0))


nonZeroYearsNutsMapData <- baseMapData2021  %>%
  filter(LEVL_CODE == 3) %>% 
  group_by(CNTR_CODE) %>%  
  summarise(geometry = st_union(geometry)) %>%
  left_join(nonZeroYearsData, by = c("CNTR_CODE" = "CountryISO")) %>%
  mutate(longitude = st_coordinates(st_centroid(geometry))[, 1],
         latitude = st_coordinates(st_centroid(geometry))[, 2])


# Generating the corresponding map of flood occurrences per country

nonZeroYearsNutsMap <- ggplot(nonZeroYearsNutsMapData) +
  geom_sf(aes(fill = entries), color = "black", size = 0.2) +
  geom_shadowtext(aes(x = longitude, y = latitude, label = entries), 
                  size = 3, fontface = "bold", 
                  color = "black", bg.color = "white", bg.r = 0.25) +
  scale_fill_viridis_c(option = "turbo", na.value = "grey90", name = "Recorded floods",
                       guide = guide_colorbar(barwidth = unit(10, "cm"), barheight = unit(0.5, "cm"))) +
  labs(title = "Years with recorded floods per country between 1950 and 2020", 
       # subtitle = "Placeholder",
       x = "Longitude",
       y = "Latitude"
  ) +
  theme_minimal() + 
  theme(
    aspect.ratio = 1,
    plot.title = element_text(hjust = 0.5, face ="bold", size = 10),
    plot.subtitle = element_text(hjust = 0.5),
    plot.caption = element_text(hjust = 0.5),
    axis.title = element_text(size = 8), 
    axis.text.x = element_text(angle = 0, vjust = 1, hjust=1, size = 6),
    axis.text.y = element_text(size = 6),
    legend.title = element_text(size = 8),
    legend.text = element_text(size = 8),
    legend.position = "bottom",
    legend.direction = "horizontal"
  )


# Storing the generated map

ggsave(file.path(mapDirectory, "Number of years with a recorded flood loss per country between 1950 and 2020.pdf"), width = 21, height = 21, units = "cm", plot = nonZeroYearsNutsMap)

rm(nonZeroYearsNutsMap, nonZeroYearsNutsMapData, nonZeroYearsData)


# Generating a final tally table

tallyData <- dataHANZE %>% 
  group_by(CountryISO) %>% 
  summarise(entries = dplyr::n(), 
            years = n_distinct(Year))

redColoringCutoff <- 4

tallyLaTeX <- tallyData %>%
  rename(Country = CountryISO) %>%
  rename(Entries = entries) %>%
  rowwise() %>%
  mutate(
    Entries = cell_spec(Entries, format = "latex", color = ifelse(Entries <= redColoringCutoff, "red", "black")),
    years = cell_spec(years, format = "latex", color = ifelse(years <= redColoringCutoff, "red", "black"))
  ) %>%
  rename('Years with entries' = years) %>%
  ungroup() %>%
  dplyr::select(Country, Entries, 'Years with entries')

length <- nrow(tallyLaTeX)
batchSize <- ceiling(length / 2)

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

rm(i, currentTable)

# Storing the final tally table

writeLines(tallyLaTeX, file.path(paste0(scriptDirectory, "/Texts"), "TallyPostDataSelection.txt"))

rm(tallyData, redColoringCutoff, tallyLaTeX, length, batchSize, batches, currentBatch)
