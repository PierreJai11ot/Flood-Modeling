# Loss Normalization - Normalizing the most suitable loss metric

# In addition to adjusting for inflation, one can normalize the loss data in order for it to reflect a more robust evaluation of the severity of each flood event 

# Another data set related to HANZE contains relevant spatial data, including vulnerability and protection against floods

# In particular, one metric describes an estimation the loss occasioned by an average flood in a given region in the case where it is not protected

# A simple way to normalize the loss data would be to divide it by the national average of this metric weighed by the surface area of each region 


# Assigning the appropriate coordinate reference system to the severity normalization data set

severityNormalizationDataHANZE <- st_transform(severityNormalizationDataHANZE, crs = 3035)


# For the sake of simplicity, one can adapt the country codes of the NUTS regions into country labels that match that of the previous data sets

severityNormalizationDataHANZE <- severityNormalizationDataHANZE %>%
  mutate(CountryCode = substr(Code, 1, 2))  %>%
  relocate(CountryCode, .before = Code) %>%
  filter(CountryCode %in% relevantCountries$CountryISO) %>%
  mutate(Country = relevantCountries$Country[match(CountryCode, relevantCountries$CountryISO)]) %>% 
  relocate(Country, .before = CountryCode) 


# Two simple options appear for loss normalization on the national scale : an area weighed average or a sum of vulnerabilities

# Computing the area of each NUTS region as well as of each country 

severityNormalizationDataHANZE <- severityNormalizationDataHANZE %>% 
  mutate(regionArea = as.numeric(st_area(geometry))/ 1e6) %>% 
  group_by(Country, CountryCode) %>% 
  mutate(countryArea = sum(regionArea)) %>% 
  ungroup %>% 
  relocate(countryArea, .after = Code) %>% 
  relocate(regionArea, .after = countryArea)


# Generating a map visualizing the severity metric for an arbitrary year

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

baseMapData2010 <- baseMapData2010 %>%
  filter(!CNTR_CODE %in% excludedCountriesNutsMap) %>%
  filter(!NUTS_ID %in% excludedTerritoriesNutsMap)

rm(excludedCountriesNutsMap, excludedTerritoriesNutsMap, relevantCountriesNutsMap)


# Generating the basic severity map data

baseVulnerabilityMapData <- baseMapData2010 %>%
  rename( Code = NUTS_ID) %>%
  left_join(dplyr::select(st_drop_geometry(severityNormalizationDataHANZE), Code, Eco_2020), by = "Code") %>%
  rowwise()  %>%
  filter(LEVL_CODE == 3) %>%
  ungroup()  %>%
  mutate(
    longitude = st_coordinates(st_centroid(geometry))[, 1],
    latitude = st_coordinates(st_centroid(geometry))[, 2])


# Generating the corresponding map of severity per NUTS-3 region in 2020

baseVulnerabilityMap <- ggplot(baseVulnerabilityMapData) +
  geom_sf(aes(fill = Eco_2020), color = "black", size = 0.2) +
  scale_fill_viridis_c(option = "turbo", na.value = "grey90", name = "Economic vulnerability",
                       guide = guide_colorbar(barwidth = unit(10, "cm"), barheight = unit(0.5, "cm"))) +
  labs(title = "Estimated economic vulnerability per NUTS-3 in 2020", 
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

ggsave(file.path(mapDirectory, "Estimated economic vulnerability per NUTS-3 in 2020.pdf"), width = 21, height = 21, units = "cm", plot = baseVulnerabilityMap)

rm(baseVulnerabilityMapData, baseVulnerabilityMap)


# Dropping the geometry of the spatial data and computing the national area weighed average and the sum of this metric 

severityNormalizationDataHANZE <- st_drop_geometry(severityNormalizationDataHANZE) %>%
  pivot_longer(
    cols = starts_with("Eco_"), 
    names_to = "Year",
    names_prefix = "Eco_",
    values_to = "vulnerabilityEconomicLoss") %>%
  mutate(Year = as.integer(Year)) %>%
  group_by(Country, Year) %>%
  mutate(areaWeighedAverageVulnerability = sum(vulnerabilityEconomicLoss*regionArea)/countryArea, totalVulnerability = sum(vulnerabilityEconomicLoss))  %>%
  ungroup() %>%
  dplyr::select(Country, CountryCode, countryArea, Year, areaWeighedAverageVulnerability, totalVulnerability) %>% distinct()


# Generating the data of area weighed national average estimated economic vulnerability in 2020

areaWeighedAverageVulnerabilityMapData <- baseMapData2010 %>%
  rowwise()  %>%
  filter(LEVL_CODE == 3) %>%
  ungroup()  %>%
  rename( CountryCode = CNTR_CODE) %>%
  group_by(CountryCode) %>%  
  summarise(geometry = st_union(geometry)) %>%
  ungroup() %>%
  left_join(dplyr::select(st_drop_geometry(filter(severityNormalizationDataHANZE, Year == 2020)), CountryCode, areaWeighedAverageVulnerability), by = "CountryCode") %>%
  mutate( longitude = st_coordinates(st_centroid(geometry))[, 1],
          latitude = st_coordinates(st_centroid(geometry))[, 2])
    

# Generating the corresponding map of  area weighed national average estimated economic vulnerability in 2020

areaWeighedAverageVulnerabilityMap <- ggplot(areaWeighedAverageVulnerabilityMapData) +
  geom_sf(aes(fill = areaWeighedAverageVulnerability), color = "black", size = 0.2) +
  scale_fill_viridis_c(option = "turbo", na.value = "grey90", name = "Economic vulnerability",
                       guide = guide_colorbar(barwidth = unit(10, "cm"), barheight = unit(0.5, "cm"))) +
  geom_shadowtext(aes(x = longitude, y = latitude, label = round(areaWeighedAverageVulnerability,2)), 
                  size = 3, fontface = "bold", 
                  color = "black", bg.color = "white", bg.r = 0.25) +
  labs(title = "Area weighed national average estimated economic vulnerability in 2020", 
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

ggsave(file.path(mapDirectory, "Area weighed national average estimated economic vulnerability in 2020.pdf"), width = 21, height = 21, units = "cm", plot = areaWeighedAverageVulnerabilityMap)

rm(areaWeighedAverageVulnerabilityMapData, areaWeighedAverageVulnerabilityMap)


# Generating the data of total national estimated economic vulnerability in 2020

totalVulnerabilityMapData <- baseMapData2010 %>%
  rowwise()  %>%
  filter(LEVL_CODE == 3) %>%
  ungroup()  %>%
  rename( CountryCode = CNTR_CODE) %>%
  group_by(CountryCode) %>%  
  summarise(geometry = st_union(geometry)) %>%
  ungroup() %>%
  left_join(dplyr::select(st_drop_geometry(filter(severityNormalizationDataHANZE, Year == 2020)), CountryCode, totalVulnerability), by = "CountryCode") %>%
  mutate( longitude = st_coordinates(st_centroid(geometry))[, 1],
          latitude = st_coordinates(st_centroid(geometry))[, 2])


# Generating the corresponding map of total national estimated economic vulnerability in 2020

totalVulnerabilityMap <- ggplot(totalVulnerabilityMapData) +
  geom_sf(aes(fill = totalVulnerability), color = "black", size = 0.2) +
  scale_fill_viridis_c(option = "turbo", na.value = "grey90", name = "Economic vulnerability",                          
                       guide = guide_colorbar(barwidth = unit(10, "cm"), barheight = unit(0.5, "cm"))) +
  geom_shadowtext(aes(x = longitude, y = latitude, label = round(totalVulnerability, 0)), 
                  size = 3, fontface = "bold", 
                  color = "black", bg.color = "white", bg.r = 0.25) +
  labs(title = "Area weighed national average estimated economic vulnerability in 2020", 
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

ggsave(file.path(mapDirectory, "Total national estimated economic vulnerability in 2020.pdf"), width = 21, height = 21, units = "cm", plot = totalVulnerabilityMap)

rm(totalVulnerabilityMapData, totalVulnerabilityMap)


# Using the vulnerability data to normalize the losses registered in the HANZE data set

dataHANZE <- dataHANZE %>% 
  mutate(Year = as.integer(Year)) %>% 
  left_join(severityNormalizationDataHANZE, by = c("Country", "Year")) %>% 
  mutate(normalizedLossAreaWeighedAverageVulnerability = `Losses (2020 euro)`/areaWeighedAverageVulnerability, 
           normalizedLossTotalVulnerability = `Losses (2020 euro)`/totalVulnerability)

# rm(severityNormalizationDataHANZE)


