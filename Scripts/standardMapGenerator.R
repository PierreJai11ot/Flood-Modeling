# Introducing a standardized map generator function for metrics on the national scale 

nationalMapGenerator <- function(data, variableName, titleLabel, metricLabel, mapData = baseMapData2021, rounding = 3, format = "g") {
  
  variableSymbol <- sym(variableName)
  
  dataEdited <- data %>% 
    dplyr::select(CountryISO, !!variableSymbol) %>% 
    rename(toBePlotted = !!variableSymbol)
  
  mapData <- mapData %>%
    filter(LEVL_CODE == 3) %>%
    rename(CountryISO = CNTR_CODE) %>%
    group_by(CountryISO) %>%
    summarise(geometry = st_union(geometry), .groups = "drop") %>%
    left_join(dataEdited, by = "CountryISO") %>%
    mutate(
      longitude = st_coordinates(st_centroid(geometry))[, 1],
      latitude = st_coordinates(st_centroid(geometry))[, 2]
    )
  
  map <- ggplot(mapData) +
    geom_sf(aes(fill = toBePlotted), color = "black", size = 0.2) +
    scale_fill_viridis_c(option = "turbo", na.value = "grey90", name = metricLabel, 
                         guide = guide_colorbar(barwidth = unit(10, "cm"), barheight = unit(0.5, "cm"))
                         ) +
    geom_shadowtext(aes(x = longitude, y = latitude, 
                        label = ifelse(is.na(toBePlotted), NA, formatC(toBePlotted, format = "g", digits = rounding))),
                    size = 3, fontface = "bold", 
                    color = "black", bg.color = "white", bg.r = 0.25) +
    labs(title = titleLabel, 
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
  
  return(map)
}
