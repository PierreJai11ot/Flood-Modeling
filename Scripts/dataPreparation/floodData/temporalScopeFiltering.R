# Older entries can be particularly subject to reporting biases
# Identifying an appropriate cutoff point to mitigate the impact of this bias


# Introducing a function to generate a moving average 

source(paste0(scriptDirectory, "/Scripts/dataPreparation/movingAverageFunction.R"))


# Drawing inspiration from similar articles, 1950 is a commonly used curoff point

proposedCutoffYear <- 1950 


# Generating plots to assess the suitability of the proposed cutoff year

# Generating said plot for EM-DAT

eventsPerYearGraphEMDAT <- dataEMDAT %>% 
  group_by(Year) %>% 
  arrange(Year) %>% 
  mutate(numberOfEvents = dplyr::n()) %>% 
  group_by(Year) %>% rowwise() %>%
  mutate( movingAverageNumberOfEvents = movingAverage(dataEMDAT, Year, 15)) %>% 
  ggplot(mapping = aes(x = Year )) + 
  geom_line( aes(x = Year , y = numberOfEvents, color = "Number of events")) + 
  geom_line( aes(x = Year , y = movingAverageNumberOfEvents, color = "15 years backward moving average number of events")) + 
  geom_smooth(mapping = aes(x = Year, y = numberOfEvents, color = "Smoothed number of events"), se = FALSE) +
  geom_vline(xintercept = proposedCutoffYear, color = "black", linetype = "dashed", linewidth = 1, alpha = 0.25) +
  labs(
    title = "EM-DAT",
    x = "Year",
    y = "Number of events", 
    color = " "
  ) +
  scale_color_manual(values = c("Number of events" = "grey","Smoothed number of events" = "blue", "15 years backward moving average number of events" = "red")) +  
  theme_minimal() +
  theme(
    aspect.ratio = 0.75,
    plot.title = element_text(hjust = 0.5, face ="bold", size = 10),
    plot.subtitle = element_text(hjust = 0.5),
    plot.caption = element_text(hjust = 0.5),
    legend.position = "bottom",
    axis.title = element_text(size = 8), 
    axis.text.x = element_text(angle = 0, vjust = 1, hjust=1, size = 6),
    axis.text.y = element_text(size = 6),
    legend.title = element_text(size = 8),
    legend.text = element_text(size = 8)
  )


# Generating said plot for HANZE

eventsPerYearGraphHANZE <- dataHANZE %>% 
  group_by(Year) %>% 
  arrange(Year) %>% 
  mutate(numberOfEvents = dplyr::n()) %>% 
  group_by(Year) %>% 
  rowwise() %>%
  mutate( movingAverageNumberOfEvents = movingAverage(dataHANZE, Year, 15)) %>% 
  ggplot(mapping = aes(x = Year )) + 
  geom_line( aes(x = Year , y = numberOfEvents, color = "Number of events")) + 
  geom_line( aes(x = Year , y = movingAverageNumberOfEvents, color = "15 years backward moving average number of events")) + 
  geom_smooth(mapping = aes(x = Year, y = numberOfEvents, color = "Smoothed number of events"), se = FALSE) +
  geom_vline(xintercept = proposedCutoffYear, color = "black", linetype = "dashed", linewidth = 1, alpha = 0.25) +
  labs(
    title = "HANZE",
    x = "Year",
    y = "Number of events", 
    color = " "
  ) +
  scale_color_manual(values = c("Number of events" = "grey","Smoothed number of events" = "blue", "15 years backward moving average number of events" = "red")) +  
  theme_minimal() +
  theme(
    aspect.ratio = 0.75,
    plot.title = element_text(hjust = 0.5, face ="bold", size = 10),
    plot.subtitle = element_text(hjust = 0.5),
    plot.caption = element_text(hjust = 0.5),
    legend.position = "bottom",
    axis.title = element_text(size = 8), 
    axis.text.x = element_text(angle = 0, vjust = 1, hjust=1, size = 6),
    axis.text.y = element_text(size = 6),
    legend.title = element_text(size = 8),
    legend.text = element_text(size = 8)
  )


# Combining the EM-DAT and HANZE plots 

eventsPerYearGraph <- (eventsPerYearGraphEMDAT | eventsPerYearGraphHANZE) + 
  plot_layout(ncol = 2, nrow = 1, guides = "collect") +
  plot_annotation(
    title = "Number of relevant recorded events per year per dataset",
    theme = theme(
      plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
      legend.position = "bottom"
    )
  )


# Storing the combined plots

ggsave(file.path(paste0(plotDirectory, "/Number of relevant recorded events per year per dataset.pdf")), width = 21, height = 11, units = "cm", plot = eventsPerYearGraph)

rm(proposedCutoffYear, eventsPerYearGraphEMDAT, eventsPerYearGraphHANZE, eventsPerYearGraph)


# It is difficult to draw conclusion from the graph above, as there seems to be a significant increase in the number of events

# Setting the chosen cutoff year

cutoffYear <- 1950


# Filtering entries preceding the cutoff year

entriesCount <- nrow(dataEMDAT)

dataEMDAT <- filter(dataEMDAT, dataEMDAT$Year >= cutoffYear )

message(paste("Out of the ", entriesCount, "flood-related entries of EM-DAT located in relevant countries ", nrow(dataEMDAT), " happen after ", cutoffYear))

entriesCount <- nrow(dataHANZE)

dataHANZE <- filter(dataHANZE, dataHANZE$Year >= cutoffYear )

message(paste("Out of the ", entriesCount, "flood-related entries of HANZE located in relevant countries ", nrow(dataHANZE), " happen after ", cutoffYear))

rm(entriesCount)

# Generating a tally to compare the data sets

tally <- countryTally(dataEMDAT, dataHANZE)
print(tally, n = Inf)

tallyLaTeX <- countryTallyLaTeX(dataEMDAT, dataHANZE)
writeLines(tallyLaTeX, file.path(paste0(scriptDirectory, "/Texts"), "Tally2.txt"))

rm(tally, tallyLaTeX, cutoffYear)
