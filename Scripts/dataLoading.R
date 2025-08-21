# Data Loading - Reading the external data sets

# Set the path to the data directory

dataDirectory <- paste0(scriptDirectory, "/Data")


# Flood event data sets - Load flood event data sets from two candidate sources: EM-DAT and HANZE

# Read the EM-DAT flood event data set

rawDataEMDAT <- read_excel(paste0(dataDirectory, "/EMDAT/public_emdat_incl_hist_2024-09-16.xlsx"), sheet= "EM-DAT Data")


# Read the HANZE flood event data set

rawDataHANZE <- read_csv(paste0(dataDirectory, "/HANZE/HANZE database of historical flood impacts in Europe, 1870-2020 - V1.2.1/HANZE_events.csv"))


# Spatial base maps - Loading geometries for map plotting

# Loading the NUTS geometry 

baseMapData2010 <- eurostat::get_eurostat_geospatial(year="2010", resolution = "60")

baseMapData2021 <- eurostat::get_eurostat_geospatial(year="2021", resolution = "60")


# Flood loss normalization data sets - Loading the HANZE flood protection an vulnerability data set 

severityNormalizationDataHANZE <- st_read(paste0(dataDirectory, "/HANZE/Flood protection and vulnerability estimates for Europe, 1950-2020/NUTS3_Vulnerability/NUTS3_Vulnerability_Eco_loss.shp"))
