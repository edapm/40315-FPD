library(tidyverse)
library(pastecs)
library(psych)
library(car)
library(sf)
library(maptiles)
library(tidyterra)
# library(plotly)

rm(list = ls())

data <- tibble(read_csv("Data.csv", col_names = TRUE))

# Convert DateTime to a date-time object -> year-month-day, hh:mm:ss timezone
data$DateTime <- as.POSIXct(data$DateTime, format = "%Y-%m-%d %H:%M:%S", tz = "Europe/Amsterdam")

# Split Coordinates column into lat long
data <- data %>%
  separate(Coordinates, into = c("Latitude", "Longitude"), sep = ",") %>%
  mutate(Latitude = as.numeric(Latitude),
         Longitude = as.numeric(Longitude))

# Turn into geometry for ggplot and sf
data <- st_as_sf(data, coords = c("Longitude","Latitude"), crs=4326)

# Plot Sampling Locations with raster map
limits <- read.csv("limits.csv", header = FALSE, sep=",")
basemap_limits <- st_as_sf(limits, coords=c("V1","V2"), crs=4326)
basemap <- get_tiles(basemap_limits, provider="CartoDB.Positron", zoom = 16)

ggplot() +
  geom_spatraster_rgb(data = basemap) +
  geom_sf(data = data, show.legend = TRUE, size = 1.25, aes(colour = SiteName)) +
  guides(color=guide_legend(title = "Site")) + 
  coord_sf() + 
  labs(caption = "\U00a9 OpenStreetMap contributors \U00a9 CARTO") + 
  theme_void()

# Calc Averages
# Ammonium
data <- data |>
  dplyr::rowwise() |>
  dplyr::mutate(Ammonium_Mean = mean(c(Ammonium1, Ammonium2, Ammonium3), na.rm=TRUE)) |>
  dplyr::ungroup() |>
  dplyr::relocate(Ammonium_Mean, .after = Ammonium3)

# Phosphate
data <- data |>
  dplyr::rowwise() |>
  dplyr::mutate(Phosphate_Mean = mean(c(Phosphate1, Phosphate2, Phosphate3), na.rm=TRUE)) |>
  dplyr::ungroup() |>
  dplyr::relocate(Phosphate_Mean, .after = Phosphate3)

# Dissolved_Ox
data <- data |>
  dplyr::rowwise() |>
  dplyr::mutate(Dissolved_Ox_Mean = mean(c(Dissolved_Ox1, Dissolved_Ox2, Dissolved_Ox3, Dissolved_Ox4), na.rm=TRUE)) |>
  dplyr::ungroup() |>
  dplyr::relocate(Dissolved_Ox_Mean, .after = Dissolved_Ox4)

# Water_EC
data <- data |>
  dplyr::rowwise() |>
  dplyr::mutate(Water_EC_Mean = mean(c(Water_EC1, Water_EC2, Water_EC3, Water_EC4), na.rm=TRUE)) |>
  dplyr::ungroup() |>
  dplyr::relocate(Water_EC_Mean, .after = Water_EC4)

# Water_TDS
data <- data |>
  dplyr::rowwise() |>
  dplyr::mutate(Water_TDS_Mean = mean(c(Water_TDS1, Water_TDS2, Water_TDS3, Water_TDS4), na.rm=TRUE)) |>
  dplyr::ungroup() |>
  dplyr::relocate(Water_TDS_Mean, .after = Water_TDS4)

# Water_PH
data <- data |>
  dplyr::rowwise() |>
  dplyr::mutate(Water_PH_Mean = mean(c(Water_PH1, Water_PH2, Water_PH3, Water_PH4), na.rm=TRUE)) |>
  dplyr::ungroup() |>
  dplyr::relocate(Water_PH_Mean, .after = Water_PH4)

# Soil_EC
data <- data |>
  dplyr::rowwise() |>
  dplyr::mutate(Soil_EC_Mean = mean(c(Soil_EC1, Soil_EC2, Soil_EC3, Soil_EC4), na.rm=TRUE)) |>
  dplyr::ungroup() |>
  dplyr::relocate(Soil_EC_Mean, .after = Soil_EC4)

# Soil_TDS
data <- data |>
  dplyr::rowwise() |>
  dplyr::mutate(Soil_TDS_Mean = mean(c(Soil_TDS1, Soil_TDS2, Soil_TDS3, Soil_TDS4), na.rm=TRUE)) |>
  dplyr::ungroup() |>
  dplyr::relocate(Soil_TDS_Mean, .after = Soil_TDS4)

# Soil_PH
data <- data |>
  dplyr::rowwise() |>
  dplyr::mutate(Soil_PH_Mean = mean(c(Soil_PH1, Soil_PH2, Soil_PH3, Soil_PH4), na.rm=TRUE)) |>
  dplyr::ungroup() |>
  dplyr::relocate(Soil_PH_Mean, .after = Soil_PH4)

# Data Analysis
# Ammonium
a_recvalue = 2
ggplot(data, aes(SiteName, Ammonium_Mean)) +
  geom_point() +
  geom_hline(yintercept = a_recvalue) + 
  geom_text(aes(0,a_recvalue, label="Recommended Value < 2 mg/L", hjust=-0.05, vjust=1.25)) + 
  labs(x = "Site", y="Ammonium Concentration (mg/L)")

# Phosphate
p_recvalue = 0.1
ggplot(data, aes(SiteName, Phosphate_Mean)) +
  geom_point() +
  geom_hline(yintercept = p_recvalue) + 
  geom_text(aes(0,p_recvalue, label="Recommended Value < 0.1 mg/L", hjust=-0.05, vjust=1.25)) + 
  labs(x = "Site", y="Phosphate Concentration (mg/L)")

# Dissolved_Ox
do_recvalue = 10
ggplot(data, aes(SiteName, Dissolved_Ox_Mean)) +
  geom_point() + 
  geom_hline(yintercept=do_recvalue) + 
  geom_text(aes(0,do_recvalue, label="Recommended Value > 10 mg/L", hjust=-0.05, vjust=-1)) + 
  labs(x = "Site", y="Dissolved Oxygen Concentration (mg/L)")

# Water_EC and Soil_EC
ec_recvalue = 500
ggplot(data, aes(x=SiteName)) +
  geom_point(aes(y=Water_EC_Mean, shape="Water")) +
  geom_point(aes(y=Soil_EC_Mean, shape="Soil")) + 
  geom_hline(yintercept = ec_recvalue) + 
  geom_text(aes(0,ec_recvalue, label="Recommended Value < 500 µS", hjust=-0.05, vjust=1.25)) + 
  scale_y_continuous(name = "Electrical Conductivity (µS)") + 
  labs(x = "Site", shape = "")

# Water_TDS and Soil_TDS
ggplot(data, aes(x=SiteName)) +
  geom_point(aes(y=Water_TDS_Mean, shape="Water")) +
  geom_point(aes(y=Soil_TDS_Mean, shape="Soil")) + 
  scale_y_continuous(name = "Total Dissolved Solids (ppm)") + 
  labs(x = "Site", shape = "")

# Water_PH and Soil_PH
ggplot(data, aes(x=SiteName)) +
  geom_point(aes(y=Water_PH_Mean, shape="Water")) +
  geom_point(aes(y=Soil_PH_Mean, shape="Soil")) + 
  scale_y_continuous(name = "pH") + 
  labs(x = "Site", shape = "")

# Statistical Tests
# Ammonium vs Phosphate - Correlation
# H(null) = There is no correlation between avg. Amm. lvls and avg. Pho. lvls
# at the 95% significance level
# H(alt) = There is a correlation between avg. Amm. lvls and avg. Pho. lvls
hist(data$Ammonium_Mean)
hist(data$Phosphate_Mean)

ggplot(data, aes(x=Ammonium_Mean, y=Phosphate_Mean)) +
  geom_point(aes(color=SiteName)) + 
  geom_smooth(method = lm)

cor.test(data$Ammonium_Mean, data$Phosphate_Mean, method="spearman", exact=FALSE)

ggplot(data, aes(x=Ammonium_Mean, y=Dissolved_Ox_Mean)) + 
  geom_point(aes(color=SiteName)) + 
  geom_smooth(method = lm)

cor.test(data$Ammonium_Mean, data$Dissolved_Ox_Mean, method="spearman", exact=FALSE)

# plot_ly(x=data$Ammonium_Mean, y=data$Phosphate_Mean, z=data$Dissolved_Ox_Mean, type="scatter3d", mode="markers", color=data$SiteName)
