library(tidyverse)
library(sf)
library(maptiles)
library(tidyterra)

rm(list = ls())

file <- read.csv("Data.csv", header = TRUE, sep = ",")
data <- tibble(file)

# Convert DateTime to a date-time object -> year-month-day, hh:mm:ss timezone
data$DateTime <- as.POSIXct(data$DateTime, format = "%Y-%m-%d %H:%M:%S", tz = "Europe/Amsterdam")

# Split Coordinates column into lat long
data <- data %>%
  separate(Coordinates, into = c("Latitude", "Longitude"), sep = ",") %>%
  mutate(Latitude = as.numeric(Latitude),
         Longitude = as.numeric(Longitude))

# Turn into geometry for ggplot and sf
data <- st_as_sf(data, coords = c("Longitude","Latitude"), crs=4326)

# data <- data |> separate_wider_delim(Ammonium, delim = ",", names = c("Ammonium1", "Ammonium2", "Ammonium3"))

# Plot Sampling Locations with raster map
limits <- read.csv("limits.csv", header = FALSE, sep=",")
basemap_limits <- st_as_sf(limits, coords=c("V1","V2"), crs=4326)
basemap <- get_tiles(basemap_limits, provider="CartoDB.Positron", zoom = 16)

ggplot() +
  geom_spatraster_rgb(data = basemap) +
  geom_sf(data = data, show.legend = TRUE, size = 1.25, aes(colour = Site)) +
  coord_sf() + 
  labs(title = "Sampling Locations", caption = "\U00a9 OpenStreetMap contributors") + 
  theme_void()