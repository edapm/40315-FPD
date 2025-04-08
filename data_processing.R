library(tidyverse)
library(sf)
library(maptiles)
library(tidyterra)

rm(list = ls())
setwd("~/Documents/University/Geography Degree/Year 1/Semester 2/40315 LC Fieldwork Project Design (FPD)/Assessment/Data Analysis")
file <- read.csv("Data.csv", header = TRUE, sep = ",")
data <- tibble(file)

# Convert DateTime to a date-time object -> year-month-day, hh:mm:ss timezone
data$DateTime <- as.POSIXct(data$DateTime, format = "%Y-%m-%d %H:%M:%S", tz = "Europe/Amsterdam")

# Split Coordinates column into lat long
data <- data %>%
  separate(Coordinates, into = c("Latitude", "Longitude"), sep = ",") %>%
  mutate(Latitude = as.numeric(Latitude),
         Longitude = as.numeric(Longitude))

limits <- read.csv("limits.csv", header = FALSE, sep=",")
basemap_limits <- st_as_sf(limits, coords=c("V1","V2"), crs=4326)

points <- st_as_sf(data, coords = c("Longitude","Latitude"), crs=4326)
basemap <- get_tiles(basemap_limits, provider="OpenStreetMap", zoom = 16)

ggplot() +
  geom_spatraster_rgb(data = basemap) +
  geom_sf(data = points, colour = "red", size = 1) +
  coord_sf(crs=4326) + 
  labs(title = "Sampling Locations", caption = "\U00a9 OpenStreetMap contributors") + 
  theme_void()
