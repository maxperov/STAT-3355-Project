# STAT 3355 Project
install.packages("lubridate")
install.packages("ggplot", dependencies = TRUE)
library(ggplot2)
library(ggmap)
library(dplyr)
library(ggdensity)
library(lubridate)
arrest <- read.csv(file = "/Users/mihuynh/Downloads/Police_Arrests 2.csv")
chicago <- read.csv(file = "/Users/mihuynh/Downloads/crimeChicago.csv")
arrest$Longitude <- as.numeric(arrest$Longitude)
dallas <- arrest %>%
  filter(Longitude >= -97.3303 & Longitude <= -96.2287)


register_stadiamaps("3089c9b5-aab3-4baf-bf18-e2af14cf8615", write = FALSE)

dallas_map <- get_stadiamap(
  bbox = c(left = -97.1, bottom = 32.6, right = -96.45, top = 33.05),
  zoom = 11,
  maptype = "stamen_terrain"
)

chicago_map <- get_stadiamap(
  bbox = c(left = -87.9, bottom = 41.6, right = -87.4, top = 42.05), 
  zoom = 11, 
  maptype = "stamen_terrain")

# Cleaning up Chicago and Dallas data
chicago <- na.omit(chicago)
chicago_arrested <- unique(chicago_arrested)
dallas <- na.omit(dallas)
dallas <- unique(dallas)
chicago_arrested <- subset(chicago, chicago$Arrest == TRUE)


# Trying to map Chicago and Dallas crime density maps
ggmap(chicago_map) +
  geom_density_2d(data = chicago_arrested, mapping = aes(x = Longitude, y = Latitude), na.rm = TRUE, alpha = 0.7)

ggmap(dallas_map) +
  geom_density2d(data = dallas, mapping = aes(x = Longitude, y = Latitude), na.rm = TRUE)
