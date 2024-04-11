# STAT 3355 Project
library(ggplot2)
library(ggmap)
library(dplyr)
library(ggdensity)
arrest <- read_csv(file = "/Users/mihuynh/Downloads/Police_Arrests 2.csv")
texas <- read.csv(file = "/Users/mihuynh/Downloads/map_texas.csv")

arrest$Longitude <- as.numeric(arrest$Longitude)

arrest$Longitude <- arrest %>%
  filter(Longitude > -97.1 & Longitude < -96.3)

register_stadiamaps("3089c9b5-aab3-4baf-bf18-e2af14cf8615", write = FALSE)

dallas_map <- get_stadiamap(
  bbox = c(left = -97.1, bottom = 32.6, right = -96.4, top = 33),
  zoom = 11,
  maptype = "stamen_terrain"
)

unique(arrest$Longitude)

arrest <- na.omit(arrest)


ggmap(dallas_map) +
  geom_hdr_points(data = arrest, aes(x = Longitude, y = Latitude, color = "red"), stat = "sum", na.rm = TRUE, position = "identity", show.legend = NA) +
  theme_minimal()




