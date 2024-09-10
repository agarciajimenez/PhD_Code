
## Mapping Scott River sites ## ----

# Install packages
install.packages("leaflet.extras")
install.packages("webshot")

# Load packages

library(leaflet) 
library(leaflet.providers)
library(leaflet.extras)
library(tidyverse)
library(terra)
library(sf)
library(mapview)
library(webshot)
webshot::install_phantomjs()


## Load csv file with gps points ----

data <- read_csv("~/PhD_Code/Spatial_ATX/Raw_Data/SR_GPSpoints.csv")

# Load watershed shapefiles, This uses the sf package ----
## Scott

watershed_sr <- st_read("C:/Users/mhoud/OneDrive/Documents/PhD_Code/Spatial_ATX/Raw_Data/Watersheds Map/HU8_18010208_Watershed/HU8_18010208_Watershed/HU8_18010208_Watershed.shp")
watershed_sr <- st_transform(watershed_sr, "+proj=longlat +datum=WGS84")

## South Fork Eel

watershed_sfe <- st_read("C:/Users/mhoud/OneDrive/Documents/PhD_Code/Spatial_ATX/Raw_Data/Watersheds Map/HU8_18010106_Watershed/HU8_18010106_Watershed/HU8_18010106_Watershed.shp")
watershed_sfe <- st_transform(watershed_sfe, "+proj=longlat +datum=WGS84")

# Using leaflet, we can create an interactive map for this area ----

north_arrow_icon <- makeIcon(
  iconUrl = "https://upload.wikimedia.org/wikipedia/commons/7/79/Map_marker_icon_%E2%80%93_Nicolas_Mollet_%E2%80%93_North_%E2%80%93_Environmentalism_%E2%80%93_Symbols_%E2%80%93_Outlined_%E2%80%93_Black.svg",
  iconWidth = 20, iconHeight = 20
)

streams <- leaflet() %>% 
  addTiles() %>%
  addCircleMarkers(data = data, 
                   lat = ~Latitude,
                   lng = ~Longitude,
                   radius = 1,
                   color = "blue") %>%
  addPolygons(data = watershed_sr,
              color = "orange",
              fillColor = "gold",
              fillOpacity = 0.3,
              weight = 1.5) %>% 
  addScaleBar(position = "topleft")
  #addPolygons(data = watershed_sfe,
              #color = "blue",
              #fillColor = "lightblue",
              #fillOpacity = 0.75,
              #weight = 1.5)

streams

# Streams outline only includes the watershed shapefiles, not the GPS points
streams_outline <- leaflet() %>% 
  addProviderTiles("OpenStreetMap.HOT") %>% 
  addPolygons(data = watershed_sr,
              color = "orange",
              fillColor = "gold",
              fillOpacity = 0.3,
              weight = 1.5)

streams_outline
              
# Making the CA outline ----
## This is a static map created in ggplot

### Load shapefile
california <- st_read("~/PhD_Code/Spatial_ATX/Raw_Data/Watersheds Map/CA_State.shp")

# Use ggplot
ggplot() +
  geom_sf(data = california, color = "black", fill = "white") +   
  geom_sf(data = watershed_sr, fill = "orange", alpha = 0.5) +   
  coord_sf(crs = 4326) +   # Coordinate system
  theme_minimal()

ggsave("california_watershed_map.png", width = 10, height = 8, dpi = 300)
