library(tidyverse)
library(knitr)
library(kableExtra)
library(ggthemes)
library(leaflet)
library(mapview)
places <- read.csv("../place_labels.csv")
sites <- read_csv("../Howard_2019_ESCO_Data.csv") %>% 
  select(longitude, latitude, trad_LOI, bottom_K) %>% 
  drop_na(longitude)

strips_only <- sites %>% 
  drop_na(bottom_K)


map <- leaflet(options = leafletOptions(zoomControl = FALSE, 
                                        attributionControl = FALSE)) %>% 
  addProviderTiles(providers$Esri.WorldImagery) %>% # add basemap
  setView(lng = -81.1, lat = 24.9, zoom = 9) %>% # focus map in a certain area / zoom level
  addMiniMap(tiles = providers$Esri.WorldStreetMap, # add inset map
             position = 'topleft', 
             width = 200, height = 150,
             toggleDisplay = FALSE) %>% 
  addCircleMarkers(data = strips_only, ~longitude, ~latitude,
                   weight = 1.5,
                   radius = 7, 
                   fillColor = "blue",
                   fillOpacity = 1, 
                   stroke = T) %>% 
  addCircleMarkers(data = sites, ~longitude, ~latitude,
                   weight = 1.5,
                   fillColor = "red",
                   radius = 4, 
                   fillOpacity = 1, 
                   stroke = T) %>% 
  addLegend("bottomright", 
            colors =c("red",  "blue"),
            labels= c("Study sites","Canvas strips recovered"),
            opacity = 0.3) %>% 
  addLabelOnlyMarkers(~long, ~lat, label =  ~as.character(label), 
                      data = places, 
                      labelOptions = labelOptions(noHide = T, 
                                                  textsize = "16px", 
                                                  opacity = 0.4,
                                                  direction = 'center', 
                                                  textOnly = FALSE))

map

mapshot(map, file = "OEM1_site_map.png", vwidth = 792,
        vheight = 544, zoom = 3 ) 
