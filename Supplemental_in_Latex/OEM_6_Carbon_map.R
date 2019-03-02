library(tidyverse)
library(knitr)
library(kableExtra)
library(ggthemes)
library(leaflet)
library(mapview)
places <- read.csv("../place_labels.csv")
df <- read_csv("../Howard_2019_ESCO_Data.csv")

df <- df %>% 
  mutate(Cdensity = df$trad_LOI*df$density/100*1000) %>% 
  select(longitude, latitude, Cdensity, sedimentscore_cata, sedimentscore_name)


rbPal <- colorRampPalette(c('blue','red'))
df$ColCorg <- rbPal(5)[as.numeric(cut(df$Cdensity,breaks = 5))]

cuts<-levels(cut(geo$Cdensity,breaks = 5))
cuts<-gsub(","," - ",cuts)
cuts<-gsub("\\(","[",cuts)


###
###map

map_Corg <- leaflet(options = leafletOptions(zoomControl = FALSE, 
                                             attributionControl = FALSE)) %>% # add  basemap
  addProviderTiles(providers$Esri.WorldImagery) %>% # focus map in a certain area / zoom level
  setView(lng = -81.1, lat = 24.9, zoom = 9) %>%
  addMiniMap(   # add inset map
    tiles = providers$Esri.WorldStreetMap,
    position = 'topleft', 
    width = 200, height = 150,
    toggleDisplay = FALSE) %>% 
  addCircleMarkers(data = df, ~longitude, ~latitude,
                   weight = 1.5,
                   radius = 7, 
                   fillColor = df$ColCorg,
                   fillOpacity = 1, 
                   stroke = T) %>% 
  
  addLegend("bottomright", 
            title = "Soil C<sub>org</sub> (mg cm<sup>-3</sup>)",
            colors =rbPal(5),
            labels= cuts,
            opacity = 1) 


map_Corg

mapshot(map_Corg, file = "OEM_6_Carbon_map_A.png", vwidth = 792,
        vheight = 544, zoom = 3 ) 

#map2
df$Col_sed <- (rev(rbPal(5)))[as.numeric(cut(df$sedimentscore_cata,breaks = c(0,1.1,2.1,3.1,4.1,5.1)))]

sed_cat <- rev(c("mud","sandy mud","muddy sand","sand","gravel"))


map_sed <- leaflet(options = leafletOptions(zoomControl = FALSE, 
                                            attributionControl = FALSE)) %>% 
  addProviderTiles(providers$Esri.WorldImagery) %>%
  setView(lng = -81.1, lat = 24.9, zoom = 9) %>%
  addMiniMap(
    tiles = providers$Esri.WorldStreetMap,
    position = 'topleft', 
    width = 200, height = 150,
    toggleDisplay = FALSE) %>% 
  addCircleMarkers(data = df, ~longitude, ~latitude,
                   weight = 1.5,
                   radius = 7, 
                   fillColor = df$Col_sed,
                   fillOpacity = 1, 
                   stroke = T) %>% 
  addLegend("bottomright", 
            title = "Sediment type",
            colors =rbPal(5),
            labels= df$sed_cat,
            opacity = 1) 


map_sed

mapshot(map_sed, file = "OEM_6_Carbon_map_B.png", vwidth = 792,
        vheight = 544, zoom = 3 ) 

