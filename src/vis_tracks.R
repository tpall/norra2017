
library(tidyverse)
library(stringr)
library(rgdal)
library(sp)
library(leaflet)
library(viridis)

gpx_files <- list.files("data/", full.names = TRUE)
tracks <- data_frame(gpx_files)
tracks <- mutate(tracks, date = str_extract(gpx_files, "[0-9]{4}_[0-9]{2}_[0-9]{2}"))
# List layers
ogrListLayers(tracks$gpx_files[1])
tracks <- mutate(tracks, track = map(gpx_files, readOGR, layer = "tracks"))
tracks

# Merge days distance
dtrk <- select(tracks, date, track) %>% 
  group_by(date) %>% 
  do(days_trk = as.list(.$track)) %>% 
  ungroup()

source("lib/rbind_SpatialLines_list.R")
dtrk <- mutate(dtrk, days_trk = map(days_trk, rbind.SpatialLines.list))

# Plot tracks
print_track <- function(trk) {
  leaflet() %>% addTiles() %>% addPolylines(data = trk)
  }

dtrk <- mutate(dtrk, trackonmap = map(days_trk, print_track))
trip <- data_frame(date = dtrk$date,
           trip = c("Geilo-Nedra Grøndalsvatne (Rallarvegen)",
                    "Nedra Grøndalsvatne-Voss-Dalavegen",
                    "Dalavegen-Stanghelle-Bergen (train)-Lyseklostervegen",
                    "Lyseklostervegen-Buavågen",
                    "Buavågen-Saudavegen",
                    "Saudavegen-Sauda-Håra",
                    "Håra-Odda-Utne",
                    "Kvanndal-Voss"))

dtrk <- left_join(dtrk, trip)

# Print more

plot_trip <- function(data, trip, popupdata) {
  m <- leaflet() %>%
    
    # Add tiles
    addTiles("https://{s}.tile.thunderforest.com/landscape/{z}/{x}/{y}.png?apikey=8eef623e4b534fc2a64f78cc99aa36eb", group = "Topographical") %>%
    addTiles("https://{s}.tile.thunderforest.com/cycle/{z}/{x}/{y}.png?apikey=8eef623e4b534fc2a64f78cc99aa36eb", group = "Cycle map") %>%
    addTiles("https://{s}.tile.thunderforest.com/outdoors/{z}/{x}/{y}.png?apikey=8eef623e4b534fc2a64f78cc99aa36eb", group = "Outdoors") %>%
    addProviderTiles("OpenStreetMap.Mapnik", group = "Road map") %>%
    addProviderTiles("Esri.WorldImagery", group = "Satellite") %>%
    
    # Add legend
    addLegend(position = 'bottomright',opacity = 0.4, 
              colors = 'blue', 
              labels = trip,
              title = 'Norway 2017')
    
  if(!is.null(popupdata)){
    
    # Add photo markers
    m <- m %>% addAwesomeMarkers(lng = popupdata$lon, 
               lat = popupdata$lat,
               popup = popupdata$popup,
               icon = icon_fa_camera,
               group = 'Photo markers')
  }
    
    
    # Layers control
    m %>% addLayersControl(position = 'bottomright',
                     baseGroups = c("Topographical", "Cycle map", "Outdoors", "Road map", "Satellite"),
                     overlayGroups = c("Biking routes", "Photo markers"),
                     options = layersControlOptions(collapsed = FALSE)) %>%
    
    addPolylines(data = data, color='blue', group = c('Biking routes','Photo markers')) 
}

dtrk <- mutate(dtrk, trip_leaf = pmap(list(days_trk, trip, popupdata), plot_trip))
dtrk$trip_leaf[[7]]

photos <- list.files("photos/", full.names = TRUE)
exif_location <- function(path) {
  # Read GPS locaton using exiftool
  exif_cmd <- paste("exiftool -c '%.6f'", path, "| grep 'GPS Position'")  
  location <- system(exif_cmd, intern = TRUE, ignore.stderr = TRUE) # execute exiftool-command
  location <- stringr::str_extract_all(location, "[0-9]+\\.[0-9]+") %>%
    unlist %>% 
    vapply(as.numeric, FUN.VALUE =  double(1), USE.NAMES = FALSE) 
  
  if(length(location)!=2){
    warning("Location data not found!")
    location <- c(NA, NA)
  }
  
  names(location) <- c("lat","lon")
  location <- c(path = path, location)
  return(location)
}

photos_location <- lapply(photos, exif_location)
photos_location <- do.call(rbind, photos_location) %>% 
  as_tibble() %>% 
  filter(complete.cases(.)) %>% 
  mutate_at(c("lat","lon"), as.numeric)
photos_location <- mutate(photos_location, 
                          date = str_extract(path, "[0-9]{8}"),
                          popup = sprintf("<img src='%s'/>", path))

icon_fa_camera <- makeAwesomeIcon(icon = 'camera', 
                           markerColor = 'red',
                           library='fa',
                           iconColor = 'black')
leaflet() %>% 
  addTiles() %>% 
  addAwesomeMarkers(lng=photos_location$lon, 
             lat=photos_location$lat, 
             popup = photos_location$popup, 
             icon = icon_fa_camera)

photos_location <- nest(photos_location, path, lat, lon, popup, .key = "popupdata")



