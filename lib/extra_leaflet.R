
library(tidyverse)
library(stringr)

gpx_files <- list.files("data/", full.names = TRUE)

# Extract date from filename and import files as xml
tracks <- data_frame(gpx_files) %>% 
  mutate(date = str_extract(gpx_files, "[0-9]{4}_[0-9]{2}_[0-9]{2}"),
         date = str_replace_all(date, "_", ""))

tracks <- tracks %>% 
  group_by(date) %>% 
  do(gpx_files = as.list(.$gpx_files)) %>% 
  ungroup()

# Start and end points for days trips to be used in legend
tracks$trip  <- c("Geilo-Nedra Grøndalsvatne (Rallarvegen)",
                  "Nedra Grøndalsvatne-Voss-Dalavegen",
                  "Dalavegen-Stanghelle-Bergen (train)-Lyseklostervegen",
                  "Lyseklostervegen-Buavågen",
                  "Buavågen-Saudavegen",
                  "Saudavegen-Sauda-Håra",
                  "Håra-Odda-Utne",
                  "Kvanndal-Voss")

merge_gpx <- function(gpxfiles, writetofile = TRUE, pathtosave = "./", activity = "Cycling") {

  # Input files
  input <- paste("-i gpx -f", gpxfiles, collapse = " ")
  
  # Writes to stdout
  output <- "-"
  
  # Write to file
  if(writetofile){
    date <- str_extract(gpxfiles[[1]], "[0-9]{4}_[0-9]{2}_[0-9]{2}")
    output <- file.path(pathtosave, sprintf("Merged_%s_%s.gpx", date, activity))
  }
  
  # Compose gpsbabel command
  gpsbabel_cmd <- sprintf("gpsbabel -t %s -x track,merge,title='COMBINED LOG' -o gpx -F %s", input, output)
  
  # Do merging and output merged file name or gpx string of merged tracks
  gpxstring <- system(gpsbabel_cmd, intern = !writetofile, ignore.stderr = TRUE)
  
  # Parse output
  if(!writetofile){
    # Returns xml as character vector, collapse rows into single string
    gpxstring <- paste(gpxstring, collapse = "")
  } else {
    # Returns file name of merged data
    message(sprintf("Writing merged gpx track to %s", output))
    gpxstring <- output
  }
  
  return(gpxstring)
}

tracks <- mutate(tracks, gpxstring = map_chr(gpx_files, merge_gpx, writetofile = FALSE))

library(sp)
library(rgdal)
library(leaflet)
library(leaflet.extras)

gpx <- tracks$gpxstring[[4]]
plot_trip2 <- function(gpx, trip = NULL, popupdata = NULL){
  
  # Thunderforest base url
  thunderforest_link_template <- "https://{s}.tile.thunderforest.com/%s/{z}/{x}/{y}.png?apikey=%s"
  
  # Munge gpx data for boundingbox and plotting
  if(str_detect(gpx, "^\\<\\?xml")){
    # Select only first two cols to keep numeric
    bb <- xml2::as_xml_document(gpx) %>% 
      xml2::xml_ns_strip() %>% 
      xml2::xml_find_all("metadata/bounds") %>% 
      xml2::xml_attrs() %>% 
      unlist %>%
      vapply(as.numeric, numeric(1))
  } else {
    # if filename is argument
    bb <- rgdal::readOGR(gpx, layer = "tracks", verbose = FALSE)
    bb <- sp::bbox(bb)
    gpx <- readr::read_file(gpx)
  }
  
  # Compose map
  m <- leaflet() %>% 
    
    # Zoom map to your data
    fitBounds(lng1 = bb[[2]], lat1 = bb[[1]], lng2 = bb[[4]], lat2 = bb[[3]]) %>%
    
    # Add tiles
    addTiles(sprintf(thunderforest_link_template, "landscape", Sys.getenv("THUNDERF_APIKEY")), group = "Topograafiline") %>%
    addTiles(sprintf(thunderforest_link_template, "cycle", Sys.getenv("THUNDERF_APIKEY")), group = "Rattateed") %>%
    addTiles(sprintf(thunderforest_link_template, "outdoors", Sys.getenv("THUNDERF_APIKEY")), group = "Matka- ja terviserajad") %>%
    addProviderTiles("Stamen.Terrain", group = "Pinnavormid", options = providerTileOptions(detectRetina = T)) %>%
    addProviderTiles("OpenStreetMap.Mapnik", group = "Sõiduteed", options = providerTileOptions(detectRetina = T)) %>%
    addProviderTiles("Esri.WorldImagery", group = "Satelliit", options = providerTileOptions(detectRetina = T)) %>% 
    
    # Add data
    addGPX(gpx, fill = FALSE)
  
  # Add legend
  if(!is.null(trip)){
    m <- m %>% addLegend(position = 'bottomright', 
                         opacity = 0.4, 
                         colors = 'blue', 
                         labels = trip,
                         title = 'Norra 2017')}
  
  # Add photo markers
  if(!is.null(popupdata)){
    m <- m %>% addAwesomeMarkers(lng = popupdata$lon,
                                 lat = popupdata$lat,
                                 popup = popupdata$popup,
                                 popupOptions = popupOptions(closeButton = FALSE, 
                                                             maxWidth = 500,
                                                             closeOnClick = TRUE),
                                 icon = icon_fa_camera,
                                 group = 'Fotod')
  }
  
  # Layers control
  m %>% addLayersControl(position = 'bottomright',
                         baseGroups = c("Topograafiline", 
                                        "Sõiduteed", 
                                        "Rattateed", 
                                        "Matka- ja terviserajad",  
                                        "Satelliit", 
                                        "Pinnavormid"),
                         overlayGroups = c("Rattamarsruut", "Fotod"),
                         options = layersControlOptions(collapsed = TRUE)) %>%
    
    # Add a fullscreen control button
    addFullscreenControl()
}

plot_trip2(tracks$gpxstring[[5]])


leaflet() %>% 
  addProviderTiles("Stamen.Terrain", group = "Pinnavormid", options = providerTileOptions(detectRetina = T)) %>%
  addGPX(gpxstring)


o <- tidy_gpx(gpxstring)
xml2::as_xml_document(gpxstring) %>% tidy_gpx() %>% as.matrix %>% bbox

