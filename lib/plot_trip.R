

# plot_trip2 function -----------------------------------------------------
#' @param gpx URL to your GPX file or the GPX itself
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
    addGPX(gpx, fill = FALSE, group = "Rattamarsruut")
  
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

# plot trip function ------------------------------------------------------
plot_trip <- function(data, trip, popupdata) {
  # Thunderforest base url
  thunderforest_link_template <- "https://{s}.tile.thunderforest.com/%s/{z}/{x}/{y}.png?apikey=%s"
  
  m <- leaflet() %>%
    
    # Add tiles
    addTiles(sprintf(thunderforest_link_template, "landscape", Sys.getenv("THUNDERF_APIKEY")), group = "Topograafiline") %>%
    addTiles(sprintf(thunderforest_link_template, "cycle", Sys.getenv("THUNDERF_APIKEY")), group = "Rattateed") %>%
    addTiles(sprintf(thunderforest_link_template, "outdoors", Sys.getenv("THUNDERF_APIKEY")), group = "Matka- ja terviserajad") %>%
    addProviderTiles("Stamen.Terrain", group = "Pinnavormid", options = providerTileOptions(detectRetina = T)) %>%
    addProviderTiles("OpenStreetMap.Mapnik", group = "Sõiduteed", options = providerTileOptions(detectRetina = T)) %>%
    addProviderTiles("Esri.WorldImagery", group = "Satelliit", options = providerTileOptions(detectRetina = T)) %>%
    
    # Add legend
    addLegend(position = 'bottomright', opacity = 0.4, 
              colors = 'blue', 
              labels = trip,
              title = 'Norra 2017')
  
  if(!is.null(popupdata)){
    
    # Add photo markers
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
    addFullscreenControl() %>% 
    
    addPolylines(data = data, color='blue', group = "Rattamarsruut") 
}