
library(leaflet)
library(htmltools)
library(htmlwidgets)
# library(leaflet.extras)
# source("lib/elevation.R")
source("lib/merge_gpx.R")
source("lib/get_gpx_bounds.R")


gpx_file <- "data/Move_2017_07_12_11_47_34_Cycling.gpx"
gpx <- readr::read_file(gpx_file)
bb <- get_gpx_bounds(gpx_file)


# kml_file <- "data/Move_2017_07_12_11_47_34_Cycling.kml"

m <- leaflet() %>%
  fitBounds(lng1 = bb[[2]], lat1 = bb[[1]], lng2 = bb[[4]], lat2 = bb[[3]]) %>%
  addProviderTiles("OpenStreetMap.Mapnik")

elevationPlugin <- htmlDependency("Leaflet.elevation", "0.0.4",
      src = normalizePath("inst/htmlwidgets/lib/elevation/"),
      script = "leaflet.elevation-0.0.4.src.js",
      stylesheet = "leaflet.elevation-0.0.4.css")

gpxPlugin <- htmlDependency("leaflet-gpx", "0.0.1",
      src = normalizePath("inst/htmlwidgets/lib/gpx/"),
      script = "gpx.js")

# A function that takes a plugin htmlDependency object and adds
# it to the map. This ensures that however or whenever the map
# gets rendered, the plugin will be loaded into the browser.
registerPlugin <- function(map, plugin) {
  map$dependencies <- c(map$dependencies, list(plugin))
  map
}

m %>%
  # Register plugins on this map instance
  registerPlugin(gpxPlugin) %>%
  registerPlugin(elevationPlugin) %>%
  # Add your custom JS logic here. The `this` keyword
  # refers to the Leaflet (JS) map object.
  onRender("function(el, x, data) {

          L.control.elevation().addTo(this);

          var g = new L.GPX(data, {async: true});
          
            g.on('loaded', function(e) {
		  		      this.fitBounds(e.target.getBounds());
           });
           
           g.on('addline',function(e){
           el.addData(e.line);
           });
           
           g.addTo(this);

          


  }", data = gpx)

