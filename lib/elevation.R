
# Source 
elevationDependency <- function() {
  list(
    htmltools::htmlDependency(
      "Leaflet.elevation", version = "0.0.4",
      "inst/htmlwidgets/lib/elevation/",
      script = c("leaflet.elevation-0.0.4.min.js"),
      stylesheet = c("leaflet.elevation-0.0.4.css")
    )
  )
}

#' Add a elevation linegraph to map
#' @description AAdd a elevation linegraph
#' @param map The leaflet map
#' @param position position of control: 'topleft', 'topright', 'bottomleft', or 'bottomright'
#' @rdname elevation
#' @export
#' @examples
#' \dontrun{
#' leaflet() %>% addTiles() %>%
#'   addFullscreenControl()
#' }
addElevation <- function(map, 
                         position = "topleft",
                         theme = "lime-theme",
                         width = 600,
                         height = 175,
                         margins = list(top = 10, right = 20, bottom = 30, left = 60),
                         useHeightIndicator = TRUE,
                         interpolation = "linear",
                         hoverNumber = list(decimalsX = 3, decimalsY = 0, formatter = NULL),
                         xTicks = NULL,
                         yTicks = NULL,
                         collapsed = FALSE,
                         yAxisMin = NULL,
                         yAxisMax = NULL,
                         forceAxisBounds = FALSE,
                         controlButton = list(iconCssClass = "elevation-toggle-icon", title = "Elevation"),
                         imperial = FALSE) {
  
  map$dependencies <- c(map$dependencies, elevationDependency())
  
  if (is.null(map$x$options)){
    map$x$options <- list()
    }
  
  map$x$options['elevationControl'] <-
    list(list(position = position, 
              theme = theme, 
              width = width, 
              height = height, 
              margins = margins,
              useHeightIndicator = useHeightIndicator,
              interpolation = interpolation,
              hoverNumber = hoverNumber,
              xTicks = xTicks,
              yTicks = yTicks,
              collapsed = collapsed,
              yAxisMin = yAxisMin,
              yAxisMax = yAxisMax,
              forceAxisBounds = forceAxisBounds,
              controlButton = controlButton,
              imperial = imperial))
  map
}


