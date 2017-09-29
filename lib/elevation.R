
# Deps
elevationDependency <- function() {
  list(
    htmltools::htmlDependency(
      "Leaflet.elevation",
      version = "0.0.4",
      src = "inst/htmlwidgets/lib/elevation/",
      script = "leaflet.elevation-0.0.4.src.js",
      stylesheet = "leaflet.elevation-0.0.4.css"
    )
  )
}

gpxDependency <- function() {
  list(
    htmltools::htmlDependency(
      "leaflet-gpx",
      version = "0.0.1",
      src = "inst/htmlwidgets/lib/gpx/",
      script = "gpx.js"
    )
  )
}

#' Add a elevation linegraph to map
#' @description AAdd a elevation linegraph
#' @param map The leaflet map
#' @param position position of control: 'topleft', 'topright', 'bottomleft', or 'bottomright'
#' @param theme theme of widget.
#' @param with width of widget px.
#' @param height height of widget px.
#' @param margins margins of widget px, list of four: 'top', 'right', 'bottom', 'left'.
#' @param useHeightIndicator default TRUE.
#' @param interpolation default 'linear'.
#' @param hoverNumber format of displayed number upon hover, list of three: 'decimalsX', 'decimalsY', 'formatter'.
#' @param collapsed default FALSE
#' @param forceAxisBounds default FALSE
#' @param imperial default FALSE
#' @rdname elevation
#' @export
#' @examples
#' \dontrun{
#' leaflet() %>% addTiles() %>%
#'   addFullscreenControl()
#' }
addElevation <- function(map,
                         data = getMapData(map),
                         position = "topleft"
                         ,
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
                         imperial = FALSE
                         ) {

  map$dependencies <- c(map$dependencies, elevationDependency())

  # options <- c(options,
  #              list(position = position,
  #                   theme = theme,
  #                   width = width,
  #                   height = height,
  #                   margins = margins,
  #                   useHeightIndicator = useHeightIndicator,
  #                   interpolation = interpolation,
  #                   hoverNumber = hoverNumber,
  #                   xTicks = xTicks,
  #                   yTicks = yTicks,
  #                   collapsed = collapsed,
  #                   yAxisMin = yAxisMin,
  #                   yAxisMax = yAxisMax,
  #                   forceAxisBounds = forceAxisBounds,
  #                   controlButton = controlButton,
  #                   imperial = imperial))

  leaflet::invokeMethod(map, data, "addElevation", position
                        , theme, width, height, margins, useHeightIndicator,
                        interpolation, hoverNumber, xTicks, yTicks, collapsed, yAxisMin, yAxisMax, forceAxisBounds,
                        controlButton, imperial
                        )

}

addGPX <- function(map, gpx){

  map$dependencies <- c(map$dependencies, gpxDependency())

  leaflet::invokeMethod(map, leaflet::getMapData(map), "addGPX", gpx)

}
