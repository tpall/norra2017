#' Get GPS track bounds from GPX
#' @param gpx URL to your GPX file or the GPX itself
#' 
get_gpx_bounds <- function(gpx_file) {
  
  # gpsbabelize
  gpx <- merge_gpx(gpx_file, writetofile = FALSE)
  
  # Extract bounds from metadata field
  xml2::as_xml_document(gpx) %>% 
    xml2::xml_ns_strip() %>% 
    xml2::xml_find_all("metadata/bounds") %>% 
    xml2::xml_attrs() %>% 
    unlist %>%
    vapply(as.numeric, numeric(1))
}
