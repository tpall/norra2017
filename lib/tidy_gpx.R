
tidy_gpx <- function(xml) {
  
  # Strip namespace
  xml <- xml2::xml_ns_strip(xml)
  
  trk <- xml2::xml_find_all(xml, "trk") 
  trk <- xml2::xml_child(trk, "trkseg") 
  trk <- xml2::xml_children(trk)
  
  # Latitude and longitude
  trkpt <- xml2::xml_attrs(trk) 
  trkpt <- do.call(rbind, trkpt) 
  trkpt <- tibble::as_tibble(trkpt) 
  trkpt <- dplyr::mutate_all(trkpt, as.numeric)
  
  # Elevation
  ele <- xml2::xml_find_all(trk, "ele")
  ele <- xml2::xml_double(ele)
  
  # Time
  time <- xml2::xml_find_all(trk, "time") 
  time <- xml2::xml_text(time)
  time <- readr::parse_datetime(time)
  
  # Extensions
  trk_nodes <- xml2::xml_find_all(trk, "extensions")
  trk_nodes <- purrr::map(trk_nodes, xml2::xml_contents)
  extensions <- purrr::map(trk_nodes, xml2::xml_double) 
  extensions <- do.call(rbind, extensions) 
  extensions <- tibble::as_tibble(extensions) 
  extensions <- dplyr::mutate_all(extensions, as.numeric)
  colnames(extensions) <- xml2::xml_name(trk_nodes[[1]])
  
  # Create data_frame
  dplyr::bind_cols(trkpt, tibble::tibble(ele, time), extensions)
}
