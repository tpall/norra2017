
tidy_gpx <- function(gpxstring) {
  
  # Strip namespace
  xml <- xml2::as_xml_document(gpxstring)
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
  
  # Create data_frame
  dplyr::bind_cols(trkpt, dplyr::data_frame(ele, time))
}
