# Extract date from file name
extract_date <- function(gpxfile){
  # Remove letters and punctuation from file name
  datetime <- gsub("[a-zA-Z[:punct:]]", "", gpxfile)
  # Parse datetime
  datetime <- lubridate::ymd_hms(datetime)
  # Extract date
  date <- lubridate::date(datetime)
  as.character(date)
}
