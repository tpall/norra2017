
#' Extract date from file name
#' @description Extracts date info from a file name.
#' @param gpxfile a file name with date and time info, character string.
#' @return a character string of date
#' @import lubridate
#' 
extract_date <- function(gpxfile){
  # Remove letters and punctuation from file name
  datetime <- gsub("[a-zA-Z[:punct:]]", "", gpxfile)
  # Parse datetime
  datetime <- lubridate::ymd_hms(datetime)
  # Extract date
  date <- lubridate::date(datetime)
  as.character(date)
}

#' Join/merge GPX files
#' @description Joins GPX files into single output using gpsbabel track,merge option. 
#' @param gpxfiles a list of .gpx file names.
#' @param writetofile TRUE, whether direct output to a file or into character vector. 
#' @param pathtosave "./", path where to save merged output, when writetofile is TRUE.
#' @param activity "Cycling", activity to add to file name, when writetofile is TRUE.
#' @return A character vector. When `writetofile` is TRUE, merged file is saved to directory specified by `pathtosave` and function returns file name of the joined/merged track in gpx format. If `writetofile` is FALSE, gpx xml string is returned.  
#' @examples 
#' \dontrun{
#' gpxfiles <- list.files("inst/gpx/", full.names = TRUE)
#' merged_gpx <- merge_gpx(gpxfiles)
#' merged_gpx
#' }
#' @import stringr
#' @export
#' 
merge_gpx <- function(gpxfiles, writetofile = TRUE, pathtosave = "./", activity = "Cycling") {
  
  # Input files
  input <- paste("-i gpx -f", gpxfiles, collapse = " ")
  
  # Writes to stdout
  output <- "-"
  
  # Write to file
  if(writetofile){
    # Create date id from unique dates
    date_id <- vapply(gpxfiles, extract_date, character(1))
    date_id <- paste0(unique(date_id), collapse = "_")
    # Output file name with path
    output <- file.path(pathtosave, sprintf("Merged_%s_%s.gpx", date_id, activity))
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
