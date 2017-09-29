
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

source("lib/merge_gpx.R")
tracks <- mutate(tracks, gpxstring = map_chr(gpx_files, merge_gpx, writetofile = FALSE))

library(sp)
library(rgdal)
library(leaflet)
library(leaflet.extras)




