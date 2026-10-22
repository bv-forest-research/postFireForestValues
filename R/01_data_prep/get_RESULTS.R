# This script imports the RESULTS openings from BC Data Catalogue for our plots

# Load libraries
library(sf)
library(bcdata) # BC data catalouge 
library(fs)
library(data.table)

in_dir <- "01-data_inputs"
out_dir <- "01-data_inputs"



#-- Load data-------------------------------------------------------------------
# plots
FRplots <- st_read(file.path(in_dir, "FRplots.gpkg")) # BCAlbers 

# RESULTS openings
#https://catalogue.data.gov.bc.ca/dataset/results-openings-svw

openings <- bcdata::bcdc_query_geodata("53a17fec-e9ad-4ac0-95e6-f5106a97e677") |>
  bcdata::filter(bcdata::INTERSECTS(FRplots)) |>
  bcdata::collect()

st_write(openings, path(out_dir, "results_openings_FRplots.gpkg"), delete_dsn = TRUE)
