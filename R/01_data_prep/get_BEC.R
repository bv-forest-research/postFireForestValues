# This script imports the BEC from BC Data Catalogue for our plots

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

#-- Get BC catalogue data ----
# BEC dataset

#https://catalogue.data.gov.bc.ca/dataset/bec-map
bec <- bcdc_query_geodata("f358a53b-ffde-4830-a325-a5a03ff672c3") |>
  select(BGC_LABEL, ZONE, SUBZONE, VARIANT, PHASE) |>
  collect()
# this can take a while fyi

#-- Prep data ----
# Convert crs to 4326
FRplots <- st_transform(FRplots, 4326)

# Match CRS to plots
bec <- st_transform(bec, st_crs(FRplots))

# Attach BEC attributes to plots, keeping all plots
FRplots_bec <- st_join(FRplots, bec[c("BGC_LABEL","ZONE", "SUBZONE", "VARIANT", "PHASE")], left = TRUE)

# convert to data table and add lat and long as columns
FRplots_dt <- copy(as.data.table(st_drop_geometry(FRplots_bec)))
# Add longitude and latitude columns
FRplots_dt[, `:=`(
  Longitude = st_coordinates(FRplots)[, 1],
  Latitude  = st_coordinates(FRplots)[, 2]
)]


#-- Export data ----
write.csv(FRplots_dt, path(out_dir, "FRplots_BEC.csv"))
