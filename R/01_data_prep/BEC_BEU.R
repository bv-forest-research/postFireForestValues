# Load libraries
library(data.table)
library(tidyverse)
library(ggpubr)
library(dplyr)
library(sf)

in_dir <- "01-data_inputs"
out_dir <- "02-prepped_values"



#-- Load data-------------------------------------------------------------------
# Plot/treatment data:
FR_treatments <- fread(file.path(in_dir,"FR_Treatments.csv"))
setnames(FR_treatments, "ID", "PlotID")

# Convert UTMs to Lat/Long #########
# Convert to an sf object using UTM coordinates
FR_treatments_sf <- st_as_sf(
  FR_treatments,
  coords = c("Easting", "Northing"),
  crs = 26909,  # Example: NAD83 / UTM zone 9N
  remove = FALSE)

# Transform to latitude/longitude (WGS84)
FR_treatments_ll <- st_transform(FR_treatments_sf, crs = 4326)

# Extract longitude and latitude
coords <- st_coordinates(FR_treatments_ll)

FR_treatments[, Longitude := coords[, 1]]
FR_treatments[, Latitude := coords[, 2]]

######## IGNORE FOR NOW #########
# Convert .xlsx to shapefile in QGIS #########
latlong <- "C:/Users/Nikki Beaudoin/Documents/r_repo/postFireForestValues/01-data_inputs/FireRehabData_v5_with_LatLong.xlsx"

latlongsites <- read_excel(file_path)

# Keep only sites with valid latitude and longitude
sites_valid <- sites %>%
  filter(
    !is.na(Latitude),
    !is.na(Longitude),
    between(Latitude, -90, 90),
    between(Longitude, -180, 180)
  )

# Convert the table to a spatial point layer
sites_sf <- st_as_sf(
  sites_valid,
  coords = c("Longitude", "Latitude"),
  crs = 4326,
  remove = FALSE
)

# Check the spatial object
print(sites_sf)
st_crs(sites_sf)

# Choose an output folder
output_folder <- "C:/Users/Nikki Beaudoin/Documents/r_repo/postFireForestValues/01-data_inputs/shapefiles"

# Create the folder if it doesn't exist
dir.create(
  output_folder,
  recursive = TRUE,
  showWarnings = FALSE
)

# Write the shapefile
shp_path <- file.path(output_folder, "FireRehab_sites.shp")

st_write(
  sites_sf,
  shp_path,
  driver = "ESRI Shapefile",
  delete_layer = TRUE
)

cat("Shapefile saved to:", shp_path)
gpkg_path <- file.path(
  output_folder,
  "FireRehab_sites.gpkg"
)

st_write(
  sites_sf,
  gpkg_path,
  layer = "FireRehab_sites",
  delete_layer = TRUE
)


gpkg_path <- file.path(
  output_folder,
  "FireRehab_sites.gpkg"
)

st_write(
  sites_sf,
  gpkg_path,
  layer = "FireRehab_sites",
  delete_layer = TRUE
)

# Add BEC #####
bec_path <- "C:/Users/Nikki Beaudoin/Downloads/BCGW_02001F02_1792089317625_5024/BEC_BIOGEOCLIMATIC_POLY/BEC_POLY_polygon.shp"
bec<-st_read(bec_path)

# Transform site points to match the BEC polygon CRS
sites_bc <- st_transform(sites_sf, st_crs(bec))

# Join BEC attributes to each site
sites_bec <- st_join(
  sites_bc,
  bec,
  join = st_intersects,
  left = TRUE
)

# Inspect the results
names(sites_bec)

sites_bec %>%
  st_drop_geometry() %>%
  select(ID, Latitude, Longitude) %>%
  head()