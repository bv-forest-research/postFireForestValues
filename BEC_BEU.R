install.packages(c("readxl", "dplyr", "sf", "writexl"))
library(readxl)
library(dplyr)
library(sf)
library(writexl)
file_path <- "C:/Users/Nikki Beaudoin/Documents/r_repo/postFireForestValues/01-data_inputs/FireRehabData_v5.xlsx"
sites <- read_excel(file_path, sheet = "Plot")
# Convert UTMs to Lat/Long #########
# Convert coordinate columns to numeric
sites <- sites %>%
  mutate(
    Easting = as.numeric(Easting),
    Northing = as.numeric(Northing),
    UTM_zone_num = as.integer(
      gsub("[^0-9]", "", as.character(UTM_zone))
    )
  )

# Initialize output columns
sites$Longitude <- NA_real_
sites$Latitude <- NA_real_

# Identify rows with valid coordinates and UTM zones
valid <- with(
  sites,
  !is.na(Easting) &
    !is.na(Northing) &
    !is.na(UTM_zone_num) &
    UTM_zone_num >= 1 &
    UTM_zone_num <= 60
)

# Convert each UTM zone separately
for (z in unique(sites$UTM_zone_num[valid])) {
  
  idx <- which(valid & sites$UTM_zone_num == z)
  
  points <- st_as_sf(
    sites[idx, ],
    coords = c("Easting", "Northing"),
    crs = 32600 + z
  )
  
  points_ll <- st_transform(points, crs = 4326)
  coords <- st_coordinates(points_ll)
  
  sites$Longitude[idx] <- coords[, 1]
  sites$Latitude[idx] <- coords[, 2]
}

# Review all sites
sites %>%
  select(ID, UTM_zone, Easting, Northing,
         Latitude, Longitude) %>%
  print(n = 75)

# Check which sites could not be converted
sites %>%
  filter(is.na(Latitude) | is.na(Longitude)) %>%
  select(ID, UTM_zone, Easting, Northing)

# Export results
output_path <- "C:/Users/Nikki Beaudoin/Documents/r_repo/postFireForestValues/01-data_inputs/FireRehabData_v5_with_LatLong.xlsx"

write_xlsx(
  sites %>% select(-UTM_zone_num),
  output_path
)

cat("Saved file to:", output_path)

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

# Adding BEU ########

beu_url <- paste0(
  "https://delivery.maps.gov.bc.ca/arcgis/rest/services/",
  "whse/bcgw_pub_whse_terrestrial_ecology/MapServer/7"
)

beu <- st_read(
  paste0(beu_url, "/query?where=1%3D1&outFields=*&",
         "returnGeometry=true&f=geojson"),
  quiet = FALSE
)

# Ensure both layers use the same coordinate reference system
beu <- st_transform(beu, st_crs(sites_bec))

# Join BEU attributes to each site
sites_beu <- st_join(
  sites_bec,
  beu,
  join = st_within,
  left = TRUE
)

# View the results
names(sites_beu)

sites_beu %>%
  st_drop_geometry() %>%
  select(ID, Latitude, Longitude) %>%
  head()





# Count unique values in the available fields
beu %>%
  st_drop_geometry() %>%
  count(FEATURE_CODE, sort = TRUE)

beu %>%
  st_drop_geometry() %>%
  count(QBEI_TAG, sort = TRUE) %>%
  print(n = 20)
