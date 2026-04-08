# Header ------------------------------------------------------------------
# PROJECT: Filtering out all religious properties from NYS Centroids of Tax Parcels
# AUTHOR: Swati Sharma
# DATE: 2024-10-17

# PURPOSE: Extract religious properties from tax parcel centroids and calculate totals for AD, SD, and REDC regions.
install.packages("pacman")
install.packages("sf")
install.packages("dplyr")
install.packages("stringr")
install.packages("readr")
install.packages("formattable")





# Setup -------------------------------------------------------------------
library(sf)
library(dplyr)
library(stringr)
library(readr)
library(formattable)

# List layers in the geodatabase
st_layers("J:/DEPT/REUP/Projects/FBAHA/Raw Data/NYS_2023_Tax_Parcels_Centroid_Points.gdb")

# Load the specific layer
centroid_points <- st_read("J:/DEPT/REUP/Projects/FBAHA/Raw Data/NYS_2023_Tax_Parcels_Centroid_Points.gdb", layer = "NYS_Tax_Parcels_Centroid_Points")
assembly_districts <- st_read("J:/DEPT/REUP/Projects/FBAHA/NYS_Assembly_Districts_182770035837985083/NYS_Assembly_Districts.shp")
senate_districts <- st_read("J:/DEPT/REUP/Projects/FBAHA/NYS_Senate_Districts_1859322758626257138/NYS_Senate_Districts.shp")
redc_shapefile <- st_read("J:/DEPT/REUP/Projects/FBAHA/Raw Data/Regional_Economic_Development_Councils/Regional_Economic_Development_Councils.shp")

# Reproject to a common CRS (EPSG:4326) -----------------------------------
centroid_points <- st_transform(centroid_points, crs = 4326)
assembly_districts <- st_transform(assembly_districts, crs = 4326)
senate_districts <- st_transform(senate_districts, crs = 4326)
redc_shapefile <- st_transform(redc_shapefile, crs = 4326)

# Rename Columns for Assembly and Senate Districts ------------------------
assembly_districts <- assembly_districts %>%
  rename_with(~ paste0("AD_", .), .cols = -geometry)

senate_districts <- senate_districts %>%
  rename_with(~ paste0("SD_", .), .cols = -geometry)

# Validate and fix geometries
redc_shapefile <- st_make_valid(redc_shapefile)

# Perform Spatial Joins ---------------------------------------------------
centroids_with_ad <- st_join(centroid_points, assembly_districts, left = TRUE)  # Include centroids without Assembly match
centroids_with_ad_and_sd <- st_join(centroids_with_ad, senate_districts, left = TRUE)  # Include centroids without Senate match
centroids_with_ad_sd_redc <- st_join(centroids_with_ad_and_sd, redc_shapefile, left = TRUE)  # Join with REDC regions

# Calculate Totals for AD, SD, and Region (Before Filtering) --------------
# Calculate TOTAL_AD
total_ad_all <- centroids_with_ad_sd_redc %>%
  group_by(AD_District) %>%
  summarize(TOTAL_AD = n(), .groups = "drop")

# Calculate TOTAL_SD
total_sd_all <- centroids_with_ad_sd_redc %>%
  group_by(SD_DISTRICT) %>%
  summarize(TOTAL_SD = n(), .groups = "drop")

# Calculate TOTAL_REGION
total_region_all <- centroids_with_ad_sd_redc %>%
  group_by(REDC) %>%
  summarize(TOTAL_REGION = n(), .groups = "drop")

# Create Unique ID for Each Parcel ----------------------------------------
centroids_with_ad_sd_redc <- centroids_with_ad_sd_redc %>%
  mutate(unique_id = paste(SWIS, SBL, sep = "_"))

# Filter Religious Properties ---------------------------------------------
religious_properties <- centroids_with_ad_sd_redc %>%
  filter(PROP_CLASS == "620") %>%
  filter(!str_detect(tolower(PRIMARY_OWNER), 
                     "unavailable|united states|town of|village of|city of|county of|llc|,|university of rochester|young adult institute|fair housing dev fund|fire dist|s huntington|oorah inc|special|asacsc inc|young life|dcas|gspdc|cemetery")) %>%
  mutate(flag = 1)

# Add TOTAL_AD, TOTAL_SD, and TOTAL_REGION to Religious Properties
religious_properties <- religious_properties %>%
  left_join(st_drop_geometry(total_ad_all), by = "AD_District") %>%  
  left_join(st_drop_geometry(total_sd_all), by = "SD_DISTRICT") %>%  
  left_join(st_drop_geometry(total_region_all), by = "REDC")         



# Add NYC_DROP Column -----------------------------------------------------
nyc_counties <- c("New York", "Kings", "Queens", "Bronx", "Richmond")
religious_properties <- religious_properties %>%
  mutate(NYC_DROP = if_else(COUNTY_NAME %in% nyc_counties, 1, 0))

# Export Religious Properties ---------------------------------------------
output_dir <- "J:/DEPT/REUP/Projects/FBAHA/Clean Data"

st_write(religious_properties, file.path(output_dir, "all_religious_properties_with_totals.gpkg"), delete_layer = TRUE)

# Convert Geometry to WKT for CSV Export
religious_properties_csv <- religious_properties %>%
  mutate(SHAPE_WKT = st_as_text(Shape)) %>%
  select(-Shape)

colnames(religious_properties)

# Save as CSV
write.csv(religious_properties_csv, file.path(output_dir, "all_religious_properties_with_totals.csv"), row.names = FALSE)

cat("GeoPackage and CSV exported successfully.\n")
