# Setup -------------------------------------------------------------------
library(sf)
library(dplyr)
library(readr)

# File paths
nyc_religious_properties_path <- "J:/DEPT/REUP/Projects/FBAHA/Clean Data/nyc_religious_tax_parcels_with_totals_2024.gpkg"
urban_areas_path <- "J:/DEPT/REUP/Projects/FBAHA/shapefile_urban_areas/nys_census_urban_areas.shp"
output_dir <- "J:/DEPT/REUP/Projects/FBAHA/Clean Data/"

# Load NYC Religious Properties and Urban Areas --------------------------
nyc_religious_properties <- st_read(nyc_religious_properties_path)
urban_areas <- st_read(urban_areas_path)

# Reproject Urban Areas to match the CRS of NYC religious properties -----
urban_areas <- st_transform(urban_areas, crs = st_crs(nyc_religious_properties))

# Add Urban Area Flag -----------------------------------------------------
nyc_religious_properties <- nyc_religious_properties %>%
  mutate(urban_area_flag = if_else(lengths(st_within(nyc_religious_properties, urban_areas)) > 0, 1, 0))

# Check and Calculate ACRES and SQ_FT Columns -----------------------------
nyc_religious_properties <- nyc_religious_properties %>%
  mutate(
    # If ACRES column is missing, create it
    ACRES = ifelse("ACRES" %in% colnames(.), ACRES, NA_real_),
    # Calculate ACRES directly from LotArea if missing or zero
    ACRES = ifelse(is.na(ACRES) | ACRES == 0, LotArea / 43560, ACRES),
    # Ensure SQ_FT matches LotArea
    SQ_FT = LotArea  # LotArea is already in square feet
  )

# Check Updated Column Names ----------------------------------------------
cat("Column names after adding ACRES and SQ_FT:\n")
print(colnames(nyc_religious_properties))

# Add Development Potential -----------------------------------------------
nyc_religious_properties <- nyc_religious_properties %>%
  mutate(
    dev_potential = ifelse(
      urban_area_flag == 1 & !is.na(ACRES),  # Check if within urban area and area is valid
      ACRES * 50,  # Multiply by 50
      NA  # Otherwise, set to NA
    )
  )



# Add unique_id column with BBL values
nyc_religious_properties <- nyc_religious_properties %>%
  mutate(unique_id = as.character(BBL))  # Create a unique_id using the BBL values

# Check the updated column names and preview
cat("Column names after adding unique_id:\n")
print(colnames(nyc_religious_properties))



# Save Final GeoPackage ---------------------------------------------------
final_file_name <- "nyc_religious_properties_with_urban_area_and_potential.gpkg"
st_write(nyc_religious_properties, 
         file.path(output_dir, final_file_name), 
         delete_layer = TRUE)

# Drop Geometry for CSV Export --------------------------------------------
nyc_religious_properties_df_no_geometry <- nyc_religious_properties %>%
  st_drop_geometry()

# Save as CSV -------------------------------------------------------------
write.csv(nyc_religious_properties_df_no_geometry, 
          file.path(output_dir, "nyc_religious_properties_with_urban_area_and_potential.csv"), 
          row.names = FALSE)

cat("GeoPackage and CSV saved as 'nyc_religious_properties_with_urban_area_and_potential' successfully.\n")
