# Install required packages if not already installed
if (!requireNamespace("sf", quietly = TRUE)) install.packages("sf")
if (!requireNamespace("ggplot2", quietly = TRUE)) install.packages("ggplot2")
if (!requireNamespace("dplyr", quietly = TRUE)) install.packages("dplyr")

# Load necessary libraries
library(sf)
library(ggplot2)
library(dplyr)

# Define paths
geo_package_path <- "J:/DEPT/REUP/Projects/FBAHA/Clean Data/updated_religious_properties_with_dev_potential_withNYC (1).gpkg"
shapefile_path <- "J:/DEPT/REUP/Projects/FBAHA/Clean Data/NYS_ZCTA/NYS_ZCTA.shp"
regions_path <- "J:/DEPT/REUP/Projects/FBAHA/Raw Data/Regional_Economic_Development_Councils/Regional_Economic_Development_Councils.shp"

# Load GeoPackage data
geo_data <- st_read(geo_package_path)

# Load Shapefile data
shp_data <- st_read(shapefile_path)

# Ensure CRS matches
if (st_crs(geo_data) != st_crs(shp_data)) {
  geo_data <- st_transform(geo_data, crs = st_crs(shp_data))
}

# Spatial join: Count the number of properties in each ZIP code
join_result <- st_intersects(shp_data, geo_data)
shp_data$Rel_prop_Count <- lengths(join_result)

# Rename the overall map object
fbo_plot <- ggplot(data = shp_data) +
  geom_sf(aes(fill = Rel_prop_Count), color = NA) +
  scale_fill_viridis_c(
    option = "viridis", direction = -1, name = "Property Count",
    breaks = c(0, 50, 100, 150, 200, 250),
    labels = c("0", "50", "100", "150", "200", "250")
  ) +
  theme_minimal() +
  labs(
    title = "FBO Owned Properties at ZIP Code-Level in New York State",
    subtitle = "Heatmap of Property Count",
    caption = "Source: NYU Furman Center"
  ) +
  theme(
    legend.position = "right",
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.title = element_blank(),
    axis.text = element_blank()
  )

# Define the file path for saving the overall plot
output_file <- "J:/DEPT/REUP/Projects/FBAHA/FBO_Properties_ZIP_Level_NY.jpg"

# Save the overall plot
ggsave(filename = output_file, plot = fbo_plot, width = 10, height = 8, units = "in", dpi = 300)

# Load the regions shapefile
regions <- st_read(regions_path)

# Ensure geometries in both shapefiles are valid
shp_data <- st_make_valid(shp_data)
regions <- st_make_valid(regions)

# Ensure CRS matches between shp_data and regions
if (st_crs(shp_data) != st_crs(regions)) {
  regions <- st_transform(regions, crs = st_crs(shp_data))
}

# Perform spatial intersection with cleaned geometries
shp_data_by_region <- st_intersection(shp_data, regions)

# View the resulting dataset
print(head(shp_data_by_region))

# Extract unique region names
unique_regions <- unique(shp_data_by_region$REDC)  # Replace `REDC` with the correct column name

# Loop through each region and generate maps
for (region in unique_regions) {
  # Filter data for the current region
  region_data <- shp_data_by_region %>% filter(REDC == region)
  
  # Generate the map for the region
  region_plot <- ggplot(data = region_data) +
    geom_sf(aes(fill = Rel_prop_Count), color = NA) +
    scale_fill_viridis_c(
      option = "viridis", direction = -1, name = "Property Count"
    ) +
    theme_minimal() +
    labs(
      title = paste("Faith Based Owned Properties in", region, "Region"),
      subtitle = "ZIP Code-Level Property Count",
      caption = "Source: NYU Furman Center"
    ) +
    theme(
      legend.position = "right",
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      axis.title = element_blank(),
      axis.text = element_blank()
    )
  
  # Save the region map
  ggsave(filename = paste0("J:/DEPT/REUP/Projects/FBAHA/Regional_Maps/Religious_Properties_", region, ".jpg"), 
         plot = region_plot, width = 8, height = 6)
}

# Check the working directory (optional)
print(getwd())








#state level interactive map

if (!requireNamespace("leaflet", quietly = TRUE)) install.packages("leaflet")
library(leaflet)



library(htmltools) # For embedding HTML in the map

library(htmltools) # For embedding HTML in the map

# Create the interactive Leaflet map
leaflet_map <- leaflet(data = shp_data) %>%
  addProviderTiles(providers$CartoDB.Positron) %>% # Base map
  addPolygons(
    color = NA, # Remove the outline
    weight = 0, # Set border width to 0
    fillColor = ~colorNumeric(
      palette = "viridis", 
      domain = range(shp_data$Rel_prop_Count) # Original range
    )(max(shp_data$Rel_prop_Count) - Rel_prop_Count + min(shp_data$Rel_prop_Count)),
    fillOpacity = 0.7,
    popup = ~paste(
      "<strong>ZIP Code:</strong>", ZCTA5CE20, "<br>",
      "<strong>Property Count:</strong>", Rel_prop_Count
    ),
    highlightOptions = highlightOptions(
      weight = 2, # Add border when hovered
      color = "white", # Border color when hovered
      fillOpacity = 0.9, # Increase fill opacity on hover
      bringToFront = TRUE
    ),
    label = ~paste("ZIP Code:", ZCTA5CE20, "<br>Property Count:", Rel_prop_Count), # Hover label
    labelOptions = labelOptions(
      style = list("font-weight" = "bold", "font-size" = "12px"), # Label style
      direction = "auto"
    )
  ) %>%
  addLegend(
    pal = colorNumeric(
      palette = "viridis", 
      domain = range(shp_data$Rel_prop_Count) # Original range
    ),
    values = max(shp_data$Rel_prop_Count) - shp_data$Rel_prop_Count + min(shp_data$Rel_prop_Count), # Inverted legend
    title = "Property Count (Inverted)",
    position = "bottomright"
  ) %>%
  # Add a centered title using HTML and CSS
  addControl(
    html = tags$div(
      style = "font-size: 18px; font-weight: bold; text-align: center; 
               background-color: rgba(255, 255, 255, 0.7); 
               padding: 10px; border-radius: 5px; max-width: 300px; margin: auto;",
      "FBO Owned Properties in New York State"
    ),
    position = "topleft"
  ) %>%
  setView(lng = -75, lat = 43, zoom = 6) # Center the map on NY State

# Show the map
leaflet_map
