# Load required library
library(readr)

# File paths
file1 <- "C:\\Users\\arswa\\Downloads\\tract_nyc_2010_2020 - Merged_tracts.csv"
file2 <- "C:\\Users\\arswa\\Downloads\\aggregated_data.csv" 
file3 <- "C:\\Users\\arswa\\Downloads\\New_York_State_ZIP_Codes-County_FIPS_Cross-Reference.csv"
# Load the CSV files
data1 <- read_csv(file1)
data2 <- read_csv(file2)
data3 <- read_csv(file3)
# Print column names for each file
cat("Columns in the first CSV file:\n")
print(colnames(data1))

cat("\nColumns in the second CSV file:\n")
print(colnames(data2))
print(colnames(data3))


# Define the counties to keep
counties_to_keep <- c("New York", "Richmond", "Queens", "Kings", "Bronx")

# Filter the dataset
filtered_data3 <- data3[data3$`County Name` %in% counties_to_keep, ]

# View the filtered dataset
print(head(filtered_data3))


# Use aggregate to group by County Name and find unique County FIPS values
unique_fips <- aggregate(`County Code` ~ `County Name`, data = filtered_data3, FUN = function(x) unique(x))

# Print the result
cat("Unique County FIPS values for each County Name:\n")
print(unique_fips)




# Create a mapping of BoroCode_20 to County Name and County Code
borocode_to_county <- data.frame(
  BoroCode_20 = c(2, 3, 1, 4, 5),
  `County Name` = c("Bronx", "Brooklyn", "Manhattan", "Queens", "Staten Island"),
  `County Code` = c("005", "047", "061", "081", "085")
)

# Merge data1 with the mapping table based on BoroCode_20
data1 <- merge(data1, borocode_to_county, by.x = "BoroCode_20", by.y = "BoroCode_20", all.x = TRUE)

# View the updated data1
print(head(data1))

colnames(data1)


# Create a new column 'tract' by removing the first digit from 'BoroCT2010'
data1$tract_noboro_10 <- substr(data1$BoroCT2010, 2, nchar(data1$BoroCT2010))
 
# View the updated dataset
print(head(data1))


# Remove leading zeros from the County.Code column
data1$CountyCode_no_leading <- sub("^0+", "", data1$County.Code)

# Combine CountyCode_no_leading and tract_noboro_10 into a new column tract_10
data1$tract_10 <- paste0(data1$CountyCode_no_leading, data1$tract_noboro_10)

# View the updated dataset
print(head(data1))




# Select only necessary columns from data2_grouped for merging
data2_to_merge <- data2 %>%
  select(cntytrct, average, level, sba)

# Merge with data1 based on tract_10 (data1) and tract (data2_grouped)
merged_data <- merge(data1, data2_to_merge, 
                     by.x = "tract_10", 
                     by.y = "cntytrct", 
                     all.x = TRUE)

# View the merged dataset
print(head(merged_data))


# Count the number of NA values in the 'category' column
na_count_category <- sum(is.na(merged_data$level))

# Print the count
cat("Number of NA values in the 'category' column:", na_count_category, "\n")


colnames(merged_data)


# Rename the columns
merged_data <- merged_data %>%
  rename(
    CT2020 = `CT2020...1`,
    BoroCT2020 = `BoroCT2020...2`,
    GEOID_2020 = `GEOID_20...3`
  )

# Select only the renamed and specified columns
merged_data <- merged_data %>%
  select(
    CT2020, BoroCT2020, GEOID_2020,
    tract_10, BoroCode_20, Area_20, CTLabel_20, 
    BoroCT2010, Status, CTLabel_10, CT2010, 
    County.Name, County.Code, tract_noboro_10, 
    average, level, sba
  )

# View the updated dataset
print(head(merged_data))



# Save the merged dataset as a CSV file
write.csv(merged_data, "NYC_2010_2020_CT_SURGE_CROSSWALK_data.csv", row.names = FALSE)

# Confirm the file has been saved
cat("Merged dataset saved as 'merged_data_final.csv'\n")
getwd()




