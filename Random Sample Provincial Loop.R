rm(list = ls())
# Install and load required packages
packages <- c("sf", "dplyr", "ggplot2", "lwgeom", "elevatr")

# Install and load packages
lapply(packages, function(pkg) {
  if (!requireNamespace(pkg, quietly = TRUE)) {
    install.packages(pkg, dependencies = TRUE)
    library(pkg, character.only = TRUE)
  }
})

library("sf")
library("dplyr")
library("ggplot2")
library("lwgeom")
library("elevatr")


# List of provinces
provinces <- c("British Columbia", "Alberta", "Ontario", "Quebec", "Saskatchewan", "Manitoba", "Northwest Territories", "Yukon")

# Set export folder
export_folder <- "C:/Users/laura/OneDrive/Desktop/Bank of Canada Code/Provincial Random Samples"

# Loop through each province
for (province in provinces) {
  # Set directory and file names
  directory_path <- "C:/Users/laura/OneDrive/Desktop/Bank of Canada Code/Provincial Shapefiles"
  geojson_file <- paste0(province, ".geojson")
  
  # Create the full file path
  geojson_path <- file.path(directory_path, geojson_file)
  
  # Get R to read provincial shape file
  province_boundaries <- st_read(geojson_path)
  
  # Plot bounding box
  print(ggplot() +
          geom_sf(data = province_boundaries, fill = "white", color = "black") +
          theme_minimal())
  
  # Generate 100 random points within province
  random_points <- st_sample(province_boundaries, size = 100, type = "random")
  
  # Extract latitude and longitude from the random points
  random_coordinates <- st_coordinates(random_points)
  
  # Plot the province boundaries and random points
  print(ggplot() +
          geom_sf(data = province_boundaries, fill = "white", color = "black") +
          geom_point(data = as.data.frame(random_coordinates), aes(x = X, y = Y), color = "red", size = 2) +
          theme_minimal() +
          labs(x = "Longitude", y = "Latitude", title = paste("100 Randomly Generated Points Within", province)))
  
  
  # Create an sf data frame from random_coordinates
  random_sf <- st_as_sf(data.frame(X = random_coordinates[, "X"], Y = random_coordinates[, "Y"]), 
                        coords = c("X", "Y"), 
                        crs = 4326)
  
  # Create a function to get elevation for a set of coordinates
  get_elevation <- function(coords) {
    elev <- elevatr::get_elev_point(coords, prj = "+proj=longlat +datum=WGS84")
    return(elev$elevation)
  }
  
  # Add elevation to the random coordinates
  random_sf$elevation <- get_elevation(random_sf)
  
  # Extract the coordinates from the geometry column of random_sf
  random_coordinates <- st_coordinates(random_sf$geometry)
  
  # Create new columns for Longitude and Latitude
  random_sf$Longitude <- random_coordinates[, "X"]
  random_sf$Latitude <- random_coordinates[, "Y"]
  
  # Plot the province boundaries, random points, and elevation
  print(ggplot() +
          geom_sf(data = province_boundaries, fill = "white", color = "black") +
          geom_sf(data = random_sf, aes(color = elevation), size = 2) +
          scale_color_gradientn(colors = viridisLite::viridis(10), name = "Elevation") +
          theme_minimal() +
          labs(x = "Longitude", y = "Latitude", title = paste("100 Randomly Generated Points Within", province)))

  
  # Reorder columns to have latitude, longitude, and elevation in the correct order
  random_sf <- random_sf[, c("Latitude", "Longitude", "elevation")]
  
  # Drop the geometry column
  random_sf <- st_drop_geometry(random_sf)
  
  # Export CSV for each province
  csv_filename <- file.path(export_folder, paste0("random_points_with_elevation_", tolower(province), ".csv"))
  write.csv(random_sf, file = csv_filename, row.names = FALSE)
  
}

  