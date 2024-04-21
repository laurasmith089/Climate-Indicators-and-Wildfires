# Clear the environment
rm(list = ls())

# Install and load required packages
packages <- c("openxlsx", "dplyr", "tidyr", "segmented")
install_packages <- packages[!(packages %in% installed.packages()[,"Package"])]
if(length(install_packages)) install.packages(install_packages, dependencies = TRUE)
lapply(packages, library, character.only = TRUE)

# Set input file path
directory_path <- "C:/Users/laura/OneDrive/Desktop/Bank of Canada Code/Provincial Averages by Year"

# Get a list of all files in the directory
file_list <- list.files(directory_path, pattern = "\\.xlsx$", full.names = TRUE)

# Define a mapping of file names to descriptive province titles
province_names <- c(
  "climate_na_input_alberta_1920-2020M_processed_annual.xlsx" = "Alberta",
  "climate_na_input_british columbia_1920-2020M_processed_annual.xlsx" = "British Columbia",
  "climate_na_input_manitoba_1920-2020M_processed_annual.xlsx" = "Manitoba",
  "climate_na_input_northwest territories_1920-2020M_processed.xlsx" = "Northwest Territories",
  "climate_na_input_ontario_1920-2020M_processed_annual.xlsx" = "Ontario",
  "climate_na_input_quebec_1920-2020M_processed.xlsx" = "Quebec",
  "climate_na_input_saskatchewan_1920-2020M_processed_annual.xlsx" = "Saskatchewan",
  "climate_na_input_yukon_1920-2020M_processed_annual.xlsx" = "Yukon"
)

# Initialize an empty data frame to store all spring CMD data
all_spring_cmd_data <- data.frame()

# Loop over each file
for (file_path in file_list) {
  # Read the Excel workbook
  data <- read.xlsx(file_path, sheet = 1)
  
  # Clean up column names by removing leading and trailing whitespaces
  colnames(data) <- trimws(colnames(data))
  
  # Filter to get only the necessary columns for plotting
  data <- data[c("Year", "AG_SPRINGAVGCMD")]
  
  # Drop rows with NA values
  data <- na.omit(data)
  
  # Add a column to identify the province using the mapping
  file_basename <- basename(file_path)
  data$Province <- province_names[file_basename]
  
  # Append to the main data frame
  all_spring_cmd_data <- rbind(all_spring_cmd_data, data)
}

# Prepare for external plotting
par(mar = c(5, 4, 4, 2) + 0.1, oma = c(0, 0, 0, 15), xpd = NA)

# Plotting all Spring CMD data together
plot(NULL, xlim = range(all_spring_cmd_data$Year), ylim = range(all_spring_cmd_data$AG_SPRINGAVGCMD), 
     xlab = "Year", ylab = "Spring Climatic Moisture Deficit (mm)", 
     main = "Spring Climatic Moisture Deficit Across Jurisdictions")
colors <- rainbow(n = length(unique(all_spring_cmd_data$Province)))

# Loop to add lines for each province
for(i in unique(all_spring_cmd_data$Province)) {
  province_data <- all_spring_cmd_data[all_spring_cmd_data$Province == i,]
  lines(province_data$Year, province_data$AG_SPRINGAVGCMD, type = "l", col = colors[which(unique(all_spring_cmd_data$Province) == i)])
}

# Add a legend outside the plot to the right
legend(x = par("usr")[2] + 0.5, y = par("usr")[4], legend = unique(all_spring_cmd_data$Province), col = colors, lty = 1, title = "Jurisdictions", horiz = FALSE, xpd = NA)
