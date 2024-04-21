# Clear the environment
rm(list = ls())

# Install required packages if not already installed
packages <- c("openxlsx", "ggplot2", "scales")
install_packages <- packages[!(packages %in% installed.packages()[,"Package"])]
if(length(install_packages)) install.packages(install_packages, dependencies = TRUE)

library("openxlsx")
library("ggplot2")
library("scales")


# Set input file path
directory_path <- "C:/Users/laura/OneDrive/Desktop/Bank of Canada Code/Provincial Fires and Hectares Burned"

# Get a list of all Excel files in the directory
file_list <- list.files(path = directory_path, pattern = "\\.xlsx", full.names = TRUE)

# Loop through each file
for (file_path in file_list) {
  # Print the filename
  cat("Graphing file:", file_path, "\n")
  
  # Read the Excel workbook
  data <- read.xlsx(file_path, sheet = 1)
  
  # Create a data frame with the relevant columns
  custom_dataframe <- data.frame(Year = as.numeric(data$Year), 
                                 Area = as.numeric(data$Area))
  
  # Create a time series plot
  p <- ggplot(custom_dataframe, aes(x = Year)) +
    geom_bar(aes(y = Area), stat = "identity", fill = "blue") +
    labs(title = "Hectares Burned",
         x = "Year", y = "Area (hectares)") +
    scale_y_continuous(labels = scales::number_format())
  
  # Print the plot
  print(p)
}