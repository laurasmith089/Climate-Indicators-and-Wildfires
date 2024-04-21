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

# Loop over each file
for (file_path in file_list) {
  # Read the Excel workbook
  data <- read.xlsx(file_path, sheet = 1)
  
  # Clean up column names by removing leading and trailing whitespaces
  colnames(data) <- trimws(colnames(data))
  
  # Specify columns to drop
  columns_to_drop <- c("Elevation", "SPRINGAVGTEMP", "SUMMERAVGTEMP", 
                       "SPRINGAVGPPT", "SUMMERAVGPPT", "AVGNFFD", 
                       "SPRINGAVGCMD", "SUMMERAVGCMD")
  
  # Drop specified columns
  data <- data[, !colnames(data) %in% columns_to_drop]
  
  # Drop rows with NA values in any column
  data <- na.omit(data)
  
  # Print file name
  print(paste("File:", basename(file_path)))
  
  # Set up a 2x4 layout with square plots
  par(mfrow = c(4, 2), mar = c(4, 4, 2, 1))
  
  #SPRING AVERAGE TEMPERATURE
  data$Year <- as.numeric(data$Year)
  
  seg_fit_spring_temp <- segmented(lm(AG_SPRINGAVGTEMP ~ Year, data = data), 
                                   seg.Z = ~Year, control = seg.control(display = FALSE))
  new_data <- data.frame(Year = data$Year)
  
  predictions_spring_temp <- predict(seg_fit_spring_temp, newdata = new_data)
  
  plot(data$Year, data$AG_SPRINGAVGTEMP, type = "l", xlab = "Year", ylab = "Degrees Celsius", main = "Spring Average Temperature")
  lines(data$Year, predictions_spring_temp, col = "blue", lty = 1)
  
  #SUMMER AVERAGE TEMPERATURE
  seg_fit_summer_temp <- segmented(lm(AG_.SUMMERAVGTEMP ~ Year, data = data), 
                                   seg.Z = ~Year, control = seg.control(display = FALSE))
  new_data <- data.frame(Year = data$Year)
  
  predictions_summer_temp <- predict(seg_fit_summer_temp, newdata = new_data)
  
  plot(data$Year, data$AG_.SUMMERAVGTEMP, type = "l", xlab = "Year", ylab = "Degrees Celsius", main = "Summer Average Temperature")
  lines(data$Year, predictions_summer_temp, col = "blue", lty = 1)
  
  #SPRING TOTAL PRECIPITATION
  spring_ppt <- lm(AG_SPRINGAVGPPT ~ Year, data = data)
  
  predictions_spring_ppt <- predict(spring_ppt, newdata = new_data)
  
  plot(data$Year, data$AG_SPRINGAVGPPT, type = "l", xlab = "Year", ylab = "Precipitation (mm)", main = "Spring Total Precipitation")
  lines(data$Year, predictions_spring_ppt, col = "blue", lty = 1)
  
  #SUMMER TOTAL PRECIPITATION
  summer_ppt <- lm(AG_.SUMMERAVGTEMP ~ Year, data = data)
  
  predictions_summer_ppt <- predict(summer_ppt, newdata = new_data)
  
  plot(data$Year, data$AG_.SUMMERAVGTEMP, type = "l", xlab = "Year", ylab = "Precipitation (mm)", main = "Summer Total Precipitation")
  lines(data$Year, predictions_summer_ppt, col = "blue", lty = 1)
  
  #SPRING TOTAL CLIMATIC MOISTURE DEFICIT
  seg_fit_spring_cmd <- segmented(lm(AG_SPRINGAVGCMD ~ Year, data = data), 
                                  seg.Z = ~Year, control = seg.control(display = FALSE))
  
  predictions_spring_cmd <- predict(seg_fit_spring_cmd, newdata = new_data)
  
  plot(data$Year, data$AG_SPRINGAVGCMD, type = "l", xlab = "Year", ylab = "Climatic Moisture Deficit (mm)", main = "Spring Climatic Moisture Deficit")
  lines(data$Year, predictions_spring_cmd, col = "blue", lty = 1)
  
  #SUMMER TOTAL CLIMATIC MOISTURE DEFICIT
  seg_fit_summer_cmd <- segmented(lm(AG_SUMMERAVGCMD ~ Year, data = data), 
                                  seg.Z = ~Year, control = seg.control(display = FALSE))
  
  predictions_summer_cmd <- predict(seg_fit_summer_cmd, newdata = new_data)
  
  plot(data$Year, data$AG_SUMMERAVGCMD, type = "l", xlab = "Year", ylab = "Climatic Moisture Deficit (mm)", main = "Summer Climatic Moisture Deficit")
  lines(data$Year, predictions_summer_cmd, col = "blue", lty = 1)
  
  #NUMBER OF FROST-FREE DAYS
  seg_fit_nffd <- segmented(lm(AG_AVGNFFD ~ Year, data = data), 
                            seg.Z = ~Year, control = seg.control(display = FALSE))
  
  predictions_nffd <- predict(seg_fit_nffd, newdata = new_data)
  
  plot(data$Year, data$AG_AVGNFFD, type = "l", xlab = "Year", ylab = "Number of Days", main = "Annual Number of Frost-Free Days")
  lines(new_data$Year, predictions_nffd, col = "blue", lty = 1)
}

