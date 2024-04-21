# Install and load required packages
if (!requireNamespace("readxl", quietly = TRUE)) install.packages("readxl", dependencies = TRUE)
if (!requireNamespace("ggplot2", quietly = TRUE)) install.packages("ggplot2", dependencies = TRUE)
if (!requireNamespace("lubridate", quietly = TRUE)) install.packages("lubridate", dependencies = TRUE)
if (!requireNamespace("readxl", quietly = TRUE))
  install.packages("readxl", dependencies = TRUE)
library(readxl)
library(lubridate)
library(ggplot2)

rm(list = ls())


# Set the file path to your Excel workbook
excel_file <- "C:/Users/laura/OneDrive/Desktop/Bank of Canada Code/Wildfire suppression costs for select provinces from 1980 to 2009.xlsx"

# Read the sheets from the Excel workbook
sheet_names <- excel_sheets(excel_file)

# Function to create time-series plot for a given sheet
create_time_series_plot <- function(sheet_name) {
  # Read the data from the sheet
  data <- read_excel(excel_file, sheet = sheet_name)
  
  # Print column names
  cat("Columns in", sheet_name, ":", paste(names(data), collapse = ", "), "\n")
  
  # Check if "Total Costs (2009 CND$)" column exists
  if ("Total Costs (2009 CND$)" %in% names(data)) {
    # Convert any date columns to Date type
    date_cols <- sapply(data, is.Date)
    data[date_cols] <- lapply(data[date_cols], as.Date)
    
    # Create a time-series plot with a trend line
    plot <- ggplot(data, aes(x = Year, y = `Total Costs (2009 CND$)`)) +
      geom_line() +
      geom_smooth(method = "lm", se = FALSE, color = "blue") +  # Add a linear trend line
      labs(title = paste("Wildfire Suppression Costs for", sheet_name, "from 1980 to 2009"),
           x = "Year",
           y = "`Total Costs (2009 CND$)`") +
      theme_minimal()
    
    # Print the plot to the RStudio plot viewer
    print(plot)
    
    return(plot)
  } else {
    warning("Sheet", sheet_name, "does not have a 'Total Costs (2009 CND$)' column.")
    return(NULL)
  }
}

# Create and view time-series plots for each sheet
for (sheet_name in sheet_names) {
  create_time_series_plot(sheet_name)
}

