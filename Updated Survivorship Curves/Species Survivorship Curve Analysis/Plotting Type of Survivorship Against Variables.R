library(ggplot2)
library(dplyr)
library(stringr) # for more robust string handling

# Load the individual records data
data <- read.csv("records.csv")

# Load the survivorship curve table, skipping the first row and using the second as header
table_data <- read.csv("Updated Survivorship Curves/Species Survivorship Curve Analysis/DataSheet w All Species Survivorship Curves - Sheet1.csv", skip = 1, header = TRUE)

# Print the head of table_data after loading
print("Head of table_data after skipping the first row:")
print(head(table_data))

# --- Function to Create Bar Graph with Class Coloring (from records.csv) ---
create_survivorship_plot <- function(y_variable) {
  # Check if the y_variable exists in the data frame
  if (!y_variable %in% names(data)) {
    stop(paste("Error: Column '", y_variable, "' not found in records.csv", sep = ""))
  }
  
  # Create a string for the y-axis label
  y_label <- paste("Average Log10(", y_variable, ")", sep = "")
  
  # Calculate average and standard error of the chosen logged variable for each curve type
  summary_data <- data %>%
    filter(Species %in% table_data$`Scientific.Name`, !is.na(!!sym(y_variable)), !!sym(y_variable) != -1, !!sym(y_variable) > 0) %>%
    left_join(table_data %>% select(`Scientific.Name`, `Curve.Type`), by = c("Species" = "Scientific.Name")) %>%
    group_by(`Curve.Type`) %>%
    summarize(
      Average_Log_Y = mean(log10(!!sym(y_variable)), na.rm = TRUE),
      SE_Log_Y = sd(log10(!!sym(y_variable)), na.rm = TRUE) / sqrt(n()),
      .groups = 'drop'
    )
  
  # Calculate average of the chosen logged variable for each species and get Class from records.csv
  species_avg_data <- data %>%
    filter(Species %in% table_data$`Scientific.Name`, !is.na(!!sym(y_variable)), !!sym(y_variable) != -1, !!sym(y_variable) > 0) %>%
    select(Species, !!sym(y_variable), Class) %>% # Select Species, y_variable, and Class from records.csv
    left_join(table_data %>% select(`Scientific.Name`, `Curve.Type`), by = c("Species" = "Scientific.Name")) %>%
    group_by(Species, `Curve.Type`, Class) %>%
    summarize(Species_Avg_Log_Y = mean(log10(!!sym(y_variable)), na.rm = TRUE), .groups = 'drop')
  
  # Define color mapping for curve types (for bars and error bars)
  curve_colors <- c(
    "Type I" = "blue",
    "Trending toward Type I" = "skyblue",
    "Type II" = "green",
    "Trending toward Type III" = "lightcoral",
    "Type III" = "red"
  )
  
  # Define color mapping for Class (for individual points)
  class_colors <- c(
    "Mammalia" = "purple",
    "Aves" = "orange",
    "Reptilia" = "darkgreen",
    "Amphibia" = "yellow"
  )
  
  # Create the bar graph with error bars and individual species averages colored by Class
  p <- ggplot(summary_data, aes(x = Curve.Type, y = Average_Log_Y, fill = Curve.Type)) +
    geom_bar(stat = "identity", color = "black", alpha = 0.7, width = 0.4) +
    geom_errorbar(aes(ymin = Average_Log_Y - SE_Log_Y, ymax = Average_Log_Y + SE_Log_Y),
                  width = 0.2, color = "black", linewidth = 0.7) +
    geom_jitter(data = species_avg_data, aes(x = Curve.Type, y = Species_Avg_Log_Y, color = Class),
                position = position_jitter(width = 0.15, height = 0), size = 3, alpha = 0.7) +
    scale_fill_manual(values = curve_colors) +
    scale_color_manual(values = class_colors) + # Use class colors for points
    labs(
      x = "Survivorship Curve Type",
      y = y_label,
      fill = "Curve Type",
      color = "Class", # Update legend title for points
      title = paste("Average Log10(", y_variable, ") by Survivorship Curve Type with Species Averages (Colored by Class)", sep = "")
    ) +
    theme_minimal() +
    theme(legend.position = "bottom")
  
  print(p)
}
create_survivorship_plot("Gestation")

