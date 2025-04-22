# Load required libraries
library(ggplot2)
library(dplyr)
library(cowplot)
library(devtools)
library(Rage)

# Load the dataset
data <- read.csv("records.csv")

# Assuming common name is in the 5th column, select it
data_with_common_name <- data %>%
  select(3, 4, 9, 15, 24, 28, 48) # Select Species (scientific), Common Name, age_months, max_longevity
colnames(data_with_common_name) <- c("age_months", "Infant", "Common_Name", "Necropsy", "Class", "Species", "max_longevity")

# Filter the dataset for Mammalia with Necropsy data and exclude infants
mammal_data <- data_with_common_name %>%
  filter(Necropsy == 1) %>%
  filter(Class == "Mammalia") %>%
  filter(Infant == 0) %>%
  mutate(
    age_months = as.numeric(age_months),
    max_longevity = as.numeric(max_longevity)
  ) %>%
  filter(age_months > 0, max_longevity > 0) # Ensure valid age and longevity


# Clean data: remove individuals with invalid ages
mammal_data$age_months[mammal_data$age_months <= 0] <- NA
mammal_data <- na.omit(mammal_data)

# Clean data: remove individuals with invalid ages
mammal_data$max_longevity[mammal_data$max_longevity <= 0] <- NA
mammal_data <- na.omit(mammal_data)


# Get unique species
unique_mammal_species <- unique(mammal_data$Species)

# Initialize lists to store common names for each type
type1_common_names <- list()
trending_type1_common_names <- list()
type2_common_names <- list()
trending_type3_common_names <- list()
type3_common_names <- list()

# Initialize lists to store species info (Common Name [Scientific Name])
type1_species_info <- list()
trending_type1_species_info <- list()
type2_species_info <- list()
trending_type3_species_info <- list()
type3_species_info <- list()

# Loop through each species
for (scientific_name in unique_mammal_species) {
  species_data <- mammal_data %>%
    filter(Species == scientific_name)
  
  # Get the common name for the current scientific name
  common_name <- unique(species_data$Common_Name)
  if (length(common_name) > 1) {
    cat(paste("Warning: Multiple common names found for", scientific_name, ". Using the first one.\n"))
    common_name <- common_name[1]
  } else if (length(common_name) == 0) {
    common_name <- NA_character_
  }
  
  # if (nrow(species_data) < 10) {
  #   cat(paste("Skipping", scientific_name, "due to insufficient data points (< 10).\n"))
  #   next
  # }
  
  # Create relative age
  species_data <- species_data %>%
    mutate(relative_age = age_months / max_longevity)
  
  # Define time steps
  time_steps <- seq(0, 1, by = 0.01)
  
  # Calculate individuals alive
  alive_counts <- sapply(time_steps, function(x) {
    sum(species_data$relative_age > x)
  })
  
  # Prepare survivorship vector
  lx <- alive_counts / max(alive_counts)
  
  # Calculate shape type
  shape_type <- tryCatch(shape_surv(lx), error = function(e) NA)
  
  if (!is.na(shape_type)) {
    # Categorize survivorship type
    surv_type <- case_when(
      shape_type >= 0.3 ~ "Type I",
      shape_type >= 0.1 & shape_type < 0.3 ~ "Trending toward Type I",
      shape_type > -0.1 & shape_type < 0.1 ~ "Type II",
      shape_type > -0.3 & shape_type <= -0.1 ~ "Trending toward Type III",
      shape_type <= -0.3 ~ "Type III",
      TRUE ~ NA_character_
    )
    
    # Store the common name based on its type
    if (!is.na(surv_type)) {
      if (surv_type == "Type I") {
        type1_common_names[[scientific_name]] <- common_name
        type1_species_info[[scientific_name]] <- paste0(common_name, " [", scientific_name, "]")
      } else if (surv_type == "Trending toward Type I") {
        trending_type1_common_names[[scientific_name]] <- common_name
        trending_type1_species_info[[scientific_name]] <- paste0(common_name, " [", scientific_name, "]")
      } else if (surv_type == "Type II") {
        type2_common_names[[scientific_name]] <- common_name
        type2_species_info[[scientific_name]] <- paste0(common_name, " [", scientific_name, "]")
      } else if (surv_type == "Trending toward Type III") {
        trending_type3_common_names[[scientific_name]] <- common_name
        trending_type3_species_info[[scientific_name]] <- paste0(common_name, " [", scientific_name, "]")
      } else if (surv_type == "Type III") {
        type3_common_names[[scientific_name]] <- common_name
        type3_species_info[[scientific_name]] <- paste0(common_name, " [", scientific_name, "]")
      }
    }
  } else {
    cat(paste("Could not calculate shape type for", scientific_name, ". Skipping.\n"))
  }
}

# Print the number of species in each list
cat("\nNumber of Mammal Species Categorized into Each Survivorship Type:\n")
cat("Type I:", length(type1_common_names), "\n")
cat("Trending toward Type I:", length(trending_type1_common_names), "\n")
cat("Type II:", length(type2_common_names), "\n")
cat("Trending toward Type III:", length(trending_type3_common_names), "\n")
cat("Type III:", length(type3_common_names), "\n")

# Print the lists of common names in each category
cat("\nCommon Names of Mammal Species in Each Survivorship Type:\n")
cat("Type I:", unlist(type1_common_names), "\n")
cat("Trending toward Type I:", unlist(trending_type1_common_names), "\n")
cat("Type II:", unlist(type2_common_names), "\n")
cat("Trending toward Type III:", unlist(trending_type3_common_names), "\n")
cat("Type III:", unlist(type3_common_names), "\n")

# Print the lists of common names with scientific names in each category
cat("\nMammal Species (Common Name [Scientific Name]) in Each Survivorship Type:\n")
cat("Type I:", unlist(type1_species_info), "\n")
cat("Trending toward Type I:", unlist(trending_type1_species_info), "\n")
cat("Type II:", unlist(type2_species_info), "\n")
cat("Trending toward Type III:", unlist(trending_type3_species_info), "\n")
cat("Type III:", unlist(type3_species_info), "\n")