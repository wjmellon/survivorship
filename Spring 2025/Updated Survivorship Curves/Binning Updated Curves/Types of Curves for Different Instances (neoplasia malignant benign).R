library(ggplot2)
library(dplyr)
library(survival)
library(survminer)
library(devtools)
install_github("jonesor/rage")

library(Rage) # Load the Rage library

# Load the dataset
data <- read.csv("records.csv")

# Compute cancer prevalence by species (for potential later use)
species_cancer_prevalence <- data %>%
  filter(Necropsy == 1, Infant == 0) %>%
  group_by(Species) %>%
  summarise(
    total_individuals = n(),
    malignant_cases = sum(Malignant == 1, na.rm = TRUE),
    benign_cases = sum(Malignant == 0, na.rm = TRUE),
    neoplasia_cases = malignant_cases + benign_cases,
    malignant_prevalence = malignant_cases / total_individuals,
    benign_prevalence = benign_cases / total_individuals,
    neoplasia_prevalence = neoplasia_cases / total_individuals
  )

# Merge back to main dataset
data <- data %>%
  left_join(species_cancer_prevalence, by = "Species")

# Function to calculate survivorship type using RAGE (direct method)
calculate_rage_type <- function(df) {
  relative_age_vector <- df %>%
    select(age_months, max_longevity) %>%
    mutate(relative_age = age_months / max_longevity) %>%
    filter(!is.na(relative_age) & relative_age >= 0 & relative_age <= 1) %>%
    pull(relative_age)
  
  if (length(relative_age_vector) < 2) {
    return(NA)
  }
  
  time_steps <- seq(0, 1, by = 0.01)
  alive_counts <- sapply(time_steps, function(x) {
    sum(relative_age_vector > x, na.rm = TRUE)
  })
  lx_raw <- alive_counts / max(alive_counts, na.rm = TRUE)
  lx <- ifelse(lx_raw == 0, min(lx_raw[lx_raw > 0], na.rm = TRUE) / 2, lx_raw)
  
  if (all(is.na(lx)) || any(!is.finite(lx))) {
    return(NA)
  }
  
  shape_type <- tryCatch({
    shape_surv(lx)
  }, error = function(e) {
    message(paste("Error in shape_surv:", e$message))
    return(NA)
  })
  
  surv_type <- case_when(
    !is.na(shape_type) & shape_type >= 0.3 ~ "Type I",
    !is.na(shape_type) & shape_type >= 0.1 & shape_type < 0.3 ~ "Trending I",
    !is.na(shape_type) & shape_type > -0.1 & shape_type < 0.1 ~ "Type II",
    !is.na(shape_type) & shape_type > -0.3 & shape_type <= -0.1 ~ "Trending III",
    !is.na(shape_type) & shape_type <= -0.3 ~ "Type III",
    TRUE ~ NA_character_
  )
  return(surv_type)
}

# Get unique classes
unique_classes <- c("Mammalia", "Aves", "Amphibia", "Reptilia")

# Initialize an empty list to store results
surv_type_data <- list()

# Iterate through each class
for (class_name in unique_classes) {
  class_data_filtered <- data %>%
    filter(Necropsy == 1, Infant == 0, Class == class_name, age_months != -1)
  
  # Initialize a row for the current class
  class_row <- list(Class = class_name)
  
  # Calculate survivorship type for "normal" (all individuals in the class)
  class_row$Normal <- calculate_rage_type(class_data_filtered)
  
  # Calculate survivorship type for malignant cases
  malignant_data <- class_data_filtered %>% filter(Malignant == 1)
  class_row$Malignant <- calculate_rage_type(malignant_data)
  
  # Calculate survivorship type for benign cases
  benign_data <- class_data_filtered %>% filter(Malignant == 0)
  class_row$Benign <- calculate_rage_type(benign_data)
  
  # Calculate survivorship type for neoplasia cases
  neoplasia_data <- class_data_filtered %>% filter(Malignant %in% c(0, 1))
  class_row$Neoplasia <- calculate_rage_type(neoplasia_data)
  
  # Add the row to the results list
  surv_type_data[[class_name]] <- class_row
}

# Convert the list to a data frame
surv_type_table <- bind_rows(surv_type_data)

# Print the resulting table
print(surv_type_table)