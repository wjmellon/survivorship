# Libraries
library(ggplot2)
library(dplyr)
library(survival)
library(survminer)
library(devtools)
install_github("jonesor/rage")  # Only needed once
library(Rage)

# Load your dataset
data <- read.csv("records.csv")

# Add species-level cancer prevalence
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

data <- data %>%
  left_join(species_cancer_prevalence, by = "Species")

# -----------------------------
# Function: Calculate mortality rates per bin
get_mortality_data <- function(df, class_name, subgroup_label) {
  # Filter by class and valid ages
  full_class_data <- df %>%
    filter(Infant == 0, Class == class_name, age_months > 0, max_longevity > 0) %>%
    mutate(relative_age = age_months / max_longevity) %>%
    filter(relative_age >= 0 & relative_age <= 1.25)
  
  # Filter by subgroup
  if (subgroup_label == "Benign") {
    full_class_data <- full_class_data %>% filter(Malignant == 0)
  } else if (subgroup_label == "Malignant") {
    full_class_data <- full_class_data %>% filter(Malignant == 1)
  } else if (subgroup_label == "Neoplasia") {
    full_class_data <- full_class_data %>% filter(Malignant %in% c(0, 1))
  } else if (subgroup_label != "Normal") {
    stop("Subgroup must be one of: 'Benign', 'Malignant', 'Neoplasia', or 'Normal'")
  }
  
  # Bin data
  binned_data <- full_class_data %>%
    mutate(age_bin = cut(relative_age, breaks = 15, include.lowest = TRUE))
  
  # Calculate mortality rate
  mortality_data <- binned_data %>%
    group_by(age_bin) %>%
    summarise(
      bin_center = mean(relative_age, na.rm = TRUE),
      n_total = n(),
      n_died = sum(Necropsy == 1, na.rm = TRUE),
      mortality_rate = n_died / n_total
    ) %>%
    ungroup() %>%
    mutate(Group = subgroup_label)
  
  return(mortality_data)
}
# -----------------------------
# Function: Plot with "Normal" background
# -----------------------------
plot_mortality_by_relative_age <- function(class_name = "Mammalia", subgroup = "Neoplasia") {
  normal_data <- get_mortality_data(data, class_name, "Normal")
  target_data <- get_mortality_data(data, class_name, subgroup)
  
  ggplot() +
    # Background: Normal
    geom_point(data = normal_data, aes(x = bin_center, y = mortality_rate), 
               shape = 1, color = "gray50") +
    geom_line(data = normal_data, aes(x = bin_center, y = mortality_rate), 
              linetype = "dashed", color = "gray50") +
    
    # Foreground: Subgroup
    geom_point(data = target_data, aes(x = bin_center, y = mortality_rate), 
               shape = 16, color = "#1f77b4") +
    geom_line(data = target_data, aes(x = bin_center, y = mortality_rate), 
              linetype = "dashed", color = "#1f77b4") +
    
    labs(
      title = "Mortality Rate vs. Relative Age",
      subtitle = paste("Class:", class_name, "| Subgroup:", subgroup),
      x = "Relative Age (age_months / max_longevity)",
      y = "Mortality Rate"
    ) +
    theme_minimal(base_size = 14)
}


plot_mortality_by_relative_age("Mammalia", "Benign")
