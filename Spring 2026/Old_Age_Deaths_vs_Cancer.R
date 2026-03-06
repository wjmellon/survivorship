# Load required libraries
library(dplyr)
library(tibble)
library(ggplot2)

# Load individual data 
individual_data <- read.csv("Fall 2025/Filtering Data/cleanPath.min20.062822.csv")

# Load min 20 species data
species_data <- read.csv("Fall 2025/Fall 2025 Clean Data/final_clean_data_w_multivariate.csv")

# Filter for individuals that are in min 20 species data 
filtered_individuals <- individual_data %>%
  semi_join(species_data, by = "Species")

# Calculate final 33% of lifespan 
max_age_species <- filtered_individuals %>%
  group_by(Species) %>%
  summarise(max_age = max(age_months, na.rm = TRUE))

species_data <- species_data %>%
  left_join(max_age_species, by = "Species") %>%
  mutate(old_age = max_age * 0.67)

         
# Merge data sets 
combined_data <- filtered_individuals %>%
  left_join(species_data, by = "Species") %>%
  select(
    ID, 
    age_months, 
    Species, 
    Class.x, 
    n, 
    max_longevity.y, 
    gestation, 
    body_size, 
    neoplasia_prevalence, 
    cancer_prevalence, 
    survivorship_type, 
    shape_value,
    max_age, 
    old_age)

# Calculate proportion of individuals that reach old age 
old_age_counts <- combined_data %>%
  group_by(Species) %>%
  summarise(prop_old_age = (sum(age_months >= old_age, na.rm = TRUE)/n()))

combined_data <- combined_data %>%
  left_join(old_age_counts, by = "Species") 

# Plot neoplasia_prevalence vs old_age_counts
ggplot(combined_data, aes(x = prop_old_age, y = neoplasia_prevalence)) +
  geom_point(alpha = 1, size = 1, color = "lightblue") +
  geom_smooth(method = "lm", se = TRUE, color = "black") +
  theme_minimal() +
  labs(
    x = "Proportion of Individuals that died in Old Age",
    y = "Neoplasia Prevalence",
    title = "Neoplasia Prevalence vs Death in Old Age"
  )

# Run the linear model
neoplasia_old_age_model <- lm(neoplasia_prevalence ~ prop_old_age, data = combined_data)

# See results
summary(neoplasia_old_age_model)

# Plot cancer_prevalence vs old_age_counts
ggplot(combined_data, aes(x = prop_old_age, y = cancer_prevalence)) +
  geom_point(alpha = 1, size = 1, color = "lightblue") +
  geom_smooth(method = "lm", se = TRUE, color = "black") +
  theme_minimal() +
  labs(
    x = "Proportion of Individuals that died in Old Age",
    y = "Cancer Prevalence",
    title = "Cancer Prevalence vs Death in Old Age"
  )

# Run the linear model
cancer_old_age_model <- lm(cancer_prevalence ~ prop_old_age, data = combined_data)

# See results
summary(cancer_old_age_model)

