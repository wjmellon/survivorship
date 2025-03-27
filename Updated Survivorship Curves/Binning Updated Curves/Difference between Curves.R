# Load required libraries
library(ggplot2)
library(dplyr)
library(survival)
library(survminer)
library(future.apply)

# Set parallel processing plan
plan(multisession)  

# Load dataset
data <- read.csv("records.csv")

# Filter data once to avoid redundant filtering inside loops
filtered_data <- data %>%
  filter(Necropsy == 1, Infant == 0) %>%
  select(Species, Class, age_months = 3, max_longevity = 48, Malignant)

# Compute cancer prevalence by species
species_cancer_prevalence <- filtered_data %>%
  group_by(Species) %>%
  summarise(
    total_individuals = n(),
    malignant_cases = sum(Malignant == 1, na.rm = TRUE),
    benign_cases = sum(Malignant == 0, na.rm = TRUE),
    malignant_prevalence = malignant_cases / total_individuals,
    benign_prevalence = benign_cases / total_individuals
  )

# Merge back to main dataset
filtered_data <- filtered_data %>%
  left_join(species_cancer_prevalence, by = "Species")

# Function to fit survival curves and extract slopes
compute_survival_slopes <- function(class_name, species_name, max_relative_age = 1.5, threshold = 0.1) {
  
  results <- list()
  
  # Filter for the given species and class
  prep_data <- filtered_data %>%
    filter(Class == class_name, Species == species_name) %>%
    mutate(
      prevalence_group = case_when(
        malignant_prevalence >= threshold ~ "High Malignant Prevalence",
        benign_prevalence >= threshold ~ "High Benign Prevalence",
        TRUE ~ "Low Prevalence"
      )
    ) %>%
    na.omit() %>%
    mutate(relative_age = age_months / max_longevity) %>%
    filter(relative_age <= max_relative_age)
  
  if (nrow(prep_data) == 0) return(NULL)  # Skip empty species
  
  # Fit survival models per prevalence group
  fits <- prep_data %>%
    group_by(prevalence_group) %>%
    group_split() %>%
    setNames(unique(prep_data$prevalence_group)) %>%
    lapply(function(df) {
      survfit(Surv(relative_age, rep(1, nrow(df))) ~ 1, data = df)
    })
  
  # Extract slopes
  slopes <- lapply(names(fits), function(group) {
    fit <- fits[[group]]
    df <- data.frame(time = fit$time, survival = fit$surv)
    
    if (nrow(df) > 1) {
      lm_fit <- lm(survival ~ time, data = df)
      slope <- coef(lm_fit)[2]
      return(data.frame(Species = species_name, Class = class_name, Prevalence_Group = group, Slope = slope))
    } else {
      return(NULL)
    }
  })
  
  return(do.call(rbind, slopes))
}

# Get unique species list
species_list <- unique(filtered_data$Species)
class_name <- "Mammalia"

# Run the function in parallel
all_results <- future_lapply(species_list, function(species) {
  compute_survival_slopes(class_name, species, threshold = 0.1)
})

# Combine results into a single data frame
all_results <- do.call(rbind, all_results)

# Save results
write.csv(all_results, "survival_slopes.csv", row.names = FALSE)

# Print summary
print(head(all_results))

# ---- PLOTTING THE RESULTS ----

# Order species by slope for better visualization
all_results <- all_results %>%
  mutate(Species = factor(Species, levels = unique(Species[order(Slope)])))

# Generate ggplot
ggplot(all_results, aes(x = Species, y = Slope, fill = Prevalence_Group)) +
  geom_bar(stat = "identity", position = position_dodge(), width = 0.7) +
  coord_flip() +  # Flip to make species labels readable
  labs(
    title = "Survival Curve Slopes by Species and Cancer Prevalence",
    x = "Species",
    y = "Slope of Survival Curve",
    fill = "Prevalence Group"
  ) +
  theme_minimal() +
  theme(
    axis.text.y = element_text(size = 10),
    axis.text.x = element_text(size = 12),
    legend.position = "top"
  )
