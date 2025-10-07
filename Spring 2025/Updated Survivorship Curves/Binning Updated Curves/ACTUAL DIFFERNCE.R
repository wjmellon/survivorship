# Load required libraries
library(dplyr)
library(tidyr)
library(survival)

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
compute_survival_slopes_class <- function(class_name, threshold = 0.1) {
  
  # Filter for the given class
  prep_data <- filtered_data %>%
    filter(Class == class_name) %>%
    mutate(
      prevalence_group_malignant = case_when(
        malignant_prevalence >= threshold ~ "High Malignant Prevalence",
        TRUE ~ "Low Malignant Prevalence"
      ),
      prevalence_group_benign = case_when(
        benign_prevalence >= threshold ~ "High Benign Prevalence",
        TRUE ~ "Low Benign Prevalence"
      )
    ) %>%
    na.omit() %>%
    mutate(relative_age = age_months / max_longevity)
  
  if (nrow(prep_data) == 0) return(NULL)  # Skip empty class
  
  # Fit survival models for low vs high malignant prevalence
  fits_malignant <- prep_data %>%
    group_by(prevalence_group_malignant) %>%
    group_split() %>%
    setNames(unique(prep_data$prevalence_group_malignant)) %>%
    lapply(function(df) {
      survfit(Surv(relative_age, rep(1, nrow(df))) ~ 1, data = df)
    })
  
  # Fit survival models for low vs high benign prevalence
  fits_benign <- prep_data %>%
    group_by(prevalence_group_benign) %>%
    group_split() %>%
    setNames(unique(prep_data$prevalence_group_benign)) %>%
    lapply(function(df) {
      survfit(Surv(relative_age, rep(1, nrow(df))) ~ 1, data = df)
    })
  
  # Extract slopes for malignant and benign groups
  slopes_malignant <- lapply(names(fits_malignant), function(group) {
    fit <- fits_malignant[[group]]
    df <- data.frame(time = fit$time, survival = fit$surv)
    
    if (nrow(df) > 1) {
      lm_fit <- lm(survival ~ time, data = df)
      slope <- coef(lm_fit)[2]
      return(data.frame(Class = class_name, Cancer_Type = "Malignant", Prevalence_Group = group, Slope = slope))
    } else {
      return(NULL)
    }
  })
  
  slopes_benign <- lapply(names(fits_benign), function(group) {
    fit <- fits_benign[[group]]
    df <- data.frame(time = fit$time, survival = fit$surv)
    
    if (nrow(df) > 1) {
      lm_fit <- lm(survival ~ time, data = df)
      slope <- coef(lm_fit)[2]
      return(data.frame(Class = class_name, Cancer_Type = "Benign", Prevalence_Group = group, Slope = slope))
    } else {
      return(NULL)
    }
  })
  
  # Combine both results
  all_slopes <- do.call(rbind, c(slopes_malignant, slopes_benign))
  
  # Return the final table
  return(all_slopes)
}

# Get unique classes
class_names <- unique(filtered_data$Class)

# Compute slopes for each class
all_results <- lapply(class_names, function(class_name) {
  compute_survival_slopes_class(class_name, threshold = 0.1)
})

# Combine all results into a single data frame
all_results <- do.call(rbind, all_results)

# Calculate the difference in slopes and which group has higher slope
final_results <- all_results %>%
  pivot_wider(names_from = Prevalence_Group, values_from = Slope) %>%
  mutate(
    Difference = `High Malignant Prevalence` - `Low Malignant Prevalence`,
    Higher_Survivorship = ifelse(Difference > 0, "Low Malignant Prevalence", "High Malignant Prevalence")
  ) %>%
  select(Class, Cancer_Type, `High Malignant Prevalence`, `Low Malignant Prevalence`, Difference, Higher_Survivorship)

# Print the table to the terminal
print(final_results)

# Optionally, you can save it to a CSV
# write.csv(final_results, "survival_slope_comparison.csv", row.names = FALSE)
