library(ggplot2)
library(dplyr)
library(survival)

data <- read.csv("records.csv")

# Filter only captive individuals
captive_data <- data %>% filter(Wild == 0)

# Compute cancer prevalence by species
species_cancer_prevalence <- captive_data %>%
  filter(Necropsy == 1, Infant == 0) %>%
  group_by(Species) %>%
  summarise(
    total_individuals = n(),
    malignant_cases = sum(Malignant == 1, na.rm = TRUE),
    benign_cases = sum(Malignant == 0, na.rm = TRUE),
    malignant_prevalence = malignant_cases / total_individuals,
    benign_prevalence = benign_cases / total_individuals
  )

# Merge back to main dataset
captive_data <- captive_data %>%
  left_join(species_cancer_prevalence, by = "Species")

plot_prevalence_survival <- function(class_name, max_relative_age = 1.25, threshold = 0.001, type = "malignant") {
  # Choose classification type
  if (type == "malignant") {
    captive_data <- captive_data %>%
      mutate(prevalence_group = factor(
        ifelse(malignant_prevalence >= threshold, "High Malignant Prevalence", "Low Malignant Prevalence"),
        levels = c("Low Malignant Prevalence", "High Malignant Prevalence")
      ))
    type_label <- "Malignant"
  } else if (type == "benign") {
    captive_data <- captive_data %>%
      mutate(prevalence_group = factor(
        ifelse(benign_prevalence >= threshold, "High Benign Prevalence", "Low Benign Prevalence"),
        levels = c("Low Benign Prevalence", "High Benign Prevalence")
      ))
    type_label <- "Benign"
  }
  
  # Filter the data for the specific class and remove NA values
  prep_data <- captive_data %>%
    filter(Necropsy == 1, Infant == 0, Class == class_name) %>%
    select(age_months = 3, max_longevity = 48, prevalence_group) %>%
    mutate(
      across(c(age_months, max_longevity), ~ ifelse(.x <= 0, NA, .x))
    ) %>%
    na.omit() %>%
    mutate(relative_age = age_months / max_longevity) %>%
    filter(relative_age <= max_relative_age)
  
  # Create survival objects for prevalence groups
  fits <- prep_data %>%
    group_by(prevalence_group) %>%
    group_split() %>%
    setNames(unique(prep_data$prevalence_group)) %>%
    lapply(function(df) {
      if (nrow(df) == 0) return(NULL)
      survfit(Surv(relative_age, rep(1, nrow(df))) ~ 1, data = df)
    })
  
  # Remove empty groups
  fits <- fits[!sapply(fits, is.null)]
  
  # Combine the results into a single data frame
  surv_data <- lapply(names(fits), function(group) {
    fit <- fits[[group]]
    data.frame(
      relative_age = fit$time,
      survival_probability = fit$surv,
      group = group
    )
  }) %>%
    bind_rows()
  
  # Plot the survival curves using ggplot2
  plot <- ggplot(surv_data, aes(x = relative_age, y = survival_probability, color = group)) +
    geom_smooth(method = "gam", formula = y ~ s(x, bs = "cs"), size = 1.2, se = FALSE) +
    geom_point(size = 0.5, alpha = 0.2, shape = 16) + 
    labs(
      title = paste("Survivorship Curves by", type_label, "Prevalence for", class_name, "(Captive)"),
      x = "Relative Age (Age/Max Longevity)",
      y = "Survival Probability",
      color = "Prevalence Group"
    ) +
    scale_color_manual(values = c("red", "blue")) +
    theme_minimal()
  
  print(plot)
}

# Example usage:
plot_prevalence_survival("Aves", threshold = 0.1, type = "malignant")
