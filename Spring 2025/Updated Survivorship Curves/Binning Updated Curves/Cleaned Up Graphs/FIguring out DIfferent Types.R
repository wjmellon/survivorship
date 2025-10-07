library(ggplot2)
library(dplyr)
library(survival)
library(Rage)

data <- read.csv("records.csv")

# Compute cancer prevalence by species
species_cancer_prevalence <- data %>%
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
data <- data %>%
  left_join(species_cancer_prevalence, by = "Species")

# Function to analyze survivorship and determine type
analyze_survivorship <- function(class_name, type = "malignant", threshold = 0.001) {
  if (type == "malignant") {
    data <- data %>%
      mutate(prevalence_group = factor(
        ifelse(malignant_prevalence >= threshold, "High Malignant Prevalence", "Low Malignant Prevalence"),
        levels = c("Low Malignant Prevalence", "High Malignant Prevalence")
      ))
    type_label <- "Malignant"
  } else if (type == "benign") {
    data <- data %>%
      mutate(prevalence_group = factor(
        ifelse(benign_prevalence >= threshold, "High Benign Prevalence", "Low Benign Prevalence"),
        levels = c("Low Benign Prevalence", "High Benign Prevalence")
      ))
    type_label <- "Benign"
  }
  
  prep_data <- data %>%
    filter(Necropsy == 1, Infant == 0, Class == class_name) %>%
    select(age_months = 3, max_longevity = 48, prevalence_group) %>%
    mutate(
      across(c(age_months, max_longevity), ~ ifelse(.x <= 0, NA, .x))
    ) %>%
    na.omit() %>%
    mutate(relative_age = age_months / max_longevity)
  
  # Compute survivorship curves
  surv_results <- prep_data %>%
    group_by(prevalence_group) %>%
    group_split() %>%
    setNames(unique(prep_data$prevalence_group)) %>%
    lapply(function(df) {
      if (nrow(df) < 5) return(NULL)  # Ignore groups with <5 data points
      survfit(Surv(relative_age, rep(1, nrow(df))) ~ 1, data = df)
    })
  
  surv_results <- surv_results[!sapply(surv_results, is.null)]
  
  surv_data <- lapply(names(surv_results), function(group) {
    fit <- surv_results[[group]]
    df <- data.frame(
      relative_age = c(0, fit$time),  
      survival_probability = c(1, fit$surv), 
      group = group
    )
    
    # Replace any zero values with a small nonzero value
    df$survival_probability[df$survival_probability == 0] <- 1e-6
    return(df)
  }) %>%
    bind_rows()
  
  # Check if we have enough nonzero survival probabilities for shape_surv()
  if (nrow(surv_data) > 2 && sum(surv_data$survival_probability > 0) > 2) {
    survivorship_types <- surv_data %>%
      group_by(group) %>%
      summarise(
        shape_type = shape_surv(survival_probability, trunc = TRUE),
        .groups = "drop"
      ) %>%
      mutate(
        survivorship_type = case_when(
          shape_type >= 0.3 ~ "Type I (late mortality, senescence)",
          shape_type >= 0.1 & shape_type < 0.3 ~ "Trending toward Type I",
          shape_type > -0.1 & shape_type < 0.1 ~ "Type II (constant mortality)",
          shape_type > -0.3 & shape_type <= -0.1 ~ "Trending toward Type III",
          shape_type <= -0.3 ~ "Type III (early mortality)"
        )
      )
  } else {
    survivorship_types <- tibble(
      group = unique(surv_data$group),
      shape_type = NA,
      survivorship_type = "Insufficient Data"
    )
  }
  
  print(survivorship_types)
  
  # Plot survivorship curves
  plot <- ggplot(surv_data, aes(x = relative_age, y = survival_probability, color = group)) +
    geom_smooth(method = "gam", formula = y ~ s(x, bs = "cs"), size = 1.2, se = FALSE) +
    geom_point(size = 0.5, alpha = 0.3) +
    labs(
      title = paste("Survivorship Curves by", type_label, "Prevalence for", class_name),
      x = "Relative Age (Age/Max Longevity)",
      y = "Survival Probability",
      color = "Prevalence Group"
    ) +
    scale_color_manual(values = c("red", "blue")) +
    theme_minimal() +
    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
    geom_text(
      data = survivorship_types,
      aes(x = 0.9, y = 0.5, label = paste0(group, ": ", survivorship_type)),
      inherit.aes = FALSE,
      hjust = 0,
      size = 4
    )
  
  print(plot)
}

# Iterate through all classes and analyze survivorship
unique_classes <- unique(data$Class)
for (class in unique_classes) {
  analyze_survivorship(class, type = "malignant", threshold = 0.1)
  analyze_survivorship(class, type = "benign", threshold = 0.1)
}
