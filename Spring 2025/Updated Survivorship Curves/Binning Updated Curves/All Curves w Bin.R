library(ggplot2)
library(dplyr)
library(survival)
library(survminer)

data <- read.csv("records.csv")

# Compute cancer prevalence by species
species_cancer_prevalence <- data %>%
  filter(Necropsy == 1, Infant == 0) %>%
  group_by(Species) %>%
  summarise(
    total_individuals = n(),
    malignant_cases = sum(Malignant == 1, na.rm = TRUE),
    benign_cases = sum(Malignant == 0, na.rm = TRUE),
    neoplasia_cases = sum(!is.na(Malignant), na.rm = TRUE),
    malignant_prevalence = malignant_cases / total_individuals,
    benign_prevalence = benign_cases / total_individuals,
    neoplasia_prevalence = neoplasia_cases / total_individuals
  )

# Merge back to main dataset
data <- data %>%
  left_join(species_cancer_prevalence, by = "Species")

# New function to overlay all prevalence types on one plot
plot_all_prevalence_survival <- function(class_name, max_relative_age = 1.5, threshold = 0.1, smooth = FALSE) {
  cancer_types <- c("malignant", "benign", "neoplasia")
  
  survival_data <- lapply(cancer_types, function(type) {
    prevalence_column <- paste0(type, "_prevalence")
    
    data %>%
      mutate(prevalence_group = ifelse(.data[[prevalence_column]] >= threshold, "High", "Low")) %>%
      filter(Necropsy == 1, Infant == 0, Class == class_name) %>%
      select(age_months = 3, max_longevity = 48, prevalence_group) %>%
      mutate(
        cancer_type = tools::toTitleCase(type),
        across(c(age_months, max_longevity), ~ ifelse(.x <= 0, NA, .x))
      ) %>%
      na.omit() %>%
      mutate(
        relative_age = age_months / max_longevity,
        group = paste(cancer_type, prevalence_group)
      ) %>%
      filter(relative_age <= max_relative_age)
  }) %>% bind_rows()
  
  # Fit survival curves for each group
  surv_obj <- Surv(survival_data$relative_age, rep(1, nrow(survival_data)))
  fit <- survfit(surv_obj ~ group, data = survival_data)
  
  # Plot all curves together
  plot <- ggsurvplot(
    fit,
    data = survival_data,
    xlab = "Relative Age (Age / Max Longevity)",
    ylab = "Survival Probability",
    title = paste("Survivorship Curves by Cancer Prevalence Type for", class_name),
    legend.title = "Cancer Type & Prevalence",
    palette = c(
      "Malignant Low" = "#1f77b4",
      "Malignant High" = "#ff7f0e",
      "Benign Low" = "#2ca02c",
      "Benign High" = "#d62728",
      "Neoplasia Low" = "#9467bd",
      "Neoplasia High" = "#8c564b"
    ),
    xlim = c(0, max_relative_age),
    break.x.by = 0.25,
    risk.table = TRUE,
    risk.table.height = 0.25,
    ggtheme = theme_minimal(),
    tables.theme = theme_cleantable()
  )
  
  if (smooth) {
    plot$plot <- plot$plot + geom_smooth(aes(color = strata), method = "loess", se = FALSE)
    plot$plot <- plot$plot + ggtitle(paste("Smoothed Survivorship Curves for", class_name, "\n(Max Relative Age:", max_relative_age, ")"))
  }
  
  print(plot)
}

# Example usage
plot_all_prevalence_survival("Mammalia", threshold = 0.1)
