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
    malignant_prevalence = malignant_cases / total_individuals,
    benign_prevalence = benign_cases / total_individuals
  )

# Merge back to main dataset
data <- data %>%
  left_join(species_cancer_prevalence, by = "Species")

# Function to plot survival based on cancer prevalence groups with threshold
plot_prevalence_survival <- function(class_name, max_relative_age = 1.5, smooth = FALSE, threshold = 0.1, type = "malignant") {
  # Choose classification type
  if (type == "malignant") {
    data <- data %>%
      mutate(prevalence_group = factor(
        ifelse(malignant_prevalence >= threshold, "High Malignant Prevalence", "Low Malignant Prevalence"),
        levels = c("Low Malignant Prevalence", "High Malignant Prevalence")
      ))
  } else if (type == "benign") {
    data <- data %>%
      mutate(prevalence_group = factor(
        ifelse(benign_prevalence >= threshold, "High Benign Prevalence", "Low Benign Prevalence"),
        levels = c("Low Benign Prevalence", "High Benign Prevalence")
      ))
  }
  
  # Filter the data for the specific class and remove NA values
  prep_data <- data %>%
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
  
  # Generate plot
  plot <- ggsurvplot_combine(
    fits,
    data = prep_data,
    title = paste("Survivorship Curves by", type, "Prevalence for", class_name),
    xlab = "Relative Age (Age/Max Longevity)",
    ylab = "Survival Probability",
    legend.title = "Prevalence Group",
    legend.labs = names(fits),
    palette = c("#1f77b4", "#ff7f0e"), # Two-color palette for high/low prevalence
    risk.table = TRUE,
    xlim = c(0, max_relative_age),
    break.x.by = 0.25,
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

# Example usage:
plot_prevalence_survival("Mammalia", threshold = 0.1, type = "benign")
