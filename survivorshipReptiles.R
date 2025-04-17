library(ggplot2)
library(dplyr)
library(survival)
library(survminer)
library(devtools)
install_github("jonesor/rage")

library(Rage) # Load the Rage library

# Load the dataset

data <- read.csv("records.csv")

# Compute cancer prevalence by species
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

# Function to calculate survivorship type using RAGE
get_surv_type_rage <- function(relative_age_vector) {
  if (length(relative_age_vector) < 2) {
    return(NA)
  }
  time_steps <- seq(0, 1, by = 0.01)
  alive_counts <- sapply(time_steps, function(x) {
    sum(relative_age_vector > x, na.rm = TRUE)
  })
  lx_raw <- alive_counts / max(alive_counts, na.rm = TRUE)
  
  # Ensure that lx values are finite and replace zeroes
  lx <- ifelse(lx_raw == 0, min(lx_raw[lx_raw > 0], na.rm = TRUE) / 2, lx_raw)
  if (all(is.na(lx)) || any(!is.finite(lx))) {
    return(NA)
  }
  
  # Calculate survivorship shape type
  shape_type <- tryCatch({
    shape_surv(lx)
  }, error = function(e) {
    message(paste("Error in shape_surv:", e$message))
    return(NA)
  })
  
  # Determine survivorship type based on the shape type
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

# Function to plot survival based on cancer prevalence groups with threshold and RAGE type
plot_prevalence_survival_rage <- function(class_name, max_relative_age = 1.5, smooth = FALSE, threshold = 0.1, type = "malignant", risk_table = TRUE) {
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
  } else if (type == "neoplasia") {
    data <- data %>%
      mutate(prevalence_group = factor(
        ifelse(neoplasia_prevalence >= threshold, "High Neoplasia Prevalence", "Low Neoplasia Prevalence"),
        levels = c("Low Neoplasia Prevalence", "High Neoplasia Prevalence")
      ))
  }
  
  # Filter the data for the specific class and remove NA values
  prep_data <- data %>%
    filter(Necropsy == 1, Infant == 0, Class == class_name) %>%
    select(age_months, Species, max_longevity, prevalence_group) %>%
    mutate(
      across(c(age_months, max_longevity), ~ ifelse(.x <= 0, NA, .x))
    ) %>%
    na.omit() %>%
    group_by(Species) %>%
    mutate(relative_age = age_months / max_longevity) %>%
    ungroup() %>%
    filter(relative_age <= max_relative_age)
  
  # Calculate RAGE survivorship type for each prevalence group
  rage_types <- prep_data %>%
    group_by(prevalence_group) %>%
    summarise(relative_ages = list(relative_age)) %>%
    mutate(rage_type = sapply(relative_ages, get_surv_type_rage))
  
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
    xlab = "Relative Age (Age / Max Longevity)",
    ylab = "Survival Probability",
    legend.title = "Prevalence Group",
    legend.labs = names(fits),
    palette = c("#1f77b4", "#ff7f0e"),
    risk.table = risk_table,
    pval = TRUE,
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
  
  # Print RAGE survivorship types
  cat(paste("\nRAGE Survivorship Curve Types for", class_name, "by", tools::toTitleCase(type), "Prevalence:\n"))
  for (i in 1:nrow(rage_types)) {
    cat(paste0(rage_types$prevalence_group[i], ": ", rage_types$rage_type[i], "\n"))
  }
}

# Example usage:
plot_prevalence_survival_rage("Mammalia", max_relative_age = 1, threshold = 0.1, type = "neoplasia")
