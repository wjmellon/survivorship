library(ggplot2)
library(dplyr)
library(survival)
library(survminer)
data <- read.csv("records.csv")

plot_class_survival <- function(class_name, max_relative_age = 1.5, smooth = FALSE) {
  # Data Preparation
  prep_data <- data %>%
    filter(Necropsy == 1, Infant == 0, Class == class_name) %>%
    select(age_months = 3, Castrated = 5, Class = 24, 
           max_longevity = 48, Malignant = 18) %>%
    mutate(
      across(c(age_months, max_longevity), ~ ifelse(.x <= 0, NA, .x)),
      Castrated = as.numeric(Castrated),
      Malignant = as.numeric(Malignant)
    ) %>%
    na.omit() %>%
    mutate(relative_age = age_months / max_longevity) %>%
    filter(relative_age <= max_relative_age)
  
  # Create comparison groups (now 5 groups)
  groups <- list(
    "All Individuals" = prep_data,
    "Malignant Cancer" = filter(prep_data, Malignant == 1),
    "Castrated" = filter(prep_data, Castrated == 1),
    "Malignant & Castrated" = filter(prep_data, Malignant == 1, Castrated == 1),
    "Malignant & Not Castrated" = filter(prep_data, Malignant == 1, Castrated == 0)
  )
  
  # Create survival objects
  fits <- lapply(groups, function(df) {
    if(nrow(df) == 0) return(NULL)
    survfit(Surv(relative_age, rep(1, nrow(df))) ~ 1, data = df)
  })
  
  # Remove empty groups
  fits <- fits[!sapply(fits, is.null)]
  
  # Generate plot with adjusted palette
  plot <- ggsurvplot_combine(
    fits,
    data = prep_data,
    title = paste("Survivorship Curves for", class_name, "\n(Max Relative Age:", max_relative_age, ")"),
    xlab = "Relative Age (Age/Max Longevity)",
    ylab = "Survival Probability",
    legend.title = "Group",
    legend.labs = names(fits),
    palette = c("#1f77b4", "#ff7f0e", "#2ca02c", "#d62728", "#9467bd"), # 5-color palette
    risk.table = TRUE,
    xlim = c(0, max_relative_age),
    break.x.by = 0.25,
    risk.table.height = 0.25,
    ggtheme = theme_minimal(),
    tables.theme = theme_cleantable()
  )
  
  if (smooth) {
    plot$plot <- plot$plot + geom_smooth(aes(color = strata), method = "loess", se = FALSE)
    plot$plot <- plot$plot + ggtitle(paste("Smoothed Survivorship Curves for", class_name, "\n(Max Relative Age:", max_relative_age, ")")) # changes the title.
  }
  
  print(plot)
}

# Example usage:
plot_class_survival("Mammalia") # unsmoothed plot
