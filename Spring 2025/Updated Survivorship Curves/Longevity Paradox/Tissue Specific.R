library(survival)
library(survminer)
library(dplyr)
library(scales)

# Filter and group rare tissues
tissue_data <- data %>%
  filter(Necropsy == 1, Infant == 0) %>%
  mutate(
    relative_age = ifelse(age_months > 0 & max_longevity > 0, 
                          age_months/max_longevity, NA),
    Malignant = as.numeric(Malignant),
    Tissue = ifelse(Tissue %in% names(sort(table(Tissue), decreasing = TRUE)[1:9]),
                    as.character(Tissue), "Other Tissues")
  ) %>%
  na.omit() %>%
  group_by(Tissue) %>%
  filter(n() >= 10) %>%
  ungroup()

# Create survival object
surv_obj <- Surv(tissue_data$relative_age, tissue_data$Malignant)

# Generate color palette based on number of tissues
n_tissues <- length(unique(tissue_data$Tissue))
color_palette <- hue_pal()(n_tissues)

# Plot with dynamic coloring
ggsurvplot(survfit(surv_obj ~ Tissue, data = tissue_data),
           data = tissue_data,
           title = "Tissue-Specific Cancer Development Patterns",
           xlab = "Relative Age (Age/Max Longevity)",
           ylab = "Cancer-Free Survival Probability",
           legend.title = "Tissue Type",
           legend.labs = levels(factor(tissue_data$Tissue)),
           risk.table = TRUE,
           pval = TRUE,
           pval.method = TRUE,
           conf.int = FALSE,  # Remove CI for clarity
           ncensor.plot = FALSE,  # Remove censor marks
           palette = color_palette,
           ggtheme = theme_minimal(base_size = 12),
           tables.theme = theme_cleantable(),
           tables.height = 0.3,
           legend = "right") +
  labs(subtitle = "Showing 8 most common tissues + 'Other' (≥10 cases each)")



summary(survfit(surv_obj ~ Tissue, data = tissue_data))$table

# # Add proper capitalization and space
round_cell_comparisons <- pairwise_survdiff(
  Surv(relative_age, Malignant) ~ Tissue,
  data = tissue_data %>% 
    mutate(Tissue = relevel(factor(Tissue), ref = "Round Cell")),
  p.adjust.method = "BH"
)