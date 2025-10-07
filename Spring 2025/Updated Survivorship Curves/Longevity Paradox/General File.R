library(ggplot2)
library(dplyr)

# Filter and prepare data
longevity_data <- data %>%
  filter(Malignant == 1, Necropsy == 1, Infant == 0) %>%
  mutate(
    max_longevity = ifelse(max_longevity <= 0, NA, max_longevity),
    age_months = ifelse(age_months <= 0, NA, age_months),
    relative_age = age_months / max_longevity
  ) %>%
  na.omit()

# Create visualization
ggplot(longevity_data, aes(x = max_longevity, y = relative_age)) +
  geom_point(alpha = 0.6, size = 2.5, color = "#2c7bb6") +
  geom_smooth(method = "loess", color = "#d7191c", se = TRUE) +
  labs(
    title = "Longevity Paradox in Cancer Development",
    subtitle = "Relationship between Species Longevity and Relative Age at Cancer Diagnosis",
    x = "Maximum Species Longevity (Months)",
    y = "Relative Age at Diagnosis (Age/Longevity)",
    caption = "Data limited to malignant cases with necropsy confirmation"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold", size = 14),
    axis.title = element_text(size = 12))
