# Load libraries
library(ggplot2)
library(dplyr)


data <- final_clean_mortality_data %>%
  mutate(abs_shape = abs(shape_value))


# Neoplasia vs Absolute Value of Survivorship
ggplot(data, aes(x = abs_shape, y = neoplasia_prevalence, color = Class)) +
  geom_point(alpha = 1, size = 1) +
  geom_smooth(method = "lm", se = TRUE, color = "black") +
  theme_minimal() +
  labs(
    x = "|Survivorship|",
    y = "Neoplasia Prevalence (%)",
    color = "Class",
    title = "Neoplasia Prevalence vs. Absolute Value of Survivorship"
  ) +
  theme(legend.position = "bottom")

# Run the linear model
neoplasia_model <- lm(neoplasia_prevalence ~ abs_shape, data = data)

# See results
summary(neoplasia_model)




# Cancer vs Absolute Value of Survivorship 
ggplot(data, aes(x = abs_shape, y = cancer_prevalence, color = Class)) +
  geom_point(alpha = 1, size = 1) +
  geom_smooth(method = "lm", se = TRUE, color = "black") +
  theme_minimal() +
  labs(
    x = "|Survivorship|",
    y = "Cancer Prevalence (%)",
    color = "Class",
    title = "Cancer Prevalence vs. Absolute Value of Survivorship"
  ) +
  theme(legend.position = "bottom")

# Run the linear model
cancer_model <- lm(cancer_prevalence ~ abs_shape, data = data)

# See results
summary(cancer_model)

