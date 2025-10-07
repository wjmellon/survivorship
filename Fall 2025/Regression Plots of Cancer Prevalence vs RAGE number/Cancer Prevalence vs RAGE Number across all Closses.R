# Load libraries
library(ggplot2)
library(dplyr)
library(broom)
library(purrr)
library(tidyr)

# Scatterplot with species-specific regression lines
ggplot(data, aes(x = shape_value, y = cancer_prevalence, color = Class)) +
  geom_point(alpha = 1, size = 1) +  
  geom_smooth(method = "lm", se = TRUE) +
  theme_minimal() +
  labs(
    x = "Survivorship",
    y = "Cancer Prevalence (%)",
    color = "Class", 
    title = "Survivorship vs. Cancer Prevalence (by Class)"
  ) +
  theme(legend.position = "bottom")

# Run linear models for each Class
models <- data %>%
  group_by(Class) %>%
  nest() %>%
  mutate(model = map(data, ~lm(cancer_prevalence ~ shape_value, data = .)))

# Extract tidy summaries (coefficients, p-values, etc.)
model_summaries <- models %>%
  mutate(summary = map(model, tidy)) %>%
  select(Class, summary) %>%
  unnest(summary)

print(model_summaries)

# R² and other model statistics
model_glance <- models %>%
  mutate(glance = map(model, glance)) %>%
  select(Class, glance) %>%
  unnest(glance)

print(model_glance)