# Load libraries
library(ggplot2)
library(dplyr)
library(broom)
library(purrr)
library(tidyr)

# Scatterplot with species-specific quadratic regression lines
ggplot(data, aes(x = shape_value, y = cancer_prevalence, color = Class)) +
  geom_point(aes(size = n), alpha = 0.7) +  
  geom_smooth(method = "lm", formula = y ~ poly(x, 2), se = TRUE) +  # quadratic for each Class
  scale_size_continuous(range = c(1, 10), breaks = c(100, 200, 300, 400, 500)) +
  theme_minimal() +
  labs(
    x = "Survivorship",
    y = "Cancer Prevalence (%)",
    color = "Class",
    size = "Sample Size (n)",
    title = "Survivorship vs. Cancer Prevalence (Quadratic by Class)"
  ) +
  theme(legend.position = "bottom")

# Run quadratic models for each Class
models_quad <- data %>%
  group_by(Class) %>%
  nest() %>%
  mutate(model = map(data, ~lm(cancer_prevalence ~ shape_value + I(shape_value^2), data = .)))

# Extract tidy summaries (coefficients, p-values, etc.)
model_summaries_quad <- models_quad %>%
  mutate(summary = map(model, tidy)) %>%
  select(Class, summary) %>%
  unnest(summary)

print(model_summaries_quad)

# R² and other model statistics
model_glance_quad <- models_quad %>%
  mutate(glance = map(model, glance)) %>%
  select(Class, glance) %>%
  unnest(glance)

print(model_glance_quad)

# Compare linear vs quadratic for each Class
cat("\n=== R² COMPARISON: Linear vs Quadratic by Class ===\n")
models_linear <- data %>%
  group_by(Class) %>%
  nest() %>%
  mutate(model = map(data, ~lm(cancer_prevalence ~ shape_value, data = .)))

comparison <- models_linear %>%
  mutate(r2_linear = map_dbl(model, ~summary(.)$r.squared)) %>%
  select(Class, r2_linear) %>%
  left_join(
    models_quad %>%
      mutate(r2_quadratic = map_dbl(model, ~summary(.)$r.squared)) %>%
      select(Class, r2_quadratic),
    by = "Class"
  ) %>%
  mutate(improvement = r2_quadratic - r2_linear)

print(comparison)