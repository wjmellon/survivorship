# Load libraries
library(ggplot2)
library(dplyr)
data <- read.csv("condensed_data.csv")

# Scatterplot with linear regression line and size by n
ggplot(data, aes(x = shape_value, y = cancer_prevalence, color = Class)) +
  geom_point(aes(size = n), alpha = 0.7) +  # size based on n, slightly transparent
  geom_smooth(method = "lm", se = TRUE, color = "black") +   # regression line
  scale_size_continuous(range = c(1, 10), breaks = c(100, 200, 300)) +  # scale size
  theme_minimal() +
  labs(
    x = "Survivorship",
    y = "Cancer Prevalence (%)",
    color = "Class",
    size = "Sample Size (n)",
    title = "Survivorship vs. Cancer Prevalence"
  ) +
  theme(legend.position = "bottom")

# Run the linear model
model <- lm(cancer_prevalence ~ shape_value, data = data)

library(mgcv)

# Load data
data <- read.csv("condensed_data.csv")

# Fit GAM model
model_gam <- gam(cancer_prevalence ~ s(shape_value), data = data)

# Print summary
summary(model_gam)

# Plot with GAM smooth
ggplot(data, aes(x = shape_value, y = cancer_prevalence, color = Class)) +
  geom_point(aes(size = n), alpha = 0.7) +
  geom_smooth(method = "gam", formula = y ~ s(x), se = TRUE, color = "black") +
  scale_size_continuous(range = c(1, 10), breaks = c(100, 200, 300)) +
  theme_minimal() +
  labs(
    x = "Survivorship",
    y = "Cancer Prevalence (%)",
    color = "Class",
    size = "Sample Size (n)",
    title = "Survivorship vs. Cancer Prevalence (GAM Smooth)"
  ) +
  theme(legend.position = "bottom")

# See results
summary(model)


model_quad <- lm(cancer_prevalence ~ shape_value + I(shape_value^2), data = data)

# Print full summary
summary(model_quad)

# Plot with quadratic curve
ggplot(data, aes(x = shape_value, y = cancer_prevalence, color = Class)) +
  geom_point(aes(size = n), alpha = 0.7) +
  geom_smooth(method = "lm", formula = y ~ poly(x, 2), se = TRUE, color = "black") +
  scale_size_continuous(range = c(1, 10), breaks = c(100, 200, 300, 400, 500)) +
  theme_minimal() +
  labs(
    x = "Survivorship",
    y = "Cancer Prevalence (%)",
    color = "Class",
    size = "Sample Size (n)",
    title = "Survivorship vs. Cancer Prevalence (Quadratic Fit)"
  ) +
  theme(legend.position = "bottom")

# Additional statistics
cat("\n=== MODEL COMPARISON ===\n")
model_linear <- lm(cancer_prevalence ~ shape_value, data = data)

cat("\nLinear Model R²:", round(summary(model_linear)$r.squared, 4))
cat("\nQuadratic Model R²:", round(summary(model_quad)$r.squared, 4))

cat("\n\nLinear Model AIC:", round(AIC(model_linear), 2))
cat("\nQuadratic Model AIC:", round(AIC(model_quad), 2))

cat("\n\nQuadratic Model Coefficients:\n")
print(coef(model_quad))

cat("\n\nQuadratic equation: Cancer Prevalence = ")
cat(round(coef(model_quad)[1], 4), " + ", 
    round(coef(model_quad)[2], 4), " * Survivorship + ",
    round(coef(model_quad)[3], 4), " * Survivorship²\n", sep="")