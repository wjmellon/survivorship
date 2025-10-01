# Load libraries
library(ggplot2)
library(dplyr)
data <- read.csv("condensed_data.csv")

# Scatterplot with linear regression line and size by n
ggplot(data, aes(x = shape_value, y = cancer_prevalence, color = Class)) +
  geom_point(aes(size = n), alpha = 0.7) +  # size based on n, slightly transparent
  geom_smooth(method = "lm", se = TRUE, color = "black") +   # regression line
  scale_size_continuous(range = c(1, 10), breaks = c(100, 200, 300, 400, 500)) +  # scale size
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

# See results
summary(model)