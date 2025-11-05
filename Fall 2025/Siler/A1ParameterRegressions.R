# Load libraries
library(ggplot2)
library(dplyr)

data <- read.csv("Fall 2025/Siler/siler_parameters_all_species.csv")


# Scatterplot with linear regression line
ggplot(data, aes(x = a1, y = neoplasia_prevalence, color = Class)) +
  geom_point(alpha = 1, size = 1) +  # points
  geom_smooth(method = "lm", se = TRUE, color = "black") +   # regression line
  theme_minimal() +
  labs(
    x = "Juvenile Mortality",
    y = "Neoplasia Prevalence (%)",
    color = "Class", 
    title = "Neoplasia Prevalence vs Juvenile Mortality"
  ) +
  theme(legend.position = "bottom") 


# Run the linear model
neoplasia_model <- lm(neoplasia_prevalence ~ a1, data = data)

# See results
summary(neoplasia_model)


# Scatterplot with linear regression line
ggplot(data, aes(x = a1, y = cancer_prevalence, color = Class)) +
  geom_point(alpha = 1, size = 1) +  # points
  geom_smooth(method = "lm", se = TRUE, color = "black") +   # regression line
  theme_minimal() +
  labs(
    x = "Juvenile Mortality",
    y = "Cancer Prevalence (%)",
    color = "Class", 
    title = "Cancer Prevalence vs Juvenile Mortality"
  ) +
  theme(legend.position = "bottom") 


# Run the linear model
cancer_model <- lm(cancer_prevalence ~ a1, data = data)

# See results
summary(cancer_model)

