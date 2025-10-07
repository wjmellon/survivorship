# Load required libraries
library(ggplot2)
library(dplyr)
library(cowplot)

# Define survivorship function models
type1_model <- function(x, a, b) { 1 / (1 + exp(a * (x - b))) }  # Logistic (Type I)
type2_model <- function(x, a) { exp(-a * x) }  # Exponential decay (Type II)
type3_model <- function(x, a, b) { 1 / (1 + (a * x)^b) }  # Power-law decay (Type III)

# Fit models using nonlinear least squares
fit_type1 <- nls(proportion_alive_all ~ type1_model(age, a, b), data = alive_data, start = list(a = 0.01, b = 50))
fit_type2 <- nls(proportion_alive_all ~ type2_model(age, a), data = alive_data, start = list(a = 0.01))
fit_type3 <- nls(proportion_alive_all ~ type3_model(age, a, b), data = alive_data, start = list(a = 0.01, b = 0.5))

# Get AIC scores
aic_values <- data.frame(
  Model = c("Type I (Logistic)", "Type II (Exponential)", "Type III (Power-Law)"),
  AIC = c(AIC(fit_type1), AIC(fit_type2), AIC(fit_type3))
)

# Identify the best model
best_model <- aic_values$Model[which.min(aic_values$AIC)]

# Create survivorship plot
ggplot(alive_data, aes(x = age)) +
  geom_smooth(aes(y = proportion_alive_all, color = "All Reptiles"), method = "gam", formula = y ~ s(x, bs = "cs"), se = FALSE, size = 1) +
  geom_smooth(aes(y = proportion_alive_malignant_manual, color = "Malignant"), method = "gam", formula = y ~ s(x, bs = "cs"), se = FALSE, size = 1) +
  geom_smooth(aes(y = proportion_alive_benign, color = "Benign Tumors"), method = "gam", formula = y ~ s(x, bs = "cs"), se = FALSE, size = 1) +
  scale_color_manual(values = c(
    "All Reptiles" = "blue",
    "Malignant" = "red",
    "Benign Tumors" = "green"
  )) +
  ggtitle(paste("Survivorship Curve (Best Model: ", best_model, ")", sep = "")) +  # Add best model to title
  xlab("Age (months)") +
  ylab("Proportion Alive (log scale)") +
  scale_y_log10() +
  annotate("text", x = max(alive_data$age) * 0.7, y = 0.1, label = paste("Best Fit:", best_model), size = 5, color = "black") +
  theme_cowplot(12)
