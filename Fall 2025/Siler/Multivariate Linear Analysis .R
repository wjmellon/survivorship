library(ggplot2)
library(dplyr)
library(purrr)

# ---- Load data ----
data <- read.csv("Fall 2025/Siler/siler_parameters_all_species.csv")

# ---- Predictors and response ----
params <- c("a1", "b1", "a2", "a3", "b3")
response_var <- "neoplasia_prevalence"

# ---- Create log-transformed predictors ----
for (p in params) {
  data[[paste0("log_", p)]] <- ifelse(data[[p]] > 0, log(data[[p]]), NA)
}

all_predictors <- c(params, paste0("log_", params))

# ---- Fit multivariable linear regression (raw + log) ----
lm_all <- lm(as.formula(
  paste(response_var, "~", paste(all_predictors, collapse = " + "))
), data = data)

summary_lm <- summary(lm_all)

# ---- Print regression equation ----
coefs <- summary_lm$coefficients
eq <- paste0("neoplasia_prevalence = ", round(coefs["(Intercept)","Estimate"], 3))
for (p in rownames(coefs)[-1]) {
  eq <- paste0(eq, " + (", round(coefs[p,"Estimate"], 3), ")*", p)
}
cat("Regression Equation (raw + log predictors):\n")
cat(eq, "\n\n")

# ---- Print p-values ----
cat("P-values for each predictor:\n")
pvals <- coefs[-1,"Pr(>|t|)"]
for (p in names(pvals)) {
  cat(p, ":", signif(pvals[p], 3), "\n")
}

# ---- Function to create scatter + predicted line plot for one predictor ----
plot_predictor <- function(predictor) {
  df_clean <- data[is.finite(data[[predictor]]), ]
  
  # Hold other predictors at mean
  newdat <- df_clean %>%
    summarise(across(all_of(all_predictors), mean, na.rm = TRUE)) %>%
    slice(rep(1, 200))
  
  # Predictor grid
  newdat[[predictor]] <- seq(min(df_clean[[predictor]], na.rm = TRUE),
                             max(df_clean[[predictor]], na.rm = TRUE),
                             length.out = 200)
  
  # Predicted values from multivariable model
  newdat$predicted <- predict(lm_all, newdata = newdat)
  
  # Scatter points
  df_scatter <- df_clean %>%
    select(all_of(predictor), all_of(response_var)) %>%
    rename(x = all_of(predictor), y = all_of(response_var))
  
  # Get p-value for this predictor
  p_val <- signif(pvals[predictor], 3)
  
  # Plot
  ggplot() +
    geom_point(data = df_scatter, aes(x = x, y = y), alpha = 0.6) +
    geom_line(data = newdat, aes(x = .data[[predictor]], y = predicted), color = "red", size = 1.1) +
    labs(title = paste0("Predictor: ", predictor, " (p = ", p_val, ")"),
         x = predictor,
         y = "Neoplasia Prevalence") +
    theme_minimal(base_size = 14)
}

# ---- Generate plots for each predictor individually ----
for (p in all_predictors) {
  print(plot_predictor(p))
}
