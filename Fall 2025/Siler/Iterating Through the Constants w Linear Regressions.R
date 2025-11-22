library(dplyr)
library(ggplot2)

data <- read.csv("Fall 2025/Siler/siler_parameters_all_species.csv")

params <- c("a1", "b1", "a2", "a3", "b3")

############################################################
# FUNCTION: Extract slope + p-value
############################################################
extract_results <- function(model) {
  coef_summary <- summary(model)$coefficients
  slope <- coef_summary[2, "Estimate"]
  pvalue <- coef_summary[2, "Pr(>|t|)"]
  return(c(slope = slope, pvalue = pvalue))
}

############################################################
# PART 1 — RAW PARAMETER LINEAR MODELS
############################################################

raw_results_neoplasia <- data.frame(Parameter = params, Slope = NA, P_value = NA)
raw_results_cancer    <- data.frame(Parameter = params, Slope = NA, P_value = NA)

for (p in params) {
  model1 <- lm(as.formula(paste("neoplasia_prevalence ~", p)), data = data)
  res1 <- extract_results(model1)
  raw_results_neoplasia[raw_results_neoplasia$Parameter == p, 2:3] <- res1
  
  model2 <- lm(as.formula(paste("cancer_prevalence ~", p)), data = data)
  res2 <- extract_results(model2)
  raw_results_cancer[raw_results_cancer$Parameter == p, 2:3] <- res2
}

############################################################
# PART 2 — LOG-PARAMETER MODELS
############################################################

for (p in params) {
  data[[paste0("log_", p)]] <- log(data[[p]])
}

log_params <- paste0("log_", params)

log_results_neoplasia <- data.frame(Parameter = log_params, Slope = NA, P_value = NA)
log_results_cancer    <- data.frame(Parameter = log_params, Slope = NA, P_value = NA)

for (p in log_params) {
  model1 <- lm(as.formula(paste("neoplasia_prevalence ~", p)), data = data)
  res1 <- extract_results(model1)
  log_results_neoplasia[log_results_neoplasia$Parameter == p, 2:3] <- res1
  
  model2 <- lm(as.formula(paste("cancer_prevalence ~", p)), data = data)
  res2 <- extract_results(model2)
  log_results_cancer[log_results_cancer$Parameter == p, 2:3] <- res2
}

############################################################
# PRINT SUMMARY TABLES
############################################################

cat("\n===== RAW PARAMETER REGRESSION RESULTS: NEOPLASIA =====\n")
print(raw_results_neoplasia)

cat("\n===== RAW PARAMETER REGRESSION RESULTS: CANCER =====\n")
print(raw_results_cancer)

cat("\n===== LOG PARAMETER REGRESSION RESULTS: NEOPLASIA =====\n")
print(log_results_neoplasia)

cat("\n===== LOG PARAMETER REGRESSION RESULTS: CANCER =====\n")
print(log_results_cancer)

############################################################
# PART 3 — IDENTIFY SIGNIFICANT MODELS (p < 0.05)
############################################################

sig_neoplasia_raw <- raw_results_neoplasia %>% filter(P_value < 0.05)
sig_cancer_raw    <- raw_results_cancer %>% filter(P_value < 0.05)

sig_neoplasia_log <- log_results_neoplasia %>% filter(P_value < 0.05)
sig_cancer_log    <- log_results_cancer %>% filter(P_value < 0.05)

cat("\n===== SIGNIFICANT MODELS (RAW) — NEOPLASIA =====\n")
print(sig_neoplasia_raw)

cat("\n===== SIGNIFICANT MODELS (RAW) — CANCER =====\n")
print(sig_cancer_raw)

cat("\n===== SIGNIFICANT MODELS (LOG) — NEOPLASIA =====\n")
print(sig_neoplasia_log)

cat("\n===== SIGNIFICANT MODELS (LOG) — CANCER =====\n")
print(sig_cancer_log)

############################################################
# PART 4 — PLOT SIGNIFICANT MODELS
############################################################

# Helper function to make annotated plot
plot_with_stats <- function(df, xvar, yvar, slope, pvalue) {
  
  ggplot(df, aes_string(x = xvar, y = yvar, color = "Class")) +
    geom_point(alpha = 0.9, size = 2) +
    geom_smooth(method = "lm", se = TRUE, color = "black") + 
    theme_minimal() +
    labs(
      title = paste("Regression:", yvar, "vs", xvar),
      subtitle = paste0("Slope = ", round(slope, 4),
                        "   |   p = ", signif(pvalue, 3)),
      x = xvar,
      y = yvar,
      color = "Class"
    ) +
    theme(
      plot.title = element_text(size = 14, face = "bold"),
      legend.position = "bottom"
    )
}

### Plot loop for all significant findings ###

# RAW — Neoplasia
for (i in 1:nrow(sig_neoplasia_raw)) {
  p <- sig_neoplasia_raw$Parameter[i]
  slope <- sig_neoplasia_raw$Slope[i]
  pvalue <- sig_neoplasia_raw$P_value[i]
  print(plot_with_stats(data, p, "neoplasia_prevalence", slope, pvalue))
}

# RAW — Cancer
for (i in 1:nrow(sig_cancer_raw)) {
  p <- sig_cancer_raw$Parameter[i]
  slope <- sig_cancer_raw$Slope[i]
  pvalue <- sig_cancer_raw$P_value[i]
  print(plot_with_stats(data, p, "cancer_prevalence", slope, pvalue))
}

# LOG — Neoplasia
for (i in 1:nrow(sig_neoplasia_log)) {
  p <- sig_neoplasia_log$Parameter[i]
  slope <- sig_neoplasia_log$Slope[i]
  pvalue <- sig_neoplasia_log$P_value[i]
  print(plot_with_stats(data, p, "neoplasia_prevalence", slope, pvalue))
}

# LOG — Cancer
for (i in 1:nrow(sig_cancer_log)) {
  p <- sig_cancer_log$Parameter[i]
  slope <- sig_cancer_log$Slope[i]
  pvalue <- sig_cancer_log$P_value[i]
  print(plot_with_stats(data, p, "cancer_prevalence", slope, pvalue))
}