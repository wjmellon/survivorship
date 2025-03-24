library(ggplot2)
library(dplyr)
library(survival)
library(survminer)

# Load the dataset
# Assuming your data is in a file named "records.csv"
data <- read.csv("records.csv")

# --- Define Tumor Type Lists ---
malignant_types <- c(
  "melanoma", "carcinoma", "lymphoma", "adenocarcinoma", "leukemia",
  "cholangiocarcinoma", "leiomyosarcoma", "fibrosarcoma", "sarcoma",
  "lymphosarcoma", "hemangiosarcoma", "osteosarcoma", "myxosarcoma",
  "liposarcoma", "mast cell tumor", "seminoma", "insulinoma",
  "lymphangiosarcoma", "anaplastic", "mesothelioma", "cystadenocarcinoma",
  "neurofibrosarcoma", "chondrosarcoma", "dysgerminoma", "histiocytic sarcoma",
  "rhabdomyosarcoma", "synovial cell sarcoma", "melanosarcoma", "multiple myeloma",
  "medulloblastoma", "astrocytoma", "plasmacytoma", "adenocarcinoma",
  "leiomyosarcoma", "fibrosarcoma", "carcinoma", "leukemia/lymphoma", "lymphoma",
  "neoplasia", "sarcoma", "melanoma", "squamous cell carcinoma", "soft tissue sarcoma",
  "round cell sarcoma", "undifferentiated sarcoma", "fibroadenocarcinoma",
  "round cell tumor", "carinoma"
)

# Function to compare survival curves for castrated vs. non-castrated for a specific cancer
compare_survival_by_castration <- function(data, cancer_type) {
  # Data preparation
  prep_data <- data %>%
    filter(Necropsy == 1, Infant == 0, Malignant == 1, Type == tolower(cancer_type),
           !is.na(Castrated), Castrated %in% c(0, 1)) %>%
    mutate(
      Type = tolower(trimws(Type)),
      Castrated = factor(Castrated, levels = c(0, 1),
                         labels = c("Intact", "Castrated")),
      age_months = ifelse(age_months <= 0 | max_longevity <= 0, NA, age_months),
      max_longevity = ifelse(max_longevity <= 0, NA, max_longevity),
      relative_age = ifelse(max_longevity > 0, age_months / max_longevity, NA)
    ) %>%
    filter(!is.na(relative_age))
  
  # Check if there is enough data
  if (nrow(prep_data) < 10 || length(unique(prep_data$Castrated)) < 2) {
    message(paste("Insufficient data to compare survival for", cancer_type, "and castration status."))
    return(NULL)
  }
  
  # Separate data for castrated and intact
  intact_data <- prep_data %>% filter(Castrated == "Intact")
  castrated_data <- prep_data %>% filter(Castrated == "Castrated")
  
  # Survival analysis for intact
  surv_obj_intact <- Surv(time = intact_data$relative_age, event = rep(1, nrow(intact_data)))
  surv_fit_intact <- survfit(surv_obj_intact ~ 1)
  
  # Survival analysis for castrated
  surv_obj_castrated <- Surv(time = castrated_data$relative_age, event = rep(1, nrow(castrated_data)))
  surv_fit_castrated <- survfit(surv_obj_castrated ~ 1)
  
  # Combine survival curves for plotting
  surv_data_intact <- data.frame(
    time = surv_fit_intact$time,
    survival = surv_fit_intact$surv,
    group = "Intact"
  )
  
  surv_data_castrated <- data.frame(
    time = surv_fit_castrated$time,
    survival = surv_fit_castrated$surv,
    group = "Castrated"
  )
  
  combined_surv_data <- rbind(surv_data_intact, surv_data_castrated)
  
  # Perform log-rank test
  log_rank_test <- survdiff(Surv(relative_age, rep(1, nrow(prep_data))) ~ Castrated, data = prep_data)
  p_value <- pchisq(log_rank_test$chisq, df = 1)
  
  # Generate plot
  plot_title <- paste("Survival Comparison for", cancer_type, "(Castrated vs. Intact)")
  surv_plot <- ggplot(combined_surv_data, aes(x = time, y = survival, color = group)) +
    geom_step() +
    labs(
      title = plot_title,
      x = "Relative Age",
      y = "Survival Probability",
      color = "Castration Status"
    ) +
    scale_color_manual(values = c("Intact" = "#1f77b4", "Castrated" = "#ff7f0e")) +
    theme_minimal() +  # Corrected function name
    annotate("text", x = max(combined_surv_data$time) * 0.7, y = 0.9,
             label = paste("Log-rank p-value =", format(p_value, digits = 3)))
  
  print(surv_plot)
  
  # Return the survival plot.
  return(surv_plot)
}

# Example usage:
# Replace "adenocarcinoma" with the desired cancer type
cancer_type_to_compare <- "adenocarcinoma"
survival_plot <- compare_survival_by_castration(data, cancer_type_to_compare)

# You can save the plot if needed
# ggsave(filename = paste0(cancer_type_to_compare, "_survival_plot.png"), plot = survival_plot)
