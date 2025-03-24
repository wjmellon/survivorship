library(ggplot2)
library(dplyr)
library(survival)
library(survminer)

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

benign_types <- c(
  "cyst", "fibroma", "polyp", "adenoma", "lipoma", "trichoepithelioma",
  "hemangioma", "thymoma", "myxoma", "leiomyoma", "melanocytoma", "hepatoma",
  "cystadenoma", "neurofibroma", "trichoblastoma", "odontoma", "osteoma",
  "chondroma", "schwannoma", "ganglioneuroma", "myelolipoma", "rhabdomyoma",
  "adenomas", "fibrolipoma", "osteochondroma", "neurilemmoma"
)

# Function to analyze castration effects on survival
analyze_castration_effects <- function(data, malignant_types, benign_types) {
  # Data preparation
  prep_data <- data %>%
    filter(Necropsy == 1, Infant == 0, !is.na(Castrated), Castrated %in% c(0, 1)) %>%
    mutate(
      Type = tolower(trimws(Type)),
      Castrated = factor(Castrated, levels = c(0, 1),
                         labels = c("Intact", "Castrated")),
      age_months = ifelse(age_months <= 0 | max_longevity <= 0, NA, age_months),
      max_longevity = ifelse(max_longevity <= 0, NA, max_longevity),
      relative_age = ifelse(max_longevity > 0, age_months / max_longevity, NA),
      Tumor_Type_Group = case_when(
        Type %in% malignant_types ~ "Malignant",
        Type %in% benign_types ~ "Benign",
        TRUE ~ "Other"
      )
    ) %>%
    filter(!is.na(relative_age), Tumor_Type_Group %in% c("Malignant", "Benign")) # Keep only Malignant and Benign
  
  # Initialize results storage
  results <- list()
  plots <- list()
  
  # Unique cancer types (Malignant and Benign)
  unique_tumor_types <- unique(prep_data$Type)
  
  # Analysis loop
  for (tumor_type in unique_tumor_types) {
    tumor_data <- prep_data %>%
      filter(Type == tumor_type)
    
    # Check if there are enough data points for the current tumor type
    if (nrow(tumor_data) < 10 || length(unique(tumor_data$Castrated)) < 2) {
      message(paste("Skipping", tumor_type, ": Insufficient data for analysis."))
      next # Skip to the next tumor type
    }
    
    tryCatch({
      # Survival analysis
      surv_obj <- Surv(time = tumor_data$relative_age, event = rep(1, nrow(tumor_data)))
      cox_fit <- coxph(surv_obj ~ Castrated, data = tumor_data)
      surv_fit <- survfit(surv_obj ~ Castrated, data = tumor_data)
      
      # Store results
      results[[tumor_type]] <- list(
        p_value = summary(cox_fit)$coefficients["Pr(>|z|)"],
        hazard_ratio = exp(coef(cox_fit)),
        conf.lower = exp(confint(cox_fit))[1, 1],
        conf.upper = exp(confint(cox_fit))[1, 2],
        n_intact = sum(tumor_data$Castrated == "Intact"),
        n_castrated = sum(tumor_data$Castrated == "Castrated"),
        tumor_type_group = unique(tumor_data$Tumor_Type_Group) # Store the group
      )
      
      # Generate plot
      plots[[tumor_type]] <- ggsurvplot(
        surv_fit,
        data = tumor_data,
        title = paste("Castration Effect in", toupper(tumor_type)),
        pval = TRUE,
        risk.table = TRUE,
        legend.labs = levels(tumor_data$Castrated),
        palette = c("#1f77b4", "#ff7f0e"),
        ggtheme = theme_minimal()
      )
    }, error = function(e) {
      message("Skipping ", tumor_type, ": ", conditionMessage(e))
    }, finally = { # added finally
      if (exists("surv_fit")) rm(surv_fit)
      if (exists("cox_fit")) rm(cox_fit)
      if (exists("surv_obj")) rm(surv_obj)
    })
  }
  
  # Process results
  result_df <- bind_rows(
    lapply(results, function(x) {
      if (length(x$p_value) > 0 && length(x$hazard_ratio) > 0) {
        data.frame(
          p_value = x$p_value,
          hazard_ratio = x$hazard_ratio,
          conf.lower = x$conf.lower,
          conf.upper = x$conf.upper,
          n_intact = x$n_intact,
          n_castrated = x$n_castrated,
          tumor_type_group = x$tumor_type_group, # Include group
          stringsAsFactors = FALSE
        )
      } else {
        NULL
      }
    }),
    .id = "Type"
  )
  
  # Multiple testing correction
  if (nrow(result_df) > 0) {
    result_df$adj_p <- p.adjust(result_df$p_value, method = "BH")
    result_df <- result_df %>%
      arrange(adj_p) %>%
      mutate(effect_direction = case_when(
        hazard_ratio > 1 ~ "Decreased Survival",
        hazard_ratio < 1 ~ "Increased Survival",
        TRUE ~ "No Effect"
      ))
  } else {
    result_df <- data.frame()
  }
  
  # Prepare output for top effects
  top_increased <- result_df %>%
    filter(effect_direction == "Increased Survival") %>%
    arrange(hazard_ratio) %>%
    head(10)
  
  top_decreased <- result_df %>%
    filter(effect_direction == "Decreased Survival") %>%
    arrange(desc(hazard_ratio)) %>%
    head(10)
  
  list(results = result_df, plots = plots, top_increased = top_increased, top_decreased = top_decreased)
}

# Load the dataset
data <- read.csv("records.csv")

# Run analysis
analysis_results <- analyze_castration_effects(data, malignant_types, benign_types)

# Print the results
print("Analysis Results:")
print(analysis_results$results)

# Print the top increased and decreased
cat("\nTop 10 Increased Survival:\n")
if (nrow(analysis_results$top_increased) > 0) {
  print(analysis_results$top_increased)
} else {
  cat("No significant increase in survival found.\n")
}

cat("\nTop 10 Decreased Survival:\n")
if (nrow(analysis_results$top_decreased) > 0) {
  print(analysis_results$top_decreased)
} else {
  cat("No significant decrease in survival found.\n")
}

# Display the plots (optional)
# for (cancer_name in names(analysis_results$plots)) {
#   print(analysis_results$plots[[cancer_name]])
# }
