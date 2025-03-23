library(ggplot2)
library(dplyr)
library(survival)
library(survminer)

# Load the dataset
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

# Filter the dataset for Mammals with Necropsy data, exclude infants, and include only malignant cases
cutdata <- data %>%
  filter(Necropsy == 1, Class == "Mammalia", Infant == 0, Malignant == 1)

# Select relevant columns by number and rename Diagnosis column
cutdata <- cutdata[, c(3, 28, 48, 18, 19)]
colnames(cutdata) <- c("age_months", "Species", "max_longevity", "Malignant", "Type")

# Clean data: remove individuals with invalid ages and longevity
cutdata <- cutdata %>%
  mutate(age_months = ifelse(age_months <= 0, NA, age_months),
         max_longevity = ifelse(max_longevity <= 0, NA, max_longevity)) %>%
  na.omit()

# Create a relative_age column: individual age divided by species-specific maximum longevity
cutdata <- cutdata %>%
  mutate(relative_age = age_months / max_longevity)

# Clean and standardize tumor type names
cutdata$Type <- tolower(cutdata$Type)

# Create a column to indicate if a diagnosis is a specific malignant type
cutdata <- cutdata %>%
  mutate(specific_malignant = ifelse(Type %in% malignant_types, Type, "other"))

# Function to compare individual malignant types to all malignant
compare_malignant_survival <- function(age_limit = NULL) {
  
  # Filter data for all malignant
  all_malignant_data <- cutdata
  
  if (!is.null(age_limit)) {
    all_malignant_data <- all_malignant_data %>%
      filter(relative_age <= age_limit)
  }
  
  # Create survival object for all malignant
  surv_obj_all_malignant <- Surv(all_malignant_data$relative_age, rep(1, nrow(all_malignant_data)))
  
  # Fit Kaplan-Meier model for all malignant
  km_fit_all_malignant <- survfit(surv_obj_all_malignant ~ 1)
  
  # Get unique malignant types
  unique_malignant_types <- unique(cutdata$specific_malignant)
  
  # Store results in a data frame
  results_df <- data.frame(Malignant_Type = character(), P_Value = numeric(), stringsAsFactors = FALSE)
  
  # Perform comparisons
  for (malignant_type in unique_malignant_types) {
    
    # Filter data for the specific malignant type
    type_data <- cutdata %>%
      filter(specific_malignant == malignant_type)
    
    if (!is.null(age_limit)) {
      type_data <- type_data %>%
        filter(relative_age <= age_limit)
    }
    
    # Create a survival object
    surv_obj_type <- Surv(type_data$relative_age, rep(1, nrow(type_data)))
    
    # Fit Kaplan-Meier model
    km_fit_type <- survfit(surv_obj_type ~ 1)
    
    # Perform log-rank test (compare to all malignant)
    combined_data <- data.frame(
      relative_age = c(type_data$relative_age, all_malignant_data$relative_age),
      group = c(rep(malignant_type, nrow(type_data)), rep("All Malignant", nrow(all_malignant_data)))
    )
    
    log_rank_test <- survdiff(Surv(relative_age) ~ group, data = combined_data)
    
    p_value <- pchisq(log_rank_test$chisq, df = 1)
    
    # Add result to data frame
    results_df <- rbind(results_df, data.frame(Malignant_Type = malignant_type, P_Value = p_value))
  }
  
  # Sort by p-value (most significant first)
  results_df <- results_df %>% arrange(P_Value)
  
  # Print the top 10
  cat("Top 10 Most Statistically Different Malignant Types:\n")
  if (nrow(results_df) > 10) {
    print(results_df[1:10, ])
  } else {
    print(results_df)
  }
}

# Example usage:
compare_malignant_survival()
compare_malignant_survival(age_limit = 1.5)
compare_malignant_survival(age_limit = 0.5)
