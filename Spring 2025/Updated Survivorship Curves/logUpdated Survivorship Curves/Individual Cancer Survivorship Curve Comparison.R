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

# Function to plot survivorship curves
plot_malignant_survival <- function(cancer_type_to_plot = "adenocarcinoma", age_limit = NULL) { # Added cancer_type argument
  
  # Filter data for the specified cancer type
  cancer_data <- cutdata %>%
    filter(specific_malignant == tolower(cancer_type_to_plot)) # Make it case-insensitive
  
  # Filter data for all malignant
  all_malignant_data <- cutdata
  
  if (!is.null(age_limit)) {
    cancer_data <- cancer_data %>%
      filter(relative_age <= age_limit)
    all_malignant_data <- all_malignant_data %>%
      filter(relative_age <= age_limit)
  }
  
  # Create survival objects
  surv_obj_cancer <- Surv(cancer_data$relative_age, rep(1, nrow(cancer_data)))
  surv_obj_all_malignant <- Surv(all_malignant_data$relative_age, rep(1, nrow(all_malignant_data)))
  
  # Fit Kaplan-Meier models
  km_fit_cancer <- survfit(surv_obj_cancer ~ 1)
  km_fit_all_malignant <- survfit(surv_obj_all_malignant ~ 1)
  
  # Create a list of the fits
  fit_list <- list(cancer_type = km_fit_cancer, all_malignant = km_fit_all_malignant)
  
  # Plot the survivorship curves together
  p <- ggsurvplot_combine(
    fit_list,
    data = cutdata, # Pass the original data
    title = paste("Relative Age at Death:", cancer_type_to_plot, "vs. All Malignant"),
    xlab = "Relative Age",
    ylab = "Proportion Dead",
    legend.title = "Cancer Type",
    legend.labs = c(cancer_type_to_plot, "All Malignant"),
    palette = c("blue", "red"),
    risk.table = TRUE,
    risk.table.y.text = FALSE
  )
  print(p)
  if (!is.null(age_limit)) {
    print(paste("Graph limited to relative age <= ", age_limit))
  }
}

# Example usage:
plot_malignant_survival(cancer_type_to_plot = "carcinoma") # To plot Carcinoma
plot_malignant_survival(cancer_type_to_plot = "sarcoma", age_limit = 1.5) # With age limit
