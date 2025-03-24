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

# Function to plot survivorship curves
plot_malignant_survival <- function(cancer_type_to_plot = "adenocarcinoma",
                                    animal_class = "Mammalia",
                                    age_limit = NULL) {
  
  # Filter the dataset for the specified animal class
  class_data <- data %>%
    filter(Necropsy == 1, Class == animal_class, Infant == 0, Malignant == 1)
  
  # Select relevant columns and rename
  class_data <- class_data[, c(3, 28, 48, 18, 19)]
  colnames(class_data) <- c("age_months", "Species", "max_longevity", "Malignant", "Type")
  
  # Clean the data
  class_data <- class_data %>%
    mutate(age_months = ifelse(age_months <= 0, NA, age_months),
           max_longevity = ifelse(max_longevity <= 0, NA, max_longevity)) %>%
    na.omit()
  
  # Create relative_age
  class_data <- class_data %>%
    mutate(relative_age = age_months / max_longevity)
  
  # Clean and standardize tumor type names
  class_data$Type <- tolower(class_data$Type)
  
  # Create a column for specific malignant type
  class_data <- class_data %>%
    mutate(specific_malignant = ifelse(Type %in% malignant_types, Type, "other"))
  
  # Filter data for the specified cancer type
  cancer_data <- class_data %>%
    filter(specific_malignant == tolower(cancer_type_to_plot))
  
  # Filter data for all malignant within the class
  all_malignant_data <- class_data
  
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
    data = class_data, # Use class_data
    title = paste("Relative Age at Death:", cancer_type_to_plot, "in", animal_class, "vs. All Malignant"),
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
plot_malignant_survival(cancer_type_to_plot = "carcinoma", animal_class = "Aves")
plot_malignant_survival(cancer_type_to_plot = "sarcoma", animal_class = "Aves", age_limit = 2)
