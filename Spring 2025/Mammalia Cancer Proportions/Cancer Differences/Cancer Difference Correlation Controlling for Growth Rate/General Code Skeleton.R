library(mgcv)
library(broom)
library(dplyr)
library(car)
library(ggplot2)  # Added for plotting

# Load dataset
data <- read_csv("records.csv")

# Define and clean cancer categories
malignant_types <- c(
  "melanoma", "carcinoma", "lymphoma", "adenocarcinoma", "leukemia",
  "cholangiocarcinoma", "leiomyosarcoma", "fibrosarcoma", "sarcoma",
  "lymphosarcoma", "hemangiosarcoma", "osteosarcoma", "myxosarcoma",
  "liposarcoma", "mast cell tumor", "seminoma", "insulinoma",
  "lymphangiosarcoma", "anaplastic", "mesothelioma", "cystadenocarcinoma",
  "neurofibrosarcoma", "chondrosarcoma", "dysgerminoma", "histiocytic sarcoma",
  "rhabdomyosarcoma", "synovial cell sarcoma", "melanosarcoma",
  "multiple myeloma", "medulloblastoma", "astrocytoma", "plasmacytoma",
  "squamous cell carcinoma", "soft tissue sarcoma", "round cell sarcoma",
  "undifferentiated sarcoma", "fibroadenocarcinoma", "round cell tumor"
) %>% tolower() %>% unique() %>% 
  str_replace("carinoma", "carcinoma")

benign_types <- c(
  "cyst", "fibroma", "polyp", "adenoma", "lipoma", "trichoepithelioma",
  "hemangioma", "thymoma", "myxoma", "leiomyoma", "melanocytoma",
  "hepatoma", "cystadenoma", "neurofibroma", "trichoblastoma", "odontoma",
  "osteoma", "chondroma", "schwannoma", "ganglioneuroma", "myelolipoma",
  "rhabdomyoma", "fibrolipoma", "osteochondroma", "neurilemmoma"
) %>% tolower() %>% unique()

analyze_controlled <- function(variable_name) {
  # Prepare data with proper cancer difference calculation
  analysis_data <- data %>%
    mutate(
      Type = tolower(Type),
      Type = case_when(
        Type %in% malignant_types ~ "malignant",
        Type %in% benign_types ~ "benign",
        TRUE ~ NA_character_
      )
    ) %>%
    filter(Class == "Mammalia",
           !!sym(variable_name) != -1,
           growth_rate != -1) %>%
    group_by(Species) %>%
    filter(n() > 30) %>%
    ungroup() %>%
    # Calculate cancer difference
    group_by(Species) %>%
    summarise(
      malignant = sum(Type == "malignant", na.rm = TRUE),
      benign = sum(Type == "benign", na.rm = TRUE),
      total = n(),
      CancerDifference = (malignant/total) - (benign/total),
      growth_rate = first(growth_rate),
      !!sym(variable_name) := first(!!sym(variable_name))
    ) %>%
    select(Species, CancerDifference, growth_rate, !!sym(variable_name)) %>%
    drop_na()
  
  # Fit controlled GAM
  formula <- as.formula(paste("CancerDifference ~ growth_rate + s(", variable_name, ")"))
  model <- gam(formula, data = analysis_data)
  
  # Plot residuals
  plot(residuals(model), main = paste("Residuals of Model for", variable_name), xlab = "Fitted values", ylab = "Residuals")
  
  # Extract statistics
  smooth_p <- summary(model)$s.table[1,4]  # P-value for smooth term
  growth_p <- summary(model)$p.table[2,4]  # P-value for growth rate
  adj_r2 <- summary(model)$r.sq
  
  return(tibble(
    Variable = variable_name,
    Growth_Rate_P = growth_p,
    Variable_P = smooth_p,
    Adj_R2 = adj_r2
  ))
}

# Example usage
gestation_results <- analyze_controlled("Gestation")
metabolic_results <- analyze_controlled("metabolic_rate")

variables_to_test <- c("birth_weight", "weaning_weight", "adult_weight", "Gestation", "Infancy",
               "litter_size", "litters_year", "interbirth_interval", "female_maturity",
               "male_maturity", "growth_rate", "max_longevity", "metabolic_rate", "Weaning")
results <- map_dfr(variables_to_test, analyze_controlled)

results %>%
  select(Variable, Growth_Rate_P, Variable_P, Adj_R2) %>%
  mutate(across(where(is.numeric), ~round(., 3)))

# Perform VIF check for multicollinearity
vif(lm(CancerDifference ~ Gestation + metabolic_rate + adult_weight + max_longevity, data = data))
