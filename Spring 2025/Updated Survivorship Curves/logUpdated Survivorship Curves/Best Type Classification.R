# Load required libraries
library(ggplot2)
library(dplyr)
library(cowplot)
library(survival) # For survival analysis

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

benign_types <- c(
  "cyst", "fibroma", "polyp", "adenoma", "lipoma", "trichoepithelioma",
  "hemangioma", "thymoma", "myxoma", "leiomyoma", "melanocytoma", "hepatoma",
  "cystadenoma", "neurofibroma", "trichoblastoma", "odontoma", "osteoma",
  "chondroma", "schwannoma", "ganglioneuroma", "myelolipoma", "rhabdomyoma",
  "adenomas", "fibrolipoma", "osteochondroma", "neurilemmoma"
)

# Filter the dataset for Mammals with Necropsy data and exclude infants
cutdata <- data %>%
  filter(Necropsy == 1) %>%
  filter(Class == "Mammalia") %>%
  filter(Infant == 0)

# Select relevant columns (Species, age_months, max_longevity, Malignancy, Diagnosis)
cutdata <- cutdata[, c(3, 28, 48, 18, 19)] # Adjust column index for Diagnosis if needed
colnames(cutdata)[5] <- "Diagnosis" # Rename the column to "Diagnosis"

# Clean data: remove individuals with invalid ages and longevity
cutdata$age_months[cutdata$age_months <= 0] <- NA
cutdata$max_longevity[cutdata$max_longevity <= 0] <- NA
cutdata <- na.omit(cutdata)

# Create a relative_age column: individual age divided by species-specific maximum longevity
cutdata <- cutdata %>%
  mutate(relative_age = age_months / max_longevity)

# Clean and standardize tumor type names
cutdata$Diagnosis <- tolower(cutdata$Diagnosis)

# --- Overall Malignant Survivorship ---

# Filter for malignant cases
malignant_mammals <- cutdata %>% filter(Malignant == 1)

# Define relative age time steps (0 to 1 by increments of 0.01)
time_steps <- seq(0, 1, by = 0.01)

# Calculate survival for malignant group (number of individuals alive at each time step)
malignant_alive <- sapply(time_steps, function(x) sum(malignant_mammals$relative_age > x))

# Create a data frame for malignant plotting
alive_data_malignant <- data.frame(
  relative_age = time_steps,
  proportion_alive_malignant = malignant_alive / max(malignant_alive)
)

# --- Analysis of Individual Tumor Types ---

# Create a column to indicate if a diagnosis is a specific malignant type
cutdata$specific_malignant <- ifelse(cutdata$Diagnosis %in% malignant_types, cutdata$Diagnosis, "other")

# Get a list of specific malignant tumor types with at least 20 cases
tumor_counts <- cutdata %>%
  filter(specific_malignant != "other") %>%
  group_by(specific_malignant) %>%
  summarize(count = n()) %>%
  filter(count >= 20)

significant_results <- data.frame(
  Tumor_Type = character(),
  Number_of_Cases = integer(),
  P_value = numeric(),
  Significance = character(),
  stringsAsFactors = FALSE
)

# Loop through each specific tumor type and compare to overall malignant
for (tumor_type in tumor_counts$specific_malignant) {
  # Filter data for this tumor type
  tumor_data <- cutdata %>%
    filter(specific_malignant == tumor_type | specific_malignant == "other")
  
  # Create a survival object
  surv_obj <- Surv(tumor_data$relative_age, tumor_data$Malignant)
  
  # Fit a Kaplan-Meier model
  km_fit <- survfit(surv_obj ~ specific_malignant, data = tumor_data)
  
  # Perform a log-rank test
  log_rank <- survdiff(surv_obj ~ specific_malignant, data = tumor_data)
  
  # Extract p-value
  p_value <- pchisq(log_rank$chisq, length(log_rank$n) - 1, lower.tail = FALSE)
  
  # Determine significance
  significance <- ifelse(p_value < 0.05, "Significant", "Not Significant")
  
  # Add results to the table
  significant_results <- rbind(significant_results, data.frame(
    Tumor_Type = tumor_type,
    Number_of_Cases = tumor_counts$count[tumor_counts$specific_malignant == tumor_type],
    P_value = p_value,
    Significance = significance
  ))
}

# --- Results Table Output ---

print(significant_results)