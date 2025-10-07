# Load libraries
library(dplyr)
library(ggplot2)
library(readr)

ucfirst <- function(s) {
  paste0(toupper(substr(s, 1, 1)), tolower(substr(s, 2, nchar(s))))
}

# Load dataset
data <- read_csv("records.csv")

# Define cancer types to cycle through
cancer_types <- c("melanoma", "carcinoma", "lymphoma", "adenocarcinoma", "leukemia",
                  "cholangiocarcinoma", "leiomyosarcoma", "fibrosarcoma", "sarcoma",
                  "lymphosarcoma", "hemangiosarcoma", "osteosarcoma", "myxosarcoma",
                  "liposarcoma", "mast cell tumor", "seminoma", "insulinoma",
                  "lymphangiosarcoma", "anaplastic", "mesothelioma", "cystadenocarcinoma",
                  "neurofibrosarcoma", "chondrosarcoma", "dysgerminoma", "histiocytic sarcoma",
                  "rhabdomyosarcoma", "synovial cell sarcoma", "melanosarcoma",
                  "multiple myeloma", "medulloblastoma", "astrocytoma", "plasmacytoma",
                  "Adenocarcinoma", "Leiomyosarcoma", "Fibrosarcoma", "Carcinoma",
                  "Leukemia/Lymphoma", "Lymphoma", "Neoplasia", "Sarcoma", "Melanoma",
                  "squamous cell carcinoma", "soft tissue sarcoma", "",
                  "round cell sarcoma", "undifferentiated sarcoma", "fibroadenocarcinoma",
                  "round cell tumor", "carinoma")

# Prepare an empty data frame to store results
slope_results <- data.frame(Cancer_Type = character(), Slope = numeric(), stringsAsFactors = FALSE)

for (cancer_type in cancer_types) {
  # Filter for mammals and deceased individuals (Necropsy == 1, not infants)
  mammal_data <- data %>% 
    filter(Class == "Mammalia", Necropsy == 1, Infant == 0)
  
  # Filter data for selected cancer type
  cancer_data <- mammal_data %>% 
    filter(grepl(cancer_type, Type, ignore.case = TRUE))
  
  # Count cases per species
  total_cases <- mammal_data %>% count(Species, name = "total_count") %>% filter(total_count > 70)
  cancer_cases <- cancer_data %>% count(Species, name = "cancer_count")
  
  # Compute proportions
  proportions <- cancer_cases %>% 
    left_join(total_cases, by = "Species") %>% 
    mutate(proportion = cancer_count / total_count) %>% 
    arrange(desc(proportion))
  
  # Ensure gestation data is present
  gestation_data <- data %>% select(Species, Gestation) %>% distinct()
  
  # Merge proportions with gestation data
  proportion_vs_gestation <- proportions %>% 
    left_join(gestation_data, by = "Species") %>% 
    filter(!is.na(Gestation))
  
  # Only proceed if there are at least 10 unique species
  if (n_distinct(proportion_vs_gestation$Species) >= 5) {
    # Fit linear model
    lm_model <- lm(proportion ~ Gestation, data = proportion_vs_gestation)
    slope <- coef(lm_model)[2]
  } else {
    slope <- NA  # Not enough species to compute slope
  }
  
  # Store results if valid slope
  if (!is.na(slope)) {
    slope_results <- rbind(slope_results, data.frame(Cancer_Type = cancer_type, Slope = slope, stringsAsFactors = FALSE))
  }
}

# Save slopes to CSV
write_csv(slope_results, "Mammalia Cancer Proportions/Cancer Proportions/Proportion Excel Sheets/cancer_slopes.csv")

# Plot vertical bar graph
slope_results <- slope_results %>% 
  mutate(Cancer_Type = factor(Cancer_Type, levels = rev(unique(Cancer_Type))))
slope_results <- slope_results %>% filter(!is.na(Slope))

ggplot(slope_results, aes(x = Slope, y = Cancer_Type, fill = Slope > 0)) +
  geom_bar(stat = "identity") +
  geom_vline(xintercept = 0, color = "black", linetype = "dashed") +
  scale_fill_manual(values = c("TRUE" = "blue", "FALSE" = "red")) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, size = 8))

  labs(title = "Cancer Type vs Slope of Proportion-Gestation Relationship",
       x = "Slope of Best Fit Line",
       y = "Cancer Type")

# Print top 5 slopes
print(slope_results %>% arrange(Slope) %>% head(5))
