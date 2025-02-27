  # Load necessary libraries
  library(dplyr)
  library(ggplot2)
  library(corrplot)
  library(readr)
  
  # Define cancer categories
  malignant_types <- c(
    "melanoma", "carcinoma", "lymphoma", "adenocarcinoma", "leukemia",
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
    "round cell tumor", "carinoma"
  )
  
  benign_types <- c(
    "cyst", "fibroma", "polyp", "adenoma", "lipoma", "trichoepithelioma",
    "hemangioma", "thymoma", "myxoma", "leiomyoma", "melanocytoma",
    "hepatoma", "cystadenoma", "neurofibroma", "trichoblastoma", "odontoma",
    "osteoma", "chondroma", "schwannoma", "ganglioneuroma", "myelolipoma",
    "rhabdomyoma", "adenomas", "fibrolipoma", "osteochondroma", "neurilemmoma"
  )
  
  # Load dataset (assuming your dataset is named "records.csv")
  data <- read_csv("records.csv")
  
  # Filter for mammals and deceased individuals (Necropsy == 1, not infants)
  mammal_data <- data %>% 
    filter(Class== "Mammalia",Necropsy == 1, Infant == 0)
  
  # Count total cancer cases per species
  total_cases <- mammal_data %>% count(Species, name = "total_count") %>% filter(total_count > 50)
  
  # Count malignant and benign cases per species
  malignant_cases <- mammal_data %>%
    filter(grepl(paste(malignant_types, collapse = "|"), Type, ignore.case = TRUE)) %>%
    count(Species, name = "malignant_count")
  
  benign_cases <- mammal_data %>%
    filter(grepl(paste(benign_types, collapse = "|"), Type, ignore.case = TRUE)) %>%
    count(Species, name = "benign_count")
  
  # Compute proportions
  malignant_proportions <- malignant_cases %>%
    left_join(total_cases, by = "Species") %>%
    mutate(malignant_proportion = malignant_count / total_count) %>%
    select(Species, malignant_proportion)
  
  benign_proportions <- benign_cases %>%
    left_join(total_cases, by = "Species") %>%
    mutate(benign_proportion = benign_count / total_count) %>%
    select(Species, benign_proportion)
  
  # Merge malignant and benign proportions
  cancer_proportions <- full_join(malignant_proportions, benign_proportions, by = "Species") %>%
    replace_na(list(malignant_proportion = 0, benign_proportion = 0))
  
  # Compute cancer difference
  cancer_differences <- cancer_proportions %>%
    mutate(cancer_difference = malignant_proportion - benign_proportion,
           absolute_difference = abs(cancer_difference))  # Absolute difference for bubble size
  
  # Select relevant biological variables
  biological_variables <- c("birth_weight", "weaning_weight", "adult_weight", "Gestation", "Infancy", 
                            "litter_size", "litters_year", "interbirth_interval", "female_maturity", 
                            "male_maturity", "growth_rate", "max_longevity", "metabolic_rate", "Weaning")
  
  # Combine cancer metrics with biological variables
  combined_data <- mammal_data %>%
    select(Species, all_of(biological_variables)) %>%
    distinct() %>%
    left_join(cancer_differences, by = "Species")
  
  # Remove any rows with NAs (optional, depending on your dataset)
  combined_data <- combined_data %>%
    drop_na()
  
  # Select relevant columns (cancer metrics and biological variables)
  selected_data <- combined_data %>%
    select(cancer_difference, malignant_proportion, benign_proportion, 
           birth_weight, weaning_weight, adult_weight, Gestation, Infancy, 
           litter_size, litters_year, interbirth_interval, female_maturity, 
           male_maturity, growth_rate, max_longevity, metabolic_rate, Weaning)
  
  # Compute the correlation matrix for selected variables
  correlation_matrix <- cor(selected_data, use = "complete.obs")
  
  # Plot the correlation heatmap using 'corrplot'
  corrplot(correlation_matrix, method = "color", 
           type = "full",  # Show entire matrix
           tl.col = "black",  # Color for the labels
           title = "Correlation Heatmap: Cancer Proportions/Differences and Biological Variables",
           mar = c(0, 0, 3, 0))  # Adjust margin for title
  
  
  # Compute correlation for each biological variable with cancer metrics
  correlations <- sapply(selected_data[, -1], function(x) cor(x, selected_data$cancer_difference, use = "complete.obs"))
  correlations_df <- data.frame(Variable = names(correlations), Correlation = correlations)
  # 
  # # Create bar plot of correlation strengths
  # ggplot(correlations_df, aes(x = reorder(Variable, Correlation), y = Correlation, fill = Correlation < 0)) +
  #   geom_bar(stat = "identity") +
  #   scale_fill_manual(values = c("TRUE" = "red", "FALSE" = "steelblue")) +  # Red for negative, blue for positive
  #   coord_flip() +
  #   labs(title = "Correlation Strength Between Cancer Metrics and Biological Variables",
  #        x = "Biological Variables", y = "Correlation with Cancer Difference") +
  #   theme_minimal()
