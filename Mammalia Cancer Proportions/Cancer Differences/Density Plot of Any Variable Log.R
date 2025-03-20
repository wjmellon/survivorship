library(ggplot2)
library(dplyr)
library(readr)
library(ggrepel)
library(stringr)

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

# Function to create analysis plot
create_cancer_plot <- function(variable_name) {
  # Clean and classify cancer types
  mammal_data <- data %>%
    mutate(
      Type = tolower(Type),
      Type = case_when(
        Type %in% malignant_types ~ "malignant",
        Type %in% benign_types ~ "benign",
        TRUE ~ NA_character_
      )
    ) %>%
    filter(!is.na(Type), 
           Class == "Mammalia",
           !!sym(variable_name) != -1) %>%  # Correct filter syntax
    group_by(Species) %>%
    filter(n() > 30) %>%
    ungroup()
  
  # Calculate cancer proportions
  cancer_proportions <- mammal_data %>%
    count(Species, Type) %>%
    pivot_wider(names_from = Type, values_from = n, values_fill = 0) %>%
    mutate(
      total = benign + malignant,
      CancerDifference = (malignant/total) - (benign/total)
    ) %>%
    select(Species, CancerDifference)
  
  # Merge with variable data
  variable_data <- mammal_data %>%
    select(Species, !!sym(variable_name)) %>%
    distinct() %>%
    filter(!is.na(!!sym(variable_name)))
  
  plot_data <- cancer_proportions %>%
    inner_join(variable_data, by = "Species")
  
  # Create plot
  model <- lm(CancerDifference ~ log10(get(variable_name)), data = plot_data)  # Fix here
  r_sq <- summary(model)$r.squared
  p_val <- summary(model)$coefficients[2,4]
  slope <- coef(model)[2] %>% round(3)
  intercept <- coef(model)[1] %>% round(3)
  
  top_species <- plot_data %>%
    arrange(desc(CancerDifference)) %>%
    head(20)
  
  ggplot(plot_data, aes(x = get(variable_name), y = CancerDifference)) +  # Fix here
    geom_density_2d(color = "gray30", linewidth = 0.3, alpha = 0.8) +
    geom_point(shape = 21, fill = "gray40", color = "black", 
               size = 2.5, alpha = 0.7, stroke = 0.3) +
    geom_smooth(method = "lm", color = "darkblue", 
                linewidth = 0.8, fill = "gray80") +
    geom_text_repel(data = top_species, aes(label = Species),
                    size = 3, box.padding = 0.3) +
    scale_x_log10() +  # Logarithmic scale for biological variables
    annotate("text", x = min(plot_data[[variable_name]]), 
             y = max(plot_data$CancerDifference),
             label = sprintf("y = %.3f + %.3flog(x)\nR² = %.2f\np = %.3f",
                             intercept, slope, r_sq, p_val),
             hjust = 0, vjust = 1, size = 3.5) +
    theme_bw() +
    labs(x = paste("Log10(", str_to_title(variable_name), ")"),
         y = "Malignant-Benign Cancer Proportion Difference",
         title = paste("Cancer Difference vs", str_to_title(variable_name)))
}

# Usage example (change variable name as needed)
create_cancer_plot("birth_weight")  # Try "max_longevity", "adult_weight", etc.


# purrr::walk(variables_to_plot, ~print(create_cancer_plot(.x)))
