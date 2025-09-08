# Install if you haven’t yet
install.packages("readxl")

# Load package
library(readxl)

# Read Excel file
df <- read_excel("DataSheet w All Species Survivorship Curves (1).xlsx")

# Type 1 subset
df_type1 <- subset(df, curve_type == "Type I")

# Type 2 subset
df_type2 <- subset(df, curve_type == "Type II")

# Type 3 subset
df_type3 <- subset(df, curve_type == "Type III")


species_list <- split(df$`Scientific Name`, df$`Curve Type`)

# Now you can access:
species_list[["Type I"]]
species_list[["Type II"]]
species_list[["Type III"]]

# Load the dataset
data <- read.csv("records.csv")

library(dplyr)


# Merge the datasets  
    merged <- data %>%
      left_join(df, by = c("Species" = "Scientific Name")) %>%
      mutate(Cancer_Status = case_when(
        Malignant %in% c(0, 1) ~ "Yes",
        Malignant == -1 ~ "No",
        TRUE ~ NA_character_
      ))

# Calculate Cancer Prevalence 
    library(dplyr)
    
    cancer_prevalence <- merged %>%
      group_by(`Curve Type`) %>%
      summarise(
        total = n(),
        cancer_yes = sum(Cancer_Status == "Yes", na.rm = TRUE),
        cancer_no  = sum(Cancer_Status == "No", na.rm = TRUE),
        prevalence = cancer_yes / total
      )
    
    cancer_prevalence

    
    library(ggplot2)
    library(dplyr)
    
 # Filter only Type 1, Type 2, Type 3
cancer_prevalence_filtered <- cancer_prevalence %>%
   filter(`Curve Type` %in% c("Type I", "Type II", "Type III")) %>%
   mutate(prevalence = as.numeric(prevalence))  # convert to numeric
              
cancer_prevalence_filtered
              

# Plot
    ggplot(cancer_prevalence_filtered, aes(x = `Curve Type`, y = mean_prev)) +
      geom_bar(stat = "identity", fill = "steelblue") +
      geom_text(aes(label = round(mean_prev, 3)), vjust = -0.5) +  # use mean_prev here
      labs(
        title = "Cancer Prevalence by Survivorship Type",
        x = "Survivorship Type",
        y = "Proportion with Cancer"
      ) + ylim(0, 1) +
      theme_minimal()
    
    
# Perform one-way ANOVA
    anova_result <- aov(prevalence ~ `Curve Type`, data = cancer_prevalence_filtered)
    
# View summary
    summary(anova_result)
    
    