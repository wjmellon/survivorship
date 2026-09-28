library(dplyr)

data <- read.csv("Spring 2026/final_clean_data_w_multivariate.csv")

data <- data %>%
  mutate(
    survivorship_type = factor(survivorship_type, levels = c(
      "Type I", "Trending toward Type I", "Type II", 
      "Trending toward Type III", "Type III"
    ))
  )


survivorship_counts <- data %>%
  group_by(survivorship_type) %>%
  summarise(
    Number_of_Species = n(),
    Number_of_Mammalian_Species = sum(Class == "Mammalia", na.rm = TRUE),
    Number_of_Reptilian_Species = sum(Class == "Reptilia", na.rm = TRUE),
    Number_of_Amphibian_Species = sum(Class == "Amphibia", na.rm = TRUE),
    Number_of_Avian_Species     = sum(Class == "Aves", na.rm = TRUE),
    .groups = "drop"
  )


write.csv(survivorship_counts, "Spring 2026/survivorship_counts.csv", row.names = FALSE)
