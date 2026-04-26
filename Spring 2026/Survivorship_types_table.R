library(dplyr)

data <- read.csv("Spring 2026/final_clean_data_w_multivariate.csv")

survivorship_table <- data %>%
  summarise(
    Species,
    n,
    survivorship_type
  )

data$survivorship_type <- factor(data$survivorship_type, levels = c("Type I", "Trending toward Type I", "Type II", "Trending toward Type III", "Type III"))

survivorship_counts <- data %>%
  group_by(Survivorship_Type = survivorship_type) %>%
  summarise(Number_of_Species = n())

write.csv(survivorship_counts, "Spring 2026/survivorship_counts.csv", row.names = FALSE)