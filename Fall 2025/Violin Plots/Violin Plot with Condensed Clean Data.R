library(ggplot2)
library(dplyr)

# Optional: Load your data
condensed_clean_data <- read.csv("Fall 2025/Fall 2025 Clean Data/final_clean_data.csv")

# Ensure factor levels for consistent order and coloring
condensed_clean_data$survivorship_type <- factor(
  condensed_clean_data$survivorship_type,
  levels = c("Type III", "Trending toward Type III", "Type II", "Trending toward Type I", "Type I")
)


# Custom fill colors for each group
custom_colors <- c(
  "Type III" = "#1f77b4",                  # blue
  "Trending toward Type III" = "#2ca02c",  # green
  "Type II" = "#ff7f0e",                   # orange
  "Trending toward Type I" = "#9467bd",    # purple
  "Type I" = "#8c564b"                     # brown 
)

# Plot for Cancer Prevalence by Survivorship Curve Type
ggplot(condensed_clean_data, aes(x = survivorship_type, y = cancer_prevalence, fill = survivorship_type)) +
  geom_violin(trim = FALSE, alpha = 0.9, color = "black") +
  geom_jitter(width = 0.10, alpha = 0.4, size = 1.8, color = "black") +
  stat_summary(fun = median, geom = "point", color = "red", size = 3, shape = 21, fill = "white", stroke = 1.5) +
  scale_fill_manual(values = custom_colors) +
  theme_minimal(base_size = 14) +
  labs(
    title = "Cancer Prevalence by Survivorship Curve Type",
    x = "Survivorship Curve Type",
    y = "Cancer Prevalence"
  ) +
  scale_y_continuous(breaks = c(-0.5, 0, 0.5, 1.0),
                     labels = c("-0.5", "0", "0.5", "1.0")) +
  theme(
    axis.text.x = element_text(angle = 0, vjust = 0.5, hjust = 0.5),  # straight x-axis labels
    axis.line.x = element_line(size = 1.2, color = "black"),          # bold x-axis line
    axis.line.y = element_line(size = 1.2, color = "black"),          # bold y-axis line
    axis.ticks = element_line(size = 1),                              # bold tick marks
    legend.position = "none",
    panel.grid = element_blank(),                                     # optional: remove grid
    panel.border = element_blank()
  )

# Save File
ggsave(filename='Cancer Violin Plot.png', width=10, height=8, limitsize=FALSE,bg="white")


# Plot for Neoplasia Prevalence by Survivorship Curve Type
ggplot(condensed_clean_data, aes(x = survivorship_type, y = neoplasia_prevalence, fill = survivorship_type)) +
  geom_violin(trim = FALSE, alpha = 0.9, color = "black") +
  geom_jitter(width = 0.10, alpha = 0.4, size = 1.8, color = "black") +
  stat_summary(fun = median, geom = "point", color = "red", size = 3, shape = 21, fill = "white", stroke = 1.5) +
  scale_fill_manual(values = custom_colors) +
  theme_minimal(base_size = 14) +
  labs(
    title = "Neoplasia Prevalence by Survivorship Curve Type",
    x = "Survivorship Curve Type",
    y = "Neoplasia Prevalence"
  ) +
  theme(
    axis.text.x = element_text(angle = 0, vjust = 0.5, hjust = 0.5),
    axis.line.x = element_line(size = 1.2, color = "black"),
    axis.line.y = element_line(size = 1.2, color = "black"),
    axis.ticks = element_line(size = 1),
    legend.position = "none",
    panel.grid = element_blank(),
    panel.border = element_blank()
  )

# Save File
ggsave(filename='Neoplasia Violin Plot.png', width=10, height=8, limitsize=FALSE,bg="white")

# Count the number of species per survivorship category
category_counts <- condensed_clean_data %>%
  group_by(survivorship_type) %>%
  summarise(num_species = n()) %>%
  arrange(desc(num_species))

# Print the counts with a message
for(i in seq_len(nrow(category_counts))) {
  cat(category_counts$survivorship_type[i], "has", category_counts$num_species[i], "species\n")
}

