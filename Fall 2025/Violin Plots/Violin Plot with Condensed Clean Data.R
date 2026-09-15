library(ggplot2)
library(dplyr)

# Optional: Load your data
condensed_clean_data <- read.csv("Fall 2025/Fall 2025 Clean Data/final_clean_data.csv")

# Ensure factor levels for consistent order
condensed_clean_data$survivorship_type <- factor(
  condensed_clean_data$survivorship_type,
  levels = c("Type III", "Trending toward Type III", "Type II", "Trending toward Type I", "Type I")
)

condensed_clean_data$Class <- factor(condensed_clean_data$Class)

class_colors <- c(
  "Mammalia" = "purple",
  "Amphibia" = "blue",
  "Reptilia" = "darkgreen",
  "Aves"     = "red"
)

# Order the fill colors to match the legend's actual key order (factor levels of Class),
# so the black-outlined legend dots always line up with the right class regardless
# of how Class's levels happen to be ordered.
legend_fill <- class_colors[levels(condensed_clean_data$Class)]

legend_guide <- guide_legend(
  override.aes = list(
    shape  = 21,             # hollow-with-fill circle, so a border can show
    colour = "black",        # black outline, legend only
    fill   = legend_fill,    # keeps the same class colors inside the outline
    size   = 3,
    alpha  = 1
  )
)

# -------------------------------------------------------------------------
# Plot 1: Cancer Prevalence by Survivorship Curve Type
# -------------------------------------------------------------------------
ggplot(condensed_clean_data, aes(x = survivorship_type, y = cancer_prevalence)) +
  # Violins are set to a static gray color
  geom_violin(trim = FALSE, alpha = 0.9, fill = "lightgray", color = "black") +
  # Jitter points are now colored by the 'Class' column
  geom_jitter(aes(color = Class), width = 0.10, alpha = 0.7, size = 1.8) +
  # Median point
  stat_summary(fun = median, geom = "point", color = "black", size = 3, shape = 21, fill = "white", stroke = 1.5) +
  # Median line across each violin
  stat_summary(fun = median, geom = "crossbar", width = 0.4, fatten = 0, 
               color = "black", linewidth = 0.7) +
  scale_color_manual(values = class_colors, guide = legend_guide) +
  theme_minimal(base_size = 14) +
  labs(
    title = "Cancer Prevalence by Survivorship Curve Type",
    x = "Survivorship Curve Type",
    y = "Cancer Prevalence (%)",
    color = "Class"
  ) +
  scale_y_continuous(breaks = c(-0.5, 0, 0.5, 1.0),
                     labels = c("-50", "0", "50", "100")) +
  theme(
    axis.text.x = element_text(angle = 0, vjust = 0.5, hjust = 0.5),  
    axis.line.x = element_line(linewidth = 1.2, color = "black"),          
    axis.line.y = element_line(linewidth = 1.2, color = "black"),          
    axis.ticks = element_line(linewidth = 1),                             
    legend.position = "right", 
    panel.grid = element_blank(),                                     
    panel.border = element_blank()
  )

# Save File
ggsave(filename='Cancer Violin Plot.png', width=11, height=8, limitsize=FALSE, bg="white")


# -------------------------------------------------------------------------
# Plot 2: Neoplasia Prevalence by Survivorship Curve Type
# -------------------------------------------------------------------------
ggplot(condensed_clean_data, aes(x = survivorship_type, y = neoplasia_prevalence)) +
  # Violins are set to a static gray color
  geom_violin(trim = FALSE, alpha = 0.9, fill = "lightgray", color = "black") +
  # Jitter points are now colored by the 'Class' column
  geom_jitter(aes(color = Class), width = 0.10, alpha = 0.7, size = 1.8) +
  # Median point
  stat_summary(fun = median, geom = "point", color = "black", size = 3, shape = 21, fill = "white", stroke = 1.5) +
  # Median line across each violin
  stat_summary(fun = median, geom = "crossbar", width = 0.4, fatten = 0, 
               color = "black", linewidth = 0.7) +
  scale_color_manual(values = class_colors, guide = legend_guide) +
  theme_minimal(base_size = 14) +
  labs(
    title = "Neoplasia Prevalence by Survivorship Curve Type",
    x = "Survivorship Curve Type",
    y = "Neoplasia Prevalence (%)",
    color = "Class"
  ) +
  scale_y_continuous(breaks = c(-0.5, 0, 0.5, 1.0),
                     labels = c("-50", "0", "50", "100")) +
  theme(
    axis.text.x = element_text(angle = 0, vjust = 0.5, hjust = 0.5),
    axis.line.x = element_line(linewidth = 1.2, color = "black"),
    axis.line.y = element_line(linewidth = 1.2, color = "black"),
    axis.ticks = element_line(linewidth = 1),
    legend.position = "right", 
    panel.grid = element_blank(),
    panel.border = element_blank()
  )

# Save File
ggsave(filename='Neoplasia Violin Plot.png', width=11, height=8, limitsize=FALSE, bg="white")


# -------------------------------------------------------------------------
# Species Counts Per Category
# -------------------------------------------------------------------------
category_counts <- condensed_clean_data %>%
  group_by(survivorship_type) %>%
  summarise(num_species = n()) %>%
  arrange(desc(num_species))

for(i in seq_len(nrow(category_counts))) {
  cat(category_counts$survivorship_type[i], "has", category_counts$num_species[i], "species\n")
}

# -------------------------------------------------------------------------
# Median Prevalence Per Survivorship Type (console output)
# -------------------------------------------------------------------------
median_summary <- condensed_clean_data %>%
  group_by(survivorship_type) %>%
  summarise(
    median_cancer_prevalence    = median(cancer_prevalence, na.rm = TRUE),
    median_neoplasia_prevalence = median(neoplasia_prevalence, na.rm = TRUE)
  )

cat("======================\n")
cat("Median Cancer Prevalence by Survivorship Type\n")
cat("======================\n")
for (i in seq_len(nrow(median_summary))) {
  cat(sprintf("  - %-30s : %.4f\n",
              median_summary$survivorship_type[i],
              median_summary$median_cancer_prevalence[i]))
}

cat("\n======================\n")
cat("Median Neoplasia Prevalence by Survivorship Type\n")
cat("======================\n")
for (i in seq_len(nrow(median_summary))) {
  cat(sprintf("  - %-30s : %.4f\n",
              median_summary$survivorship_type[i],
              median_summary$median_neoplasia_prevalence[i]))
  
  
  
  
  
}



