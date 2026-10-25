library(ggplot2)
library(dplyr)

dir.create("results/figures", recursive = TRUE, showWarnings = FALSE)

class_colors <- c(Amphibia = "#F8766D", Aves = "#7CAE00",
                  Mammalia = "#00BFC4", Reptilia = "#C77CFF")
type_levels <- c("Type II", "Trending toward Type I", "Type I")

prevalence <- read.csv("data/species-cancer-prevalence-data.csv", check.names = FALSE) %>%
  mutate(Species = gsub(" ", "_", Species)) %>%
  distinct(Species, .keep_all = TRUE) %>%
  select(Species, Class,
         neoplasia_prevalence = NeoplasiaPrevalence,
         cancer_prevalence = MalignancyPrevalence)

plot_data <- read.csv("results/rage_shape_values.csv") %>%
  mutate(Species = gsub(" ", "_", Species)) %>%
  filter(survivorship_type %in% type_levels) %>%
  inner_join(prevalence, by = "Species") %>%
  mutate(survivorship_type = factor(survivorship_type, levels = type_levels),
         Class = factor(Class))

violin <- function(yvar, ylab, title, file) {
  p <- ggplot(plot_data, aes(x = survivorship_type, y = .data[[yvar]])) +
    geom_violin(trim = FALSE, alpha = 0.9, fill = "lightgray", color = "black") +
    geom_jitter(aes(color = Class), width = 0.10, alpha = 0.7, size = 1.8) +
    stat_summary(fun = median, geom = "crossbar", width = 0.4, fatten = 0.5,
                 color = "black", linewidth = 2.5) +
    scale_color_manual(values = class_colors,
                       guide = guide_legend(override.aes = list(shape = 16, size = 3, alpha = 1))) +
    coord_cartesian(xlim = c(0.2, length(type_levels) + 0.8), ylim = c(0, 1),
                    expand = FALSE, clip = "on") +
    scale_y_continuous(breaks = c(0, 0.5, 1), labels = c("0", "50", "100")) +
    labs(title = title, x = "Survivorship Curve Type", y = ylab, color = "Class") +
    theme_minimal(base_size = 14) +
    theme(axis.line = element_line(linewidth = 1.2, color = "black"),
          axis.ticks = element_line(linewidth = 1),
          panel.grid = element_blank())
  ggsave(file.path("results/figures", file), p, width = 11, height = 8, bg = "white")
}

violin("cancer_prevalence", "Cancer Prevalence (%)",
       "Cancer Prevalence by Survivorship Curve Type", "violin_cancer.png")
violin("neoplasia_prevalence", "Neoplasia Prevalence (%)",
       "Neoplasia Prevalence by Survivorship Curve Type", "violin_neoplasia.png")

summary_table <- plot_data %>%
  group_by(survivorship_type) %>%
  summarise(n_species = n(),
            median_neoplasia_prevalence = median(neoplasia_prevalence, na.rm = TRUE),
            median_cancer_prevalence = median(cancer_prevalence, na.rm = TRUE))
write.csv(summary_table, "results/violin_summary.csv", row.names = FALSE)
