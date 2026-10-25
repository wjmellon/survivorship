library(ggplot2)
library(dplyr)
library(cowplot)

dir.create("results/figures", recursive = TRUE, showWarnings = FALSE)

data <- read.csv("data/cleanPath.min20.062822.csv")
rage <- read.csv("results/rage_shape_values.csv") %>%
  select(Species, survivorship_type)

type_levels <- c("Type I", "Trending toward Type I", "Type II",
                 "Trending toward Type III", "Type III")
type_colors <- c("Type I" = "#8c564b",
                 "Trending toward Type I" = "#9467bd",
                 "Type II" = "#ff7f0e",
                 "Trending toward Type III" = "#2ca02c",
                 "Type III" = "#1f77b4")
time_steps <- seq(0, 1, by = 0.01)

build_curves <- function(class_name) {
  cut <- data %>%
    filter(Necropsy == 1, Infant == 0, Class == class_name) %>%
    transmute(Species, age_months = as.numeric(age_months)) %>%
    filter(!is.na(age_months), age_months > 0) %>%
    group_by(Species) %>%
    filter(n() >= 20) %>%
    mutate(relative_age = age_months / max(age_months)) %>%
    ungroup()

  bind_rows(lapply(unique(cut$Species), function(sp) {
    r <- cut$relative_age[cut$Species == sp]
    alive <- sapply(time_steps, function(t) sum(r > t))
    data.frame(relative_age = time_steps, proportion_alive = alive / max(alive),
               Species = sp, Class = class_name)
  })) %>%
    filter(proportion_alive > 0) %>%
    left_join(rage, by = "Species")
}

curve_plot <- function(df, title, show_legend = FALSE) {
  df$survivorship_type <- droplevels(factor(df$survivorship_type, levels = type_levels))
  p <- ggplot(df, aes(relative_age, proportion_alive, group = Species, color = survivorship_type)) +
    geom_step(linewidth = 0.6, alpha = 0.7, direction = "hv") +
    scale_color_manual(values = type_colors, name = "Survivorship Type", na.value = "grey60") +
    scale_y_log10() +
    labs(title = title, x = "Proportion of Maximum Observed Lifespan",
         y = "Proportion Alive (log scale)") +
    theme_cowplot(12) +
    theme(plot.margin = margin(t = 18, r = 8, b = 6, l = 6))
  if (!show_legend) p <- p + theme(legend.position = "none")
  p
}

mammals  <- build_curves("Mammalia")
birds    <- build_curves("Aves")
reptiles <- build_curves("Reptilia")
amphibia <- build_curves("Amphibia")
all_df   <- bind_rows(mammals, birds, reptiles, amphibia)

grid <- plot_grid(
  curve_plot(all_df, "All Species"), curve_plot(mammals, "Mammals"),
  curve_plot(reptiles, "Reptiles"), curve_plot(birds, "Aves"),
  labels = c("A", "B", "C", "D"), label_size = 14,
  label_x = 0.02, label_y = 0.98, hjust = 0, vjust = 1,
  ncol = 2, align = "hv"
)
legend <- get_legend(curve_plot(all_df, "", show_legend = TRUE) +
                       theme(legend.box.margin = margin(0, 0, 0, 12)))

ggsave("results/figures/survivorship_curves_4panel.png",
       plot_grid(grid, legend, ncol = 2, rel_widths = c(1, 0.22)),
       width = 16, height = 12, bg = "white")
