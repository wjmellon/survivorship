library(ggplot2)
library(dplyr)
library(cowplot)
library(ggrepel)

data      <- read.csv("Fall 2025/Filtering Data/cleanPath.min20.062822.csv")
condensed <- read.csv("Spring 2026/final_clean_data_w_multivariate.csv")

# --- Helper function ---
build_species_df <- function(class_name) {
  cutdata <- data %>%
    filter(Necropsy == 1) %>%
    filter(Class == class_name) %>%
    filter(Infant == 0)
  
  cutdata <- cutdata[, c(4, 29, 19)]           # age_months, Species, Malignant
  colnames(cutdata) <- c("age_months", "Species", "Malignant")
  
  cutdata$age_months[cutdata$age_months <= 0] <- NA
  cutdata <- na.omit(cutdata)
  
  cutdata <- cutdata %>%
    group_by(Species) %>%
    filter(n() >= 20) %>%
    ungroup()
  
  cutdata <- cutdata %>%
    group_by(Species) %>%
    mutate(max_age = max(age_months, na.rm = TRUE)) %>%
    ungroup() %>%
    mutate(relative_age = age_months / max_age)
  
  time_steps   <- seq(0, 1, by = 0.01)
  species_list <- unique(cutdata$Species)
  
  cat("======================\n")
  cat("Class:", class_name, "\n")
  cat("Number of species:", length(species_list), "\n")
  cat("Species:\n")
  for (sp in sort(species_list)) {
    sp_max <- cutdata %>% filter(Species == sp) %>% pull(max_age) %>% unique()
    cat(sprintf("  - %-40s (max observed age: %.1f months)\n", sp, sp_max))
  }
  cat("======================\n\n")
  
  df <- bind_rows(lapply(species_list, function(sp) {
    sp_data <- cutdata %>% filter(Species == sp)
    alive   <- sapply(time_steps, function(x) sum(sp_data$relative_age > x))
    if (max(alive) == 0) return(NULL)
    data.frame(
      relative_age     = time_steps,
      proportion_alive = alive / max(alive),
      Species          = sp,
      Class            = class_name
    )
  }))
  
  df <- df %>% filter(proportion_alive > 0)
  
  df <- df %>%
    left_join(dplyr::select(condensed, Species, shape_value, survivorship_type),
              by = "Species")
  df
}

# --- Build data ---
df_mammalia <- build_species_df("Mammalia")
df_aves     <- build_species_df("Aves")
df_reptilia <- build_species_df("Reptilia")
df_amphibia <- build_species_df("Amphibia")

all_df <- bind_rows(df_mammalia, df_aves, df_reptilia, df_amphibia)

# --- Plot builder ---
# show_legend/show_labels let us build clean, label-free, legend-free panels for the grid
make_plot <- function(df, title, log_y = TRUE, label_types = NULL,
                      show_legend = TRUE, show_labels = TRUE) {
  legend_order <- c("Type I", "Trending toward Type I", "Type II",
                    "Trending toward Type III", "Type III")
  df$survivorship_type <- factor(df$survivorship_type, levels = legend_order)
  
  label_df <- df %>%
    group_by(Species) %>%
    filter(relative_age == max(relative_age)) %>%
    slice(1) %>%
    ungroup()
  
  if (!is.null(label_types)) {
    label_df <- label_df %>% filter(survivorship_type %in% label_types)
  }
  
  p <- ggplot(df, aes(x = relative_age, y = proportion_alive,
                      group = Species, color = survivorship_type)) +
    geom_step(linewidth = 0.6, alpha = 0.7, direction = "hv") +
    scale_color_manual(
      values = c(
        "Type I"                   = "#8c564b",
        "Trending toward Type I"   = "#9467bd",
        "Type II"                  = "#ff7f0e",
        "Trending toward Type III" = "#2ca02c",
        "Type III"                 = "#1f77b4"
      ),
      name     = "Survivorship Type",
      na.value = "grey60"
    ) +
    ggtitle(title) +
    xlab("Proportion of Maximum Observed Lifespan") +
    ylab(if (log_y) "Proportion Alive (log scale)" else "Proportion Alive") +
    theme_cowplot(12)
  
  if (show_labels && nrow(label_df) > 0) {
    p <- p + geom_text_repel(
      data = label_df,
      aes(label = Species),
      size = 2.5,
      max.overlaps = Inf,
      segment.size = 0.2,
      segment.alpha = 0.5,
      min.segment.length = 0,
      box.padding = 0.3,
      show.legend = FALSE,
      seed = 42
    )
  }
  
  if (log_y) p <- p + scale_y_log10()
  
  if (!show_legend) p <- p + theme(legend.position = "none")
  
  p
}

# --- Standalone plots (unchanged behavior, own legends/labels) ---
p_mammalia <- make_plot(df_mammalia, "Survivorship Curves of Mammals", log_y = TRUE, label_types = "Type III")
p_aves     <- make_plot(df_aves,     "Survivorship Curves of Aves",     log_y = TRUE)
p_reptilia <- make_plot(df_reptilia, "Survivorship Curves of Reptiles", log_y = TRUE)
p_amphibia <- make_plot(df_amphibia, "Survivorship Curves of Amphibians", log_y = TRUE)
p_all      <- make_plot(all_df,      "Survivorship Curves of All Species", log_y = TRUE)

print(p_mammalia)
print(p_aves)
print(p_reptilia)
print(p_amphibia)
print(p_all)

ggsave(filename = "survivorship_curve_amphibians.png", plot = p_amphibia,
       width = 10, height = 8, limitsize = FALSE, bg = "white")

# --- 4-panel figure: All Species, Mammalia, Aves, Reptilia ---
# no species labels, no per-panel legends -- legend pulled out and shared
panel_all      <- make_plot(all_df,      "All Species", log_y = TRUE, show_legend = FALSE, show_labels = FALSE)
panel_mammalia <- make_plot(df_mammalia, "Mammals",      log_y = TRUE, show_legend = FALSE, show_labels = FALSE)
panel_aves     <- make_plot(df_aves,     "Aves",         log_y = TRUE, show_legend = FALSE, show_labels = FALSE)
panel_reptilia <- make_plot(df_reptilia, "Reptiles",     log_y = TRUE, show_legend = FALSE, show_labels = FALSE)

# extra top margin on each panel so the A/B/C/D tag has room and doesn't sit on the axis/title
pad_top <- theme(plot.margin = margin(t = 18, r = 8, b = 6, l = 6))
panel_all      <- panel_all      + pad_top
panel_mammalia <- panel_mammalia + pad_top
panel_aves     <- panel_aves     + pad_top
panel_reptilia <- panel_reptilia + pad_top

# pull one legend from a version of the plot that still has it
legend_source <- make_plot(all_df, "", log_y = TRUE, show_legend = TRUE, show_labels = FALSE)
shared_legend <- get_legend(legend_source + theme(legend.box.margin = margin(0, 0, 0, 12)))

grid_2x2 <- plot_grid(
  panel_all, panel_mammalia, panel_reptilia, panel_aves,
  labels = c("A", "B", "C", "D"),
  label_size = 14,
  label_x = 0.02, label_y = 0.98,   # nudge tags inward, away from axis
  hjust = 0, vjust = 1,
  ncol = 2, align = "hv"
)

p_combined <- plot_grid(
  grid_2x2, shared_legend,
  ncol = 2, rel_widths = c(1, 0.22)   # widen/narrow this to taste
)

print(p_combined)

ggsave(filename = "survivorship_curves_4panel.png", plot = p_combined,
       width = 16, height = 12, limitsize = FALSE, bg = "white")