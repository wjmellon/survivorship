library(ggplot2)
library(dplyr)
library(cowplot)

data      <- read.csv("Fall 2025/Filtering Data/cleanPath.min20.062822.csv")
condensed <- read.csv("Spring 2026/final_clean_data_w_multivariate.csv")



# --- Helper function ---
build_species_df <- function(class_name) {
  cutdata <- data %>%
    filter(Necropsy == 1) %>%
    filter(Class == class_name) %>%
    filter(Infant == 0)
  
  cutdata <- cutdata[, c(4, 29, 19)]
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
              by = "Species") %>%
    mutate(shape_value = as.numeric(shape_value))
  
  df
}

# --- Build data ---
df_mammalia <- build_species_df("Mammalia")
df_aves     <- build_species_df("Aves")
df_reptilia <- build_species_df("Reptilia")
df_amphibia <- build_species_df("Amphibia")

all_df <- bind_rows(df_mammalia, df_aves, df_reptilia, df_amphibia)

# --- Line plot function (individual classes) ---
make_plot <- function(df, title) {
  ggplot(df, aes(x = relative_age, y = proportion_alive,
                 group = Species, color = shape_value)) +
    geom_smooth(method = "gam", formula = y ~ s(x, bs = "cs", k = 5),
                se = FALSE, linewidth = 0.7) +
    scale_y_log10() +
    scale_color_gradient2(
      low      = "#313695",
      mid      = "#1A9641",
      high     = "#D7191C",
      midpoint = 0,
      name     = "Shape Value\n(← Type III | Type I →)",
      na.value = "grey60"
    ) +
    ggtitle(title) +
    xlab("Proportion of Maximum Observed Lifespan") +
    ylab("Proportion Alive (log scale)") +
    theme_cowplot(12) +
    guides(color = guide_colorbar(barwidth = 10, barheight = 0.8,
                                  title.position = "top", title.hjust = 0.5))
}

# --- Heatmap function ---
make_heatmap <- function(df, title) {
  # One proportion_alive value per species per time step (already structured this way)
  # Order species by shape_value: Type I (positive) at top, Type III (negative) at bottom
  species_order <- df %>%
    distinct(Species, shape_value) %>%
    arrange(desc(shape_value)) %>%
    pull(Species)
  
  df <- df %>%
    mutate(Species = factor(Species, levels = species_order))
  
  ggplot(df, aes(x = relative_age, y = Species, fill = proportion_alive)) +
    geom_tile() +
    scale_fill_viridis_c(
      option   = "magma",
      name     = "Proportion\nAlive",
      limits   = c(0, 1),
      na.value = "grey50"
    ) +
    scale_x_continuous(expand = c(0, 0)) +
    ggtitle(title) +
    xlab("Proportion of Maximum Observed Lifespan") +
    ylab("Species (ordered by survivorship type)") +
    theme_cowplot(12) +
    theme(
      axis.text.y  = element_text(size = 7),
      axis.ticks.y = element_blank(),
      legend.position = "right"
    )
}

# --- Individual line plots ---
print(make_plot(df_mammalia, "Survivorship: Mammalia"))
print(make_plot(df_aves,     "Survivorship: Aves"))
print(make_plot(df_reptilia, "Survivorship: Reptilia"))
print(make_plot(df_amphibia, "Survivorship: Amphibia"))

# --- Combined heatmap ---
print(make_heatmap(all_df, "Survivorship Heatmap: All Classes"))