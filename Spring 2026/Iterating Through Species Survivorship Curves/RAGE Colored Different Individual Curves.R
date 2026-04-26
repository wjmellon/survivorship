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
  
  cutdata <- cutdata[, c(4, 29, 19)]           # age_months, Species, Malignant
  colnames(cutdata) <- c("age_months", "Species", "Malignant")
  
  cutdata$age_months[cutdata$age_months <= 0] <- NA
  cutdata <- na.omit(cutdata)
  
  cutdata <- cutdata %>%
    group_by(Species) %>%
    filter(n() >= 20) %>%
    ungroup()
  
  # Use the longest-lived observed individual per species as the reference maximum
  cutdata <- cutdata %>%
    group_by(Species) %>%
    mutate(max_age = max(age_months, na.rm = TRUE)) %>%
    ungroup() %>%
    mutate(relative_age = age_months / max_age)
  
  time_steps   <- seq(0, 1, by = 0.01)
  species_list <- unique(cutdata$Species)
  
  # Terminal report
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

# --- Combine all ---
all_df <- bind_rows(df_mammalia, df_aves, df_reptilia, df_amphibia)

# --- Plot function ---
make_plot <- function(df, title) {
  legend_order <- c("Type I", "Trending toward Type I", "Type II", 
                    "Trending toward Type III", "Type III")
  df$survivorship_type <- factor(df$survivorship_type, levels = legend_order)
    
  ggplot(df, aes(x = relative_age, y = proportion_alive,
                 group = Species, color = survivorship_type)) +
    geom_step(linewidth = 0.6, alpha = 0.7, direction = "hv")+
    scale_y_log10() +
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
    ylab("Proportion Alive (log scale)") +
    theme_cowplot(12)
}

# --- Individual plots ---
print(make_plot(df_mammalia, "Survivorship Curves of Mammals"))
print(make_plot(df_aves,     "Survivorship Curves of Aves"))
print(make_plot(df_reptilia, "Survivorship Curves of Reptiles"))
print(make_plot(df_amphibia, "Survivorship Curves of Amphibians"))

# --- Combined plot ---
print(make_plot(all_df, "Survivorship Curves of All Species"))

ggsave(filename='survivorship_curve_amphibians.png', width=10, height=8, limitsize=FALSE,bg="white")

