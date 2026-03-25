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
  
  # Compute max observed age per species
  cutdata <- cutdata %>%
    group_by(Species) %>%
    mutate(
      max_age    = max(age_months, na.rm = TRUE),
      n_species  = n()
    ) %>%
    ungroup() %>%
    mutate(relative_age = age_months / max_age)
  
  # Only keep species with >= 20 individuals
  species_list <- cutdata %>%
    count(Species) %>%
    filter(n >= 20) %>%
    pull(Species)
  
  # Terminal report
  cat("======================\n")
  cat("Class:", class_name, "\n")
  cat("Number of species:", length(species_list), "\n")
  cat("Species:\n")
  for (sp in sort(species_list)) {
    sp_data <- cutdata %>% filter(Species == sp)
    sp_max  <- unique(sp_data$max_age)
    sp_n    <- nrow(sp_data)
    cat(sprintf("  - %-40s (max observed age: %.1f months, n = %d)\n", sp, sp_max, sp_n))
  }
  cat("======================\n\n")
  
  df <- bind_rows(lapply(species_list, function(sp) {
    sp_data   <- cutdata %>% filter(Species == sp)
    sp_max    <- unique(sp_data$max_age)
    sp_n      <- unique(sp_data$n_species)
    
    # Extend x-axis by 1 month beyond max lifespan (converted to relative scale)
    extra_rel  <- 1 / sp_max          # 1 month in relative units
    max_rel    <- 1 + extra_rel        # slightly past 1.0
    
    # Build a fine grid of time steps up to the extended max
    time_steps <- seq(0, max_rel, by = 0.005)
    
    # Compute raw step-wise survival (strictly monotone decreasing)
    n_total <- nrow(sp_data)
    alive   <- sapply(time_steps, function(x) sum(sp_data$relative_age > x))
    
    if (max(alive) == 0) return(NULL)
    
    data.frame(
      relative_age     = time_steps,
      proportion_alive = alive / n_total,   # proportion of all individuals
      n_alive          = alive,
      Species          = sp,
      Class            = class_name,
      n_species        = sp_n,
      max_age_months   = sp_max
    )
  }))
  
  df %>% filter(proportion_alive > 0)
}

# --- Build data for each class ---
df_mammalia <- build_species_df("Mammalia")
df_aves     <- build_species_df("Aves")
df_reptilia <- build_species_df("Reptilia")
df_amphibia <- build_species_df("Amphibia")

# --- Combine all ---
df_all <- bind_rows(df_mammalia, df_aves, df_reptilia, df_amphibia)

# --- Bin by n ---
df_all <- df_all %>%
  mutate(n_bin = case_when(
    n_species >= 20 & n_species <= 50 ~ "20–50 individuals",
    n_species > 50                    ~ "50+ individuals",
    TRUE                              ~ NA_character_
  ))

# ============================================================
# --- Plot function using geom_step (monotone, no smoothing) ---
# ============================================================
make_plot_step <- function(df, title, line_color = NULL) {
  p <- ggplot(df, aes(x = relative_age, y = proportion_alive))
  
  if (is.null(line_color)) {
    # Individual class plots: color by species
    p <- p +
      geom_step(aes(color = Species, group = Species),
                linewidth = 0.6, alpha = 0.4, direction = "hv") +
      guides(color = "none")
  } else {
    # Combined class plot: single color
    p <- p +
      geom_step(aes(group = Species), color = line_color,
                linewidth = 0.6, alpha = 0.4, direction = "hv")
  }
  
  p +
    scale_y_log10(
      breaks = c(0.001, 0.01, 0.1, 1),
      labels = c("0.001", "0.01", "0.1", "1")
    ) +
    scale_x_continuous(limits = c(0, NA), expand = expansion(mult = c(0, 0.02))) +
    ggtitle(title) +
    xlab("Proportion of Maximum Observed Lifespan") +
    ylab("Proportion Alive (log scale)") +
    theme_cowplot(12)
}

# --- Individual class plots ---
print(make_plot_step(df_mammalia, "Survivorship: Mammalia"))
print(make_plot_step(df_aves,     "Survivorship: Aves"))
print(make_plot_step(df_reptilia, "Survivorship: Reptilia"))
print(make_plot_step(df_amphibia, "Survivorship: Amphibia"))

# --- Combined plot: all classes, colored by class ---
class_colors <- c(
  "Mammalia" = "red",
  "Aves"     = "blue",
  "Reptilia" = "darkgreen",
  "Amphibia" = "goldenrod"
)

p_combined <- ggplot(df_all,
                     aes(x = relative_age, y = proportion_alive,
                         group = interaction(Species, Class),
                         color = Class)) +
  geom_step(linewidth = 0.6, alpha = 0.4, direction = "hv") +
  scale_color_manual(values = class_colors) +
  scale_y_log10(
    breaks = c(0.001, 0.01, 0.1, 1),
    labels = c("0.001", "0.01", "0.1", "1")
  ) +
  scale_x_continuous(limits = c(0, NA), expand = expansion(mult = c(0, 0.02))) +
  ggtitle("Survivorship Curves: All Classes") +
  xlab("Proportion of Maximum Observed Lifespan") +
  ylab("Proportion Alive (log scale)") +
  theme_cowplot(12) +
  theme(legend.title = element_text(size = 10),
        legend.text  = element_text(size = 9))

print(p_combined)

# ============================================================
# --- Binned plots: 20–50 vs 50+ individuals, colored by class ---
# ============================================================

make_binned_plot <- function(df_bin, bin_label) {
  ggplot(df_bin,
         aes(x = relative_age, y = proportion_alive,
             group = interaction(Species, Class),
             color = Class)) +
    geom_step(linewidth = 0.6, alpha = 0.25, direction = "hv") +
    scale_color_manual(values = class_colors) +
    scale_y_log10(
      breaks = c(0.001, 0.01, 0.1, 1),
      labels = c("0.001", "0.01", "0.1", "1")
    ) +
    scale_x_continuous(limits = c(0, NA), expand = expansion(mult = c(0, 0.02))) +
    ggtitle(paste0("Survivorship Curves: ", bin_label)) +
    xlab("Proportion of Maximum Observed Lifespan") +
    ylab("Proportion Alive (log scale)") +
    theme_cowplot(12) +
    theme(legend.title = element_text(size = 10),
          legend.text  = element_text(size = 9))
}

# Plot bin 1: 20–50 individuals
df_bin1 <- df_all %>% filter(n_bin == "20–50 individuals")
if (nrow(df_bin1) > 0) {
  cat(sprintf("Bin 20–50: %d species\n",
              length(unique(paste(df_bin1$Species, df_bin1$Class)))))
  print(make_binned_plot(df_bin1, "Species with 20–50 Individuals"))
}

# Plot bin 2: 50+ individuals
df_bin2 <- df_all %>% filter(n_bin == "50+ individuals")
if (nrow(df_bin2) > 0) {
  cat(sprintf("Bin 50+: %d species\n",
              length(unique(paste(df_bin2$Species, df_bin2$Class)))))
  print(make_binned_plot(df_bin2, "Species with 50+ Individuals"))
}

# --- Optional: side-by-side panel of both bins ---
p_panel <- plot_grid(
  make_binned_plot(df_bin1, "n = 20–50"),
  make_binned_plot(df_bin2, "n = 50+"),
  nrow = 1,
  labels = "AUTO"
)
print(p_panel)