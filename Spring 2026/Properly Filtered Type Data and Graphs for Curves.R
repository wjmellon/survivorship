# ============================================================
# UNIFIED SCRIPT: Survivorship Type Calculation + Plotting
# Filters and "alive" definition are IDENTICAL in both steps
# so labels match the curves shown.
# ============================================================

library(dplyr)
library(Rage)
library(tibble)
library(ggplot2)
library(cowplot)

# ------------------------------------------------------------
# Step 0: Load raw data
# ------------------------------------------------------------
data           <- read.csv("Fall 2025/Filtering Data/cleanPath.min20.062822.csv")
condensed_data <- read.csv("Fall 2025/Fall 2025 Clean Data/condensed_data_with_gt_and_bs.csv")

# ------------------------------------------------------------
# Step 1: ONE consistent filter used for EVERYTHING downstream
#   - Necropsy == 1
#   - Infant == 0
#   - age_months > 0
#   - species must have >= 20 individuals AFTER these filters
# ------------------------------------------------------------
filtered_clean_data <- data %>%
  filter(Class %in% c("Mammalia", "Reptilia", "Amphibia", "Aves")) %>%
  filter(Necropsy == 1) %>%
  filter(Infant == 0) %>%
  filter(age_months > 0) %>%
  group_by(Species) %>%
  filter(n() >= 20) %>%
  ungroup()

# ------------------------------------------------------------
# Step 2: Prevalence metrics per species (same filtered pool)
# ------------------------------------------------------------
prevalence_clean_data <- filtered_clean_data %>%
  group_by(Species, Class) %>%
  summarise(
    n = n(),
    gestation = mean(Gestation[Gestation != -1], na.rm = TRUE),
    body_size = mean(adult_weight[adult_weight != -1], na.rm = TRUE),
    neoplasia_cases = sum(Malignant >= 0, na.rm = TRUE),
    neoplasia_prevalence = neoplasia_cases / n,
    cancer_cases = sum(Malignant == 1, na.rm = TRUE),
    cancer_prevalence = cancer_cases / n,
    max_longevity = max(age_months, na.rm = TRUE),
    .groups = "drop"
  )

# ------------------------------------------------------------
# Step 3: Calculate survivorship type per species
#   "alive at x" uses relative_age > x (strict), matching
#   the plotting curve's definition exactly.
#   Zero values are dropped from lx before shape_surv(),
#   matching the plotting script's filter(proportion_alive > 0).
# ------------------------------------------------------------
survival_clean_metrics <- filtered_clean_data %>%
  group_by(Species) %>%
  group_map(~{
    df <- .x
    species_name <- .y$Species
    
    df <- df %>%
      mutate(relative_age = age_months / max(age_months, na.rm = TRUE))
    
    time_steps <- seq(0, 1, by = 0.01)
    
    alive_counts <- sapply(time_steps, function(x) sum(df$relative_age > x))
    
    if (max(alive_counts) == 0) {
      return(tibble(Species = species_name, survivorship_type = NA, shape_value = NA))
    }
    
    lx <- alive_counts / max(alive_counts)
    lx <- lx[lx > 0]   # drop zero(s) before log-based shape_surv calc
    
    if (length(lx) < 2) {
      return(tibble(Species = species_name, survivorship_type = NA, shape_value = NA))
    }
    
    shape_type <- shape_surv(lx)
    
    surv_type <- case_when(
      shape_type >= 0.3                        ~ "Type I",
      shape_type >= 0.1 & shape_type < 0.3      ~ "Trending toward Type I",
      shape_type > -0.1 & shape_type < 0.1      ~ "Type II",
      shape_type > -0.3 & shape_type <= -0.1    ~ "Trending toward Type III",
      shape_type <= -0.3                        ~ "Type III"
    )
    
    tibble(
      Species = species_name,
      survivorship_type = surv_type,
      shape_value = shape_type
    )
  }) %>%
  bind_rows()

# ------------------------------------------------------------
# Step 4: Merge prevalence + survival metrics
# ------------------------------------------------------------
condensed_clean_data <- prevalence_clean_data %>%
  left_join(survival_clean_metrics, by = "Species") %>%
  select(
    Species, Class, n, max_longevity, gestation, body_size,
    neoplasia_prevalence, cancer_prevalence,
    survivorship_type, shape_value
  )

# ------------------------------------------------------------
# Step 5: Merge with condensed_data (only species present in both)
# ------------------------------------------------------------
final_clean_data <- condensed_data %>%
  semi_join(condensed_clean_data, by = "Species") %>%
  left_join(
    dplyr::select(condensed_clean_data, Species, survivorship_type, shape_value),
    by = "Species"
  )

write.csv(final_clean_data,
          "Fall 2025/Fall 2025 Clean Data/final_clean_data_w_multivariate_MATCHED.csv",
          row.names = FALSE)

# ============================================================
# PART 2: BUILD CURVES FOR PLOTTING (same filtered pool as above)
# ============================================================

build_species_df <- function(class_name) {
  cutdata <- filtered_clean_data %>%
    filter(Class == class_name)
  
  cutdata <- cutdata[, c("age_months", "Species", "Malignant")]
  cutdata <- na.omit(cutdata)
  
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
    left_join(dplyr::select(final_clean_data, Species, shape_value, survivorship_type),
              by = "Species")
  
  df
}

df_mammalia <- build_species_df("Mammalia")
df_aves     <- build_species_df("Aves")
df_reptilia <- build_species_df("Reptilia")
df_amphibia <- build_species_df("Amphibia")

all_df <- bind_rows(df_mammalia, df_aves, df_reptilia, df_amphibia)

# ------------------------------------------------------------
# Plot function
# ------------------------------------------------------------
make_plot <- function(df, title, log_y = TRUE) {
  legend_order <- c("Type I", "Trending toward Type I", "Type II",
                    "Trending toward Type III", "Type III")
  df$survivorship_type <- factor(df$survivorship_type, levels = legend_order)
  
  p <- ggplot(df, aes(x = relative_age, y = proportion_alive,
                      group = Species, color = survivorship_type)) +
    geom_step(linewidth = 0.6, alpha = 0.7, direction = "hv") +
    scale_color_manual(
      values = c(
        "Type I"                   = "red",
        "Trending toward Type I"   = "purple",
        "Type II"                  = "blue",
        "Trending toward Type III" = "green",
        "Type III"                 = "yellow"
      ),
      name     = "Survivorship Type",
      na.value = "grey60"
    ) +
    ggtitle(title) +
    xlab("Proportion of Maximum Observed Lifespan") +
    ylab(if (log_y) "Proportion Alive (log scale)" else "Proportion Alive") +
    theme_cowplot(12)
  
  if (log_y) p <- p + scale_y_log10()
  
  p
}

# ------------------------------------------------------------
# Individual plots
# ------------------------------------------------------------
print(make_plot(df_mammalia, "Survivorship Curves of Mammals", log_y = FALSE))
print(make_plot(df_aves,     "Survivorship Curves of Aves"))
print(make_plot(df_reptilia, "Survivorship Curves of Reptiles"))
print(make_plot(df_amphibia, "Survivorship Curves of Amphibians"))

# ------------------------------------------------------------
# Combined plot
# ------------------------------------------------------------
print(make_plot(all_df, "Survivorship Curves of All Species"))

ggsave(filename = "survivorship_curve_amphibians_MATCHED.png",
       width = 10, height = 8, limitsize = FALSE, bg = "white")

# ------------------------------------------------------------
# Diagnostic: one panel per species to visually verify type labels
# ------------------------------------------------------------
verify_df <- all_df %>%
  mutate(facet_label = sprintf("%s\n%s (shape=%.2f)",
                               Species, survivorship_type, shape_value))

ggplot(verify_df, aes(x = relative_age, y = proportion_alive)) +
  geom_step(linewidth = 0.6, direction = "hv", color = "steelblue") +
  facet_wrap(~ facet_label, scales = "free_y") +
  scale_y_log10() +
  xlab("Proportion of Maximum Observed Lifespan") +
  ylab("Proportion Alive (log scale)") +
  theme_cowplot(10) +
  theme(strip.text = element_text(size = 7))

ggsave("survivorship_by_species_verification_MATCHED.png",
       width = 20, height = 20, limitsize = FALSE, bg = "white")