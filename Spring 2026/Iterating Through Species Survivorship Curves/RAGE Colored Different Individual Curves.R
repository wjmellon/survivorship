library(ggplot2)
library(dplyr)
library(cowplot)

data <- read.csv("Fall 2025/Filtering Data/cleanPath.min20.062822.csv")
condensed <- read.csv("Spring 2026/final_clean_data_w_multivariate.csv")

# --- Helper function ---
build_species_df <- function(class_name) {
  cutdata <- data %>%
    filter(Necropsy == 1) %>%
    filter(Class == class_name) %>%
    filter(Infant == 0)
  
  cutdata <- cutdata[, c(4, 29, 49, 19)]
  colnames(cutdata) <- c("age_months", "Species", "max_longevity", "Malignant")
  
  cutdata$age_months[cutdata$age_months <= 0] <- NA
  cutdata$max_longevity[cutdata$max_longevity <= 0] <- NA
  cutdata <- na.omit(cutdata)
  
  cutdata <- cutdata %>%
    group_by(Species) %>%
    filter(n() >= 20) %>%
    ungroup()
  
  cutdata <- cutdata %>% mutate(relative_age = age_months / max_longevity)
  
  time_steps <- seq(0, 1, by = 0.01)
  species_list <- unique(cutdata$Species)
  
  # Terminal report
  cat("======================\n")
  cat("Class:", class_name, "\n")
  cat("Number of species:", length(species_list), "\n")
  cat("Species:\n")
  for (sp in sort(species_list)) cat(" -", sp, "\n")
  cat("======================\n\n")
  
  df <- bind_rows(lapply(species_list, function(sp) {
    sp_data <- cutdata %>% filter(Species == sp)
    alive <- sapply(time_steps, function(x) sum(sp_data$relative_age > x))
    if (max(alive) == 0) return(NULL)
    data.frame(
      relative_age = time_steps,
      proportion_alive = alive / max(alive),
      Species = sp,
      Class = class_name
    )
  }))0
  
  df <- df %>% filter(proportion_alive > 0)
  
  df <- df %>%
    left_join(dplyr::select(condensed, Species, shape_value, survivorship_type), by = "Species")
  
  df
}

# --- Build data ---
df_mammalia <- build_species_df("Mammalia")
df_aves     <- build_species_df("Aves")
df_reptilia <- build_species_df("Reptilia")
df_amphibia <- build_species_df("Amphibia")

### Issue here ^ with an increase in number of observations?? 

# --- Combine all ---
all_df <- bind_rows(df_mammalia, df_aves, df_reptilia, df_amphibia)

# --- Plot function ---
make_plot <- function(df, title) {
  ggplot(df, aes(x = relative_age, y = proportion_alive,
                 group = Species, color = survivorship_type)) +
    geom_smooth(method = "gam", formula = y ~ s(x, bs = "cs", k = 5),
                se = FALSE, size = 0.7) +
    scale_y_log10() +
    scale_color_manual(
      values = c(
        "Type I"                  = "red",
        "Trending toward Type I"  = "orange",
        "Type II"                 = "green",
        "Trending toward Type III"= "steelblue",
        "Type III"                = "blue"
      ),
      name = "Survivorship Type",
      na.value = "grey60"
    ) +
    ggtitle(title) +
    xlab("Proportion of Maximum Lifespan") +
    ylab("Proportion Alive (log scale)") +
    theme_cowplot(12)
}

# --- Individual plots ---
print(make_plot(df_mammalia, "Survivorship: Mammalia"))
print(make_plot(df_aves,     "Survivorship: Aves"))
print(make_plot(df_reptilia, "Survivorship: Reptilia"))
print(make_plot(df_amphibia, "Survivorship: Amphibia"))

# --- Combined plot ---
print(make_plot(all_df, "Survivorship: All Classes"))