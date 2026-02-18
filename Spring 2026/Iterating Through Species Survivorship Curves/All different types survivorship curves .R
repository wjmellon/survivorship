library(ggplot2)
library(dplyr)
library(cowplot)

data <- read.csv("records.csv")

# --- Helper function ---
build_species_df <- function(class_name) {
  cutdata <- data %>%
    filter(Necropsy == 1) %>%
    filter(Class == class_name) %>%
    filter(Infant == 0)
  
  cutdata <- cutdata[, c(3, 28, 48, 18)]
  colnames(cutdata) <- c("age_months", "Species", "max_longevity", "Malignant")
  
  cutdata$age_months[cutdata$age_months <= 0] <- NA
  cutdata$max_longevity[cutdata$max_longevity <= 0] <- NA
  cutdata <- na.omit(cutdata)
  
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
  }))
  
  df %>% filter(proportion_alive > 0)
}

# --- Build data for each class ---
df_mammalia  <- build_species_df("Mammalia")
df_aves      <- build_species_df("Aves")
df_reptilia  <- build_species_df("Reptilia")
df_amphibia  <- build_species_df("Amphibia")

# --- Plot function ---
make_plot <- function(df, title, line_color = NULL) {
  p <- ggplot(df, aes(x = relative_age, y = proportion_alive))
  if (is.null(line_color)) {
    p <- p + geom_smooth(aes(color = Species), method = "gam",
                         formula = y ~ s(x, bs = "cs", k = 5),
                         se = FALSE, size = 0.7) +
      guides(color = "none")
  } else {
    p <- p + geom_smooth(aes(group = Species), color = line_color,
                         method = "gam", formula = y ~ s(x, bs = "cs", k = 5),
                         se = FALSE, size = 0.7, alpha = 0.6)
  }
  p +
    scale_y_log10() +
    ggtitle(title) +
    xlab("Proportion of Maximum Lifespan") +
    ylab("Proportion Alive (log scale)") +
    theme_cowplot(12)
}

# --- Individual plots ---
p_mammalia <- make_plot(df_mammalia, "Survivorship: Mammalia")
p_aves     <- make_plot(df_aves,     "Survivorship: Aves")
p_reptilia <- make_plot(df_reptilia, "Survivorship: Reptilia")
p_amphibia <- make_plot(df_amphibia, "Survivorship: Amphibia")

print(p_mammalia)
print(p_aves)
print(p_reptilia)
print(p_amphibia)

# --- Combined plot ---
class_colors <- c(
  "Mammalia" = "red",
  "Aves"     = "blue",
  "Reptilia" = "darkgreen",
  "Amphibia" = "purple"
)

df_all <- bind_rows(df_mammalia, df_aves, df_reptilia, df_amphibia)

p_combined <- ggplot(df_all, aes(x = relative_age, y = proportion_alive,
                                 group = interaction(Species, Class),
                                 color = Class)) +
  geom_line(size = 0.7, alpha = 0.6) +
  scale_color_manual(values = class_colors) +
  scale_y_log10() +
  ggtitle("Survivorship Curves: All Classes") +
  xlab("Proportion of Maximum Lifespan") +
  ylab("Proportion Alive (log scale)") +
  theme_cowplot(12) +
  theme(legend.title = element_text(size = 10),
        legend.text  = element_text(size = 9))

print(p_combined)