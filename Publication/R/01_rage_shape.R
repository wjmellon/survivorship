library(dplyr)
library(Rage)

dir.create("results", showWarnings = FALSE)

data <- read.csv("data/cleanPath.min20.062822.csv")

cutdata <- data %>%
  filter(Necropsy == 1, Infant == 0) %>%
  transmute(Species,
            age_months = as.numeric(age_months),
            max_longevity = as.numeric(max_longevity)) %>%
  filter(!is.na(age_months), age_months > 0,
         !is.na(max_longevity), max_longevity > 0) %>%
  mutate(relative_age = age_months / max_longevity) %>%
  group_by(Species) %>%
  filter(n() >= 20) %>%
  ungroup()

time_steps <- seq(0, 1, by = 0.01)

classify <- function(x) {
  case_when(
    x >= 0.3 ~ "Type I",
    x >= 0.1 ~ "Trending toward Type I",
    x > -0.1 ~ "Type II",
    x > -0.3 ~ "Trending toward Type III",
    TRUE ~ "Type III"
  )
}

rage <- cutdata %>%
  group_by(Species) %>%
  summarise(
    n = n(),
    shape_value = {
      alive <- sapply(time_steps, function(t) sum(relative_age >= t))
      shape_surv(alive / max(alive), trunc = TRUE)
    },
    .groups = "drop"
  ) %>%
  mutate(survivorship_type = classify(shape_value),
         shape_value = round(shape_value, 3),
         abs_shape = abs(shape_value))

write.csv(rage, "results/rage_shape_values.csv", row.names = FALSE)
