library(dplyr)
library(fmsb)

dir.create("results", showWarnings = FALSE)

individual_data <- read.csv("data/cleanPath.min20.062822.csv")
species_list <- read.csv("data/siler_species_list.csv")$Species

make_life_table <- function(sp) {
  ages <- individual_data$age_months[individual_data$Species == sp]
  breaks <- seq(0, max(ages, na.rm = TRUE), length.out = 11)
  d_x <- hist(ages, breaks = breaks, plot = FALSE)$counts
  l_x <- numeric(10)
  l_x[1] <- length(ages)
  for (i in 2:10) l_x[i] <- l_x[i - 1] - d_x[i - 1]
  data.frame(Species = sp,
             Interval_Start = breaks[-11],
             Interval_End = breaks[-1],
             l_x = l_x,
             d_x = d_x,
             q_x = ifelse(l_x > 0, d_x / l_x, NA))
}

life_tables <- bind_rows(lapply(species_list, make_life_table))
write.csv(life_tables, "results/life_tables.csv", row.names = FALSE)

fit_species <- function(sp) {
  q_x <- life_tables %>%
    filter(Species == sp, !is.na(q_x), q_x >= 0, q_x <= 1) %>%
    pull(q_x)
  empty <- data.frame(Species = sp, a1 = NA, b1 = NA, a2 = NA, a3 = NA, b3 = NA, FLAG = NA)
  if (length(q_x) < 5) return(empty)
  res <- tryCatch(fitSiler(c(0.1, 0.1, 0.01, 0.1, 0.01), q_x), error = function(e) NULL)
  if (is.null(res)) return(empty)
  attempt <- 0
  while (res[7] > 0 && attempt < 20) {
    res <- fitSiler(res[1:5], q_x)
    attempt <- attempt + 1
  }
  data.frame(Species = sp, a1 = res[1], b1 = res[2], a2 = res[3],
             a3 = res[4], b3 = res[5], FLAG = res[7])
}

siler <- bind_rows(lapply(species_list, fit_species))
write.csv(siler, "results/siler_parameters.csv", row.names = FALSE)
