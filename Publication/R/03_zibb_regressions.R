library(ape)
library(rstan)
library(tidyverse)
library(cowplot)
library(scales)

select <- dplyr::select

phylo_model <- "BM"
chains      <- 4L
warmup      <- 2000L
samples     <- 4000L
seed        <- 1234L
MIN_SPECIES <- 10
min_trials  <- 20

out_dir <- "results/zibb"
dir.create(file.path(out_dir, "plots"), recursive = TRUE, showWarnings = FALSE)

class_colors <- c(Amphibia = "#F8766D", Aves = "#7CAE00",
                  Mammalia = "#00BFC4", Reptilia = "#C77CFF")
subsets  <- c("All", names(class_colors))
outcomes <- c(NeoplasiaWithDenominators = "Neoplasia", Malignant = "Malignancy")

clean_species <- function(x) {
  x <- gsub("_+", "_", gsub("[[:space:]]+", "_", trimws(as.character(x))))
  vapply(strsplit(x, "_", fixed = TRUE), function(z) {
    if (length(z) < 2L) return(z[1])
    paste(c(paste0(toupper(substr(z[1], 1, 1)), tolower(substring(z[1], 2))),
            tolower(z[-1])), collapse = "_")
  }, character(1))
}

aliases   <- read.csv("data/species_aliases.csv", stringsAsFactors = FALSE)
alias_map <- setNames(clean_species(aliases$tree_species), clean_species(aliases$data_species))
fix_names <- function(x) {
  x <- clean_species(x)
  hit <- x %in% names(alias_map)
  x[hit] <- unname(alias_map[x[hit]])
  x
}

tree <- read.tree("data/min20Fixed516.nwk")
tree$tip.label <- clean_species(tree$tip.label)

prevalence_data <- read.csv("data/species-cancer-prevalence-data.csv", check.names = FALSE) %>%
  mutate(.species = fix_names(Species)) %>%
  distinct(.species, .keep_all = TRUE) %>%
  transmute(.species, Class,
            trials = as.integer(Necropsies),
            NeoplasiaWithDenominators = as.integer(NeoplasiaWithDenominators),
            Malignant = as.integer(Malignant)) %>%
  filter(!is.na(trials), trials >= min_trials)

shape_data <- read.csv("results/rage_shape_values.csv") %>%
  mutate(.species = fix_names(Species)) %>%
  distinct(.species, .keep_all = TRUE) %>%
  select(.species, abs_shape)

siler_data <- read.csv("results/siler_parameters.csv") %>%
  filter(as.character(FLAG) == "0") %>%
  mutate(.species = fix_names(Species)) %>%
  distinct(.species, .keep_all = TRUE) %>%
  select(.species, a1, b1, a2, a3, b3)

predictor_list <- list(
  abs_shape = list(data = shape_data, label = "Distance from Type II Survivorship", file = "abs_shape"),
  a1 = list(data = siler_data, label = "Initial Juvenile Mortality Risk (a1)", file = "siler_a1"),
  b1 = list(data = siler_data, label = "Rate of Decline in Juvenile Mortality Risk (b1)", file = "siler_b1"),
  a2 = list(data = siler_data, label = "Age-Independent Mortality Risk (a2)", file = "siler_a2"),
  a3 = list(data = siler_data, label = "Initial Senescent Mortality Risk (a3)", file = "siler_a3"),
  b3 = list(data = siler_data, label = "Change in Mortality Risk in the Senescent Stage (b3)", file = "siler_b3")
)

stan_file <- file.path("stan", c(BM = "ZiBBPGLMM_BM_main.stan",
                                 lambda = "ZiBBPGLMM_joint_lambda_main.stan",
                                 OU = "ZiBBPGLMM_joint_OU_alpha_main.stan")[[phylo_model]])
rstan_options(auto_write = TRUE)
cores <- min(chains, max(1L, parallel::detectCores(), na.rm = TRUE))
stan_mod <- stan_model(file = stan_file)

q3 <- function(v) unname(quantile(v, c(0.5, 0.025, 0.975), na.rm = TRUE))

zibb_fit <- function(pred_df, xvar, yvar, subset) {
  info <- predictor_list[[xvar]]
  base <- tibble(subset = subset, predictor = xvar, predictor_label = info$label,
                 response = outcomes[[yvar]], phylo_model = phylo_model)

  d <- prevalence_data %>%
    inner_join(pred_df %>% select(.species, x = all_of(xvar)), by = ".species") %>%
    mutate(y = .data[[yvar]]) %>%
    filter(is.finite(x), !is.na(y), y <= trials, .species %in% tree$tip.label)
  if (subset != "All") d <- d %>% filter(Class == subset)

  cat(outcomes[[yvar]], "~", xvar, "|", subset, "(", nrow(d), "species )\n")
  if (nrow(d) < MIN_SPECIES) return(base %>% mutate(n_species = nrow(d), status = "skipped"))

  ptree <- drop.tip(tree, setdiff(tree$tip.label, d$.species))
  sp <- ptree$tip.label
  d  <- d[match(sp, d$.species), ]
  x_center <- mean(d$x); x_sd <- sd(d$x)
  log_n <- log(d$trials)

  stan_data <- list(
    N = nrow(d), y = d$y, trials = d$trials,
    K_mu = 1L, X_mu = matrix((d$x - x_center) / x_sd, ncol = 1L),
    K_zi = 1L, X_zi = matrix((log_n - mean(log_n)) / sd(log_n), ncol = 1L),
    rho_prior_a = 1, rho_prior_b = 9,
    zi_intercept_prior_mean = -2, zi_intercept_prior_sd = 1.5)

  if (phylo_model %in% c("BM", "lambda")) {
    A <- cov2cor(vcv(ptree, corr = FALSE))[sp, sp, drop = FALSE]
    diag(A) <- 1
    if (phylo_model == "BM") stan_data$L_A <- t(chol(A)) else stan_data$A_BM <- A
  } else {
    h <- max(node.depth.edgelength(ptree)[seq_along(sp)])
    st <- ptree; st$edge.length <- st$edge.length / h
    shared <- vcv(st, corr = FALSE)
    stan_data$T_shared <- shared[sp, sp, drop = FALSE]
    stan_data$tip_heights <- unname(diag(shared)[sp])
  }

  fit <- sampling(stan_mod, data = stan_data, chains = chains, iter = warmup + samples,
                  warmup = warmup, cores = cores, seed = seed, refresh = 0,
                  control = list(adapt_delta = if (phylo_model == "lambda") 0.99 else 0.999,
                                 max_treedepth = 14))

  extra <- c(if (phylo_model == "lambda") "lambda", if (phylo_model == "OU") "alpha")
  dr <- rstan::extract(fit, pars = c("Intercept_mu_original", "b_mu", "Intercept_zi_original",
                                     "b_zi", "rho", "phi", "sd_phylo", extra))
  b_sd   <- as.numeric(dr$b_mu[, 1])
  int_mu <- as.numeric(dr$Intercept_mu_original)
  int_zi <- as.numeric(dr$Intercept_zi_original)

  s_b <- q3(b_sd); s_u <- q3(b_sd / x_sd); s_i <- q3(int_mu); s_p <- q3(plogis(int_mu))
  s_zi <- q3(int_zi); s_bzi <- q3(dr$b_zi[, 1]); s_zp <- q3(plogis(int_zi))
  s_rho <- q3(dr$rho); s_phi <- q3(dr$phi); s_sd <- q3(dr$sd_phylo)
  p_pos <- mean(b_sd > 0)

  sampler <- get_sampler_params(fit, inc_warmup = FALSE)
  diag_s  <- summary(fit, pars = c("Intercept_mu", "b_mu", "Intercept_zi", "b_zi",
                                   "rho", "sd_phylo", extra))$summary
  bfmi <- vapply(sampler, function(p) mean(diff(p[, "energy__"])^2) / var(p[, "energy__"]), numeric(1))
  divs <- sum(vapply(sampler, function(p) sum(p[, "divergent__"]), numeric(1)))
  tdh  <- sum(vapply(sampler, function(p) sum(p[, "treedepth__"] >= 14), numeric(1)))
  rhat <- max(diag_s[, "Rhat"], na.rm = TRUE)
  ess  <- min(diag_s[, "n_eff"], na.rm = TRUE)

  row <- base %>% mutate(
    n_species = nrow(d), n_zero_case_species = sum(d$y == 0), status = "fit",
    beta_median = s_b[1], beta_lower95 = s_b[2], beta_upper95 = s_b[3],
    OR_median = exp(s_b[1]), OR_lower95 = exp(s_b[2]), OR_upper95 = exp(s_b[3]),
    beta_perUnit_median = s_u[1], beta_perUnit_lower95 = s_u[2], beta_perUnit_upper95 = s_u[3],
    P_positive = p_pos, P_negative = 1 - p_pos,
    significant_95CrI = s_b[2] > 0 | s_b[3] < 0,
    intercept_median = s_i[1], intercept_lower95 = s_i[2], intercept_upper95 = s_i[3],
    prevalence_at_mean_x_median = s_p[1], prevalence_at_mean_x_lower95 = s_p[2], prevalence_at_mean_x_upper95 = s_p[3],
    zi_intercept_median = s_zi[1], zi_intercept_lower95 = s_zi[2], zi_intercept_upper95 = s_zi[3],
    zi_slope_median = s_bzi[1], zi_slope_lower95 = s_bzi[2], zi_slope_upper95 = s_bzi[3],
    zi_prob_median = s_zp[1], zi_prob_lower95 = s_zp[2], zi_prob_upper95 = s_zp[3],
    rho_median = s_rho[1], rho_lower95 = s_rho[2], rho_upper95 = s_rho[3],
    phi_median = s_phi[1], phi_lower95 = s_phi[2], phi_upper95 = s_phi[3],
    sd_phylo_median = s_sd[1], sd_phylo_lower95 = s_sd[2], sd_phylo_upper95 = s_sd[3],
    x_mean = x_center, x_sd = x_sd,
    divergences = divs, treedepth_hits = tdh, min_bfmi = min(bfmi),
    max_rhat = rhat, min_ess = ess,
    diagnostics_ok = divs == 0 && tdh == 0 && min(bfmi) >= 0.3 && rhat <= 1.01 && ess >= 100 * chains
  )
  if (phylo_model == "lambda") { s_l <- q3(dr$lambda)
    row <- row %>% mutate(lambda_median = s_l[1], lambda_lower95 = s_l[2], lambda_upper95 = s_l[3]) }
  if (phylo_model == "OU") { s_a <- q3(dr$alpha)
    row <- row %>% mutate(alpha_median = s_a[1], alpha_lower95 = s_a[2], alpha_upper95 = s_a[3]) }

  grid <- seq(min(d$x), max(d$x), length.out = 200)
  eta  <- outer(int_mu, rep(1, length(grid))) + outer(b_sd, (grid - x_center) / x_sd)
  band <- apply(plogis(eta), 2, quantile, c(0.025, 0.5, 0.975))
  fit_df <- tibble(x = grid, lo = band[1, ], med = band[2, ], hi = band[3, ])

  p <- ggplot() +
    geom_ribbon(data = fit_df, aes(x = x, ymin = lo, ymax = hi), fill = "grey80", alpha = 0.6) +
    geom_line(data = fit_df, aes(x = x, y = med), color = "grey40", linewidth = 1.2) +
    geom_point(data = d, aes(x = x, y = y / trials, color = Class, size = trials)) +
    scale_color_manual(values = class_colors) +
    scale_size_continuous(range = c(3, 8), breaks = c(20, 100, 250, 500),
                          labels = comma, limits = c(0, max(7500, max(d$trials))),
                          name = "Necropsies (n)") +
    guides(color = guide_legend(override.aes = list(size = 4), order = 1),
           size  = guide_legend(override.aes = list(color = "grey40"), order = 2)) +
    scale_y_continuous(labels = function(x) x * 100) +
    labs(x = info$label, y = paste0(outcomes[[yvar]], " Prevalence (%)"),
         title = paste0(outcomes[[yvar]], " Prevalence vs. ", info$label,
                        if (subset != "All") paste0(" (", subset, ")"))) +
    theme_cowplot(12) +
    labs(tag = if (yvar == "NeoplasiaWithDenominators") "A" else "B") +
    theme(plot.tag = element_text(face = "bold", size = 20),
          plot.tag.position = "topleft")

  tag <- paste0(info$file, "_", tolower(outcomes[[yvar]]), if (subset != "All") paste0("_", subset))
  ggsave(file.path(out_dir, "plots", paste0(tag, ".png")), p,
         width = 10, height = 8, bg = "white")
  row
}

results <- list()
for (xvar in names(predictor_list)) {
  for (s in subsets) {
    for (yvar in names(outcomes)) {
      results[[length(results) + 1]] <- zibb_fit(predictor_list[[xvar]]$data, xvar, yvar, s)
    }
  }
}

write.csv(bind_rows(results), file.path(out_dir, paste0("zibb_results_", phylo_model, ".csv")),
          row.names = FALSE)
