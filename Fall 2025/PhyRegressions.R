# =========================================================================
# PGLS: Neoplasia / Malignancy Prevalence vs. Life-History Variables
#   Prevalence, n, SE_simple, Class -> species-cancer-prevalence-data.csv
#   abs_shape                        -> Survivorship_Types_Min20.csv (recalculated Rage)
#   Siler params                     -> Fall 2025/Siler/siler_parameters_all_min50species.csv
# =========================================================================

library(phytools)
library(geiger)
library(caper)
library(tidyverse)
library(cowplot)
library(rr2)
library(nlme)
library(scales)

tree <- read.tree("min20Fixed516.nwk")

class_colors <- c(
  "Amphibia" = "#F8766D",
  "Aves"     = "#7CAE00",
  "Mammalia" = "#00BFC4",
  "Reptilia" = "#C77CFF"
)

MIN_SPECIES <- 10
subsets     <- c("All", names(class_colors))
all_results <- list()

# -------------------------------------------------------------------------
# pglsSEyPagel: PGLS with Pagel's lambda + known measurement error in y
#   V = sig2 * C(lambda) + diag(SE^2), lambda & sig2 fit by ML,
#   then refit as an nlme::gls (fixed corr + fixed variance weights)
#   so summary(), coef(), and rr2::R2 all work as usual.
# -------------------------------------------------------------------------
pglsSEyPagel <- function(model, data, tree, se, method = "ML") {
  data <- data[tree$tip.label, ]
  se   <- se[tree$tip.label]
  y    <- model.response(model.frame(model, data))
  X    <- model.matrix(model, data)
  C    <- vcv(tree)
  n    <- length(y)
  
  lam_C <- function(lambda) { L <- C * lambda; diag(L) <- diag(C); L }
  
  negLL <- function(par) {
    sig2 <- exp(par[1]); lambda <- par[2]
    V  <- sig2 * lam_C(lambda) + diag(se^2, n)
    Vi <- tryCatch(solve(V), error = function(e) NULL)
    if (is.null(Vi)) return(1e10)
    b  <- solve(t(X) %*% Vi %*% X, t(X) %*% Vi %*% y)
    r  <- y - X %*% b
    0.5 * (as.numeric(determinant(V)$modulus) + sum(r * (Vi %*% r)) + n * log(2 * pi))
  }
  
  fit <- optim(c(log(var(y) + 1e-8), 0.5), negLL, method = "L-BFGS-B",
               lower = c(-30, 0), upper = c(10, 1))
  sig2   <- exp(fit$par[1]); lambda <- fit$par[2]
  
  V  <- sig2 * lam_C(lambda) + diag(se^2, n)
  vd <- diag(V)
  R  <- cov2cor(V)
  
  data$.vd <- vd
  m <- gls(model, data = data, method = method,
           weights     = varFixed(~ .vd),
           correlation = corSymm(R[lower.tri(R)], form = ~ 1, fixed = TRUE))
  m$lambda <- lambda
  m$sig2   <- sig2
  m$logLik_ME <- -fit$value
  m
}

# -------------------------------------------------------------------------
# Prevalence data (single source of truth for response, n, SE, Class)
# -------------------------------------------------------------------------
prevalence_data <- read.csv("species-cancer-prevalence-data.csv", check.names = FALSE) %>%
  mutate(Species = gsub(" ", "_", Species),
         n       = RecordsWithDenominators) %>%
  distinct(Species, .keep_all = TRUE) %>%
  select(Species, Class, NeoplasiaPrevalence, MalignancyPrevalence, n, SE_simple)

# If prevalence is stored as 0-100, convert to proportions (so % axis is right)
if (max(prevalence_data$NeoplasiaPrevalence, na.rm = TRUE) > 1) {
  prevalence_data <- prevalence_data %>%
    mutate(NeoplasiaPrevalence  = NeoplasiaPrevalence / 100,
           MalignancyPrevalence = MalignancyPrevalence / 100)
  cat("Prevalence looked like percents -> converted to proportions.\n")
}

# Drop any old copies of these columns, then attach the new ones
drop_cols <- c("Class", "neoplasia_prevalence", "cancer_prevalence",
               "NeoplasiaPrevalence", "MalignancyPrevalence",
               "SE_simple", "n", "RecordsWithDenominators")

add_prevalence <- function(df) {
  df %>%
    mutate(Species = gsub(" ", "_", Species)) %>%
    select(-any_of(drop_cols)) %>%
    inner_join(prevalence_data, by = "Species")
}

outcomes <- c(NeoplasiaPrevalence = "Neoplasia", MalignancyPrevalence = "Malignancy")

# -------------------------------------------------------------------------
# Helper: clean + match data and tree
# -------------------------------------------------------------------------
prep_data <- function(data, tree, xvar, yvar) {
  data <- data[complete.cases(data[, c(xvar, yvar, "n", "SE_simple")]), ]
  data <- data[data$Species %in% tree$tip.label, ]
  pruned.tree <- drop.tip(tree, setdiff(tree$tip.label, data$Species))
  rownames(data) <- data$Species
  data <- data[pruned.tree$tip.label, ]
  SE <- setNames(data$SE_simple, data$Species)
  list(data = data, tree = pruned.tree, SE = SE)
}

# -------------------------------------------------------------------------
# Helper: fit + plot one subset
# -------------------------------------------------------------------------
pgls_plot <- function(data, tree, xvar, yvar, xlab, ylab, title, file, subset = "All") {
  
  n_all <- data$n
  if (subset != "All") data <- data[data$Class == subset, ]
  prep  <- prep_data(data, tree, xvar, yvar)
  label <- paste0(yvar, " ~ ", xvar, " | ", subset)
  cat("\n==========", label, "==========\n")
  
  if (nrow(prep$data) < MIN_SPECIES) {
    cat("  - Skipping (only", nrow(prep$data), "species)\n")
    return(invisible(NULL))
  }
  
  form  <- as.formula(paste(yvar, "~", xvar))
  model <- tryCatch(
    pglsSEyPagel(form, data = prep$data, tree = prep$tree, se = prep$SE, method = "ML"),
    error = function(e) { message("  ! PGLS failed: ", conditionMessage(e)); NULL }
  )
  if (is.null(model)) return(invisible(NULL))
  print(summary(model))
  cat("  lambda =", round(model$lambda, 3), "\n")
  
  tt <- summary(model)$tTable
  all_results[[label]] <<- tibble(
    subset    = subset,
    x         = xvar,
    y         = yvar,
    n_species = nrow(prep$data),
    intercept = tt[1, 1],
    slope     = tt[2, 1],
    p_value   = tt[2, 4],
    R2        = tryCatch(as.numeric(R2(phy = prep$tree, model)[3]), error = function(e) NA),
    lambda    = model$lambda
  )
  
  if (subset != "All") {
    title <- paste0(title, " (", subset, ")")
    file  <- sub("\\.png$", paste0("_", subset, ".png"), file)
  }
  
  n_lim  <- range(n_all, na.rm = TRUE)
  n_brks <- c(20, 50, 100, 250, 500)
  n_brks <- n_brks[n_brks >= n_lim[1] & n_brks <= n_lim[2]]
  
  p <- ggplot(prep$data, aes(x = .data[[xvar]], y = .data[[yvar]],
                             color = Class, size = n)) +
    geom_point(alpha = 1) +
    scale_color_manual(values = class_colors) +
    scale_size_continuous(range  = c(1.5, 7),
                          breaks = n_brks,
                          labels = scales::comma,
                          limits = n_lim,
                          name   = "Necropsies") +
    guides(color = guide_legend(override.aes = list(size = 4), order = 1),
           size  = guide_legend(override.aes = list(color = "grey40"), order = 2)) +
    geom_abline(intercept = coef(model)[1], slope = coef(model)[2],
                color = "grey", linewidth = 1.2) +
    scale_y_continuous(labels = function(x) x * 100) +
    labs(x = xlab, y = ylab, color = NULL) +
    labs(tag = if (xvar == "b3" && subset == "Mammalia") {
      if (yvar == "NeoplasiaPrevalence") "C" else "D"
    } else {
      if (yvar == "NeoplasiaPrevalence") "A" else "B"
    }) +
    theme_cowplot(12) +
    theme(legend.position = "right",
          plot.tag = element_text(face = "bold", size = 20),
          plot.tag.position = "topleft")
  
  print(p)
  ggsave(file, p, width = 10, height = 8, limitsize = FALSE, bg = "white")
  plot_store[[paste(xvar, subset, yvar, sep = "|")]] <<- p
  invisible(p)
}

plot_store <- list()

save_panel <- function(plots, file, ncol = 2) {
  plots <- Filter(Negate(is.null), plots)
  if (length(plots) == 0) return(invisible(NULL))
  legend <- get_legend(plots[[1]] + theme(legend.box.margin = margin(0, 0, 0, 12)))
  panels <- lapply(plots, function(p) p + theme(legend.position = "none"))
  grid   <- plot_grid(plotlist = panels, ncol = ncol, align = "hv")
  out    <- plot_grid(grid, legend, ncol = 2, rel_widths = c(1, 0.15))
  nrow   <- ceiling(length(panels) / ncol)
  ggsave(file, out, width = 9 * ncol + 2, height = 7.5 * nrow, limitsize = FALSE, bg = "white")
  invisible(out)
}

make_panels <- function(xvar, prefix) {
  for (s in subsets) {
    panel_file <- paste0(prefix, if (s != "All") paste0("_", s), "_panel.png")
    tryCatch(
      save_panel(list(plot_store[[paste(xvar, s, "NeoplasiaPrevalence", sep = "|")]],
                      plot_store[[paste(xvar, s, "MalignancyPrevalence", sep = "|")]]),
                 panel_file),
      error = function(e) message("  ! Panel failed (", panel_file, "): ", conditionMessage(e)))
  }
}

# #########################################################################
# PART 1: DISTANCE FROM TYPE II SURVIVORSHIP
# #########################################################################
# Recalculated Rage shape values (from RecalculatingRageNumbers.R)
data <- read.csv("Survivorship_Types_Min20.csv") %>%
  mutate(shape_value = shape_type,
         abs_shape   = abs(shape_type)) %>%
  add_prevalence()
cat("Shape + prevalence species:", nrow(data), "\n")
print(summary(data$n))

for (s in subsets) {
  for (yvar in names(outcomes)) {
    pgls_plot(data, tree, "abs_shape", yvar,
              "Distance from Type II Survivorship",
              paste0(outcomes[[yvar]], " Prevalence (%)"),
              paste0(outcomes[[yvar]], " Prevalence vs. Distance from Type II Survivorship"),
              paste0("abs_shape_", tolower(outcomes[[yvar]]), ".png"),
              subset = s)
  }
}
make_panels("abs_shape", "abs_shape")

# #########################################################################
# PART 2: SILER PARAMETERS
# #########################################################################
data <- read.csv("Fall 2025/Siler/siler_parameters_all_min50species.csv") %>%
  filter(as.character(FLAG) == "0") %>%
  add_prevalence()
cat("Siler + prevalence species:", nrow(data), "\n")
print(summary(data$n))

siler_params <- c(
  a1 = "Initial Juvenile Mortality Risk (a1)",
  b1 = "Rate of Decline in Juvenile Mortality Risk (b1)",
  a2 = "Age-Independent Mortality Risk (a2)",
  a3 = "Initial Senescent Mortality Risk (a3)",
  b3 = "Change in Mortality Risk in the Senescent Stage (b3)"
)

missing <- setdiff(names(siler_params), names(data))
if (length(missing) > 0) {
  cat("Not found in Siler CSV, skipping:", paste(missing, collapse = ", "), "\n")
  siler_params <- siler_params[names(siler_params) %in% names(data)]
}

for (param in names(siler_params)) {
  for (s in subsets) {
    for (yvar in names(outcomes)) {
      pgls_plot(data, tree, param, yvar,
                siler_params[[param]],
                paste0(outcomes[[yvar]], " Prevalence (%)"),
                paste0(outcomes[[yvar]], " Prevalence vs. ", siler_params[[param]]),
                paste0("siler_", param, "_", tolower(outcomes[[yvar]]), ".png"),
                subset = s)
    }
  }
}

for (param in names(siler_params)) make_panels(param, paste0("siler_", param))

tryCatch(save_panel(list(plot_store[["b3|All|NeoplasiaPrevalence"]],
                plot_store[["b3|All|MalignancyPrevalence"]],
                plot_store[["b3|Mammalia|NeoplasiaPrevalence"]],
                plot_store[["b3|Mammalia|MalignancyPrevalence"]]),
           "siler_b3_All_Mammalia_panel.png"),
         error = function(e) message("  ! Panel failed (siler_b3_All_Mammalia_panel.png): ", conditionMessage(e)))

# #########################################################################
# SUMMARY TABLE
# #########################################################################
if (length(all_results) == 0) stop("No models fit - check the console for '! PGLS failed' messages.")

results_table <- bind_rows(all_results) %>%
  mutate(across(c(intercept, slope, p_value, R2, lambda), ~ signif(.x, 3)))
print(results_table, n = Inf)
write.csv(results_table, "pgls_results_by_class.csv", row.names = FALSE)
cat("\nDone. Results saved to pgls_results_by_class.csv\n")