# Survivorship curve shape and cancer prevalence across vertebrates

## Structure

- `data/` – input data
  - `cleanPath.min20.062822.csv` – individual necropsy records
  - `species-cancer-prevalence-data.csv` – species-level neoplasia and malignancy counts
  - `min20Fixed516.nwk` – phylogeny
  - `species_aliases.csv` – taxonomic name matches between data and tree
  - `siler_species_list.csv` – species included in Siler model fitting
- `stan/` – zero-inflated beta-binomial phylogenetic GLMM (ZiBB-PGLMM) models
- `R/` – analysis scripts
- `figures/` – supplementary figures
  - `Figure_S1_siler_a1.png` – prevalence vs. initial juvenile mortality risk (a1)
  - `Figure_S2_siler_b1.png` – prevalence vs. rate of decline in juvenile mortality risk (b1)
  - `Figure_S3_siler_a2.png` – prevalence vs. age-independent mortality risk (a2)
  - `Figure_S4_siler_a3.png` – prevalence vs. initial senescent mortality risk (a3)

## Run order

Run from this folder as the working directory. Outputs are written to `results/`.

1. `R/01_rage_shape.R` – Rage survivorship shape values and types
2. `R/02_siler_parameters.R` – life tables and Siler mortality parameters
3. `R/03_zibb_regressions.R` – ZiBB-PGLMM regressions of neoplasia and malignancy on Rage survivorship shape value and Siler parameters
4. `R/04_violin_plots.R` – prevalence by survivorship type
5. `R/05_survivorship_curves.R` – survivorship curves by class

## Requirements

R packages: dplyr, tidyverse, Rage, fmsb, ape, rstan (>= 2.26), ggplot2, cowplot, scales

The ZiBB-PGLMM Stan models are from https://github.com/VNF1981/Zero-Inflated-Beta-Binomial-PGLMM-for-Analyzing-the-Cross-Species-Cancer-Data
