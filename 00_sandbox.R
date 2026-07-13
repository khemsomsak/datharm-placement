########################################################
#  10_sandbox.R                                        #
#  Exploratory analysis workspace                      #
#  Purpose: respond to supervisor (Prabin) feedback    #
#  on dose-count zero-inflation and ZINB suitability;  #
#  general scratch space for ad hoc exploration.       #
#  Created: 07/07/2026                                 #
#  Last Updated: 07/07/2026                            #
#                                                      #
#  This script LOADS existing processed outputs and    #
#  raw tables so exploration can begin immediately     #
#  without rerunning the 01-08 pipeline. It writes     #
#  nothing back to canonical output directories;       #
#  all exploratory output goes to 03_output/10_sandbox #
########################################################

# Reset environment -----------------------------------------------------

rm(list = ls())
setwd("C:/Users/HP/Documents/GitHub/datharm-placement")
options(scipen = 999)
Sys.setlocale("LC_TIME", "English")

#Set link shortcuts
home        <- "C:/Users/HP/Documents/GitHub/datharm-placement"
mchtrack_dir<- file.path(home, "03_output/01_mchtrack_data")
reg_dir     <- file.path(home, "03_output/03_regression")
resid_dir   <- file.path(home, "03_output/04_ward_residuals")
sandbox_dir <- file.path(home, "03_output/10_sandbox")
dir.create(sandbox_dir, showWarnings = FALSE, recursive = TRUE)

#Routine packages
library(tidyverse)
library(lubridate)
library(fixest)
library(ggplot2)
library(patchwork)
library(scales)

#Packages specific to zero-inflation exploration
# install.packages("pscl") if not present — for zeroinfl()
suppressWarnings(suppressMessages({
  have_pscl <- requireNamespace("pscl", quietly = TRUE)
  have_mass <- requireNamespace("MASS", quietly = TRUE)
}))
if (have_pscl) library(pscl)
if (have_mass) library(MASS)

#----------------------------------------------------------------------------

###################
# Load everything #
###################

# 1. Cleaned MCHTrack tables (script 01 outputs) ------------------------------
data_ll_clean    <- readRDS(file.path(mchtrack_dir, "01_linelisted_clean.rds"))
data_fv_clean    <- readRDS(file.path(mchtrack_dir, "01_facility_visits_clean.rds"))
data_idzd_clean  <- readRDS(file.path(mchtrack_dir, "01_identifiedzd_clean.rds"))
data_dt_clean    <- readRDS(file.path(mchtrack_dir, "01_defaultertracing_clean.rds"))
data_any_vaccine <- readRDS(file.path(mchtrack_dir, "01_any_vaccine_flag.rds"))

# 2. Analysis datasets (script 03 outputs) ------------------------------------
data_model_a     <- readRDS(file.path(reg_dir, "03_model_a_dataset.rds"))
data_model_b     <- readRDS(file.path(reg_dir, "03_model_b_dataset.rds"))

# 3. Ward residuals (script 04 output) ----------------------------------------
data_ward_res <- tryCatch(
  readRDS(file.path(reg_dir, "03_ward_residuals_model_a.rds")),
  error = function(e) NULL)

cat("Loaded objects:\n")
cat("  linelisted:      ", nrow(data_ll_clean),   "rows\n")
cat("  facility_visits: ", nrow(data_fv_clean),   "rows\n")
cat("  model_a:         ", nrow(data_model_a),    "rows\n")
cat("  model_b:         ", nrow(data_model_b),    "rows\n\n")

#----------------------------------------------------------------------------

##########################################
# Construct per-child vaccine dose count #
##########################################

# Prabin's request: distribution of number of doses per child, to gauge
# zero-inflation. Dose count = number of immunisation-flagged facility
# visits per child. A child in the primary sample with no immunisation
# visit record has a dose count of zero.

# 4. Count immunisation visits per child --------------------------------------
data_dose_counts <- data_fv_clean %>%
  filter(is_immunization == TRUE) %>%
  count(patient_id, name = "n_doses")

# 5. Join onto primary analytic sample; missing = zero doses ------------------
data_doses <- data_model_a %>%
  filter(in_primary_sample) %>%
  left_join(data_dose_counts, by = c("pseudo_id" = "patient_id")) %>%
  mutate(n_doses = replace_na(n_doses, 0L))

#Validation summary ----
cat("Dose-count construction:\n")
data_doses %>%
  summarise(
    n_children     = n(),
    pct_zero       = round(mean(n_doses == 0) * 100, 1),
    mean_doses     = round(mean(n_doses), 2),
    var_doses      = round(var(n_doses), 2),
    vm_ratio       = round(var(n_doses) / mean(n_doses), 2),
    max_doses      = max(n_doses)
  ) %>%
  print()

# Poisson-implied zero share for comparison (the key zero-inflation test) ----
lambda_hat  <- mean(data_doses$n_doses)
pois_p0     <- exp(-lambda_hat)
obs_p0      <- mean(data_doses$n_doses == 0)
cat("\nZero-inflation check:\n")
cat("  Observed P(0 doses):        ", round(obs_p0 * 100, 1), "%\n")
cat("  Poisson-implied P(0):       ", round(pois_p0 * 100, 1), "%\n")
cat("  Excess zeros (obs - Pois):  ", round((obs_p0 - pois_p0) * 100, 1), "pp\n\n")

#----------------------------------------------------------------------------

#################################
# Shared aesthetic (from paper) #
#################################

col_zd     <- "#C0312D"    # zero-dose / structural-zero red
col_count  <- "#1D6FA4"    # count-process blue
col_pois   <- "#BA7517"    # Poisson-reference amber
col_nb     <- "#1D9E75"    # negative-binomial green
grey_mid   <- "#888780"

theme_diss <- function(bs = 12) {
  theme_minimal(base_size = bs) %+replace% theme(
    text               = element_text(family = "serif"),
    plot.title         = element_text(face = "bold", size = bs, hjust = 0, margin = margin(b = 4)),
    plot.subtitle      = element_text(size = bs - 2, colour = grey_mid, hjust = 0, margin = margin(b = 10)),
    plot.caption       = element_text(size = 9, colour = grey_mid, hjust = 0, margin = margin(t = 8), lineheight = 1.1),
    axis.title         = element_text(size = bs - 1),
    axis.text          = element_text(size = bs - 1, colour = "#333"),
    panel.grid.major.y = element_line(colour = "#eeeeee", linewidth = 0.4),
    panel.grid.major.x = element_blank(),
    panel.grid.minor   = element_blank(),
    legend.position    = "top", legend.title = element_blank())
}

#----------------------------------------------------------------------------

######################################################
# VISUAL 1 — Dose-count distribution (Prabin's ask)  #
######################################################

# Exactly what was requested: x = number of doses, y = number of children.
# The spike at zero is the visual answer to "how zero-inflated is this?"

dose_dist <- data_doses %>%
  mutate(dose_grp = if_else(n_doses >= 8, "8+", as.character(n_doses)),
         dose_grp = factor(dose_grp, levels = c(as.character(0:7), "8+"))) %>%
  count(dose_grp)

v1 <- ggplot(dose_dist, aes(x = dose_grp, y = n)) +
  geom_col(aes(fill = dose_grp == "0"), width = 0.72) +
  geom_text(aes(label = comma(n)), vjust = -0.4, size = 3.0, family = "serif", colour = "#333") +
  scale_fill_manual(values = c("TRUE" = col_zd, "FALSE" = col_count), guide = "none") +
  scale_y_continuous(labels = comma, expand = expansion(mult = c(0, 0.12))) +
  labs(
    title    = "Figure 3.1d.  Distribution of recorded vaccine doses per child",
    subtitle = "Primary analytic sample · dose = immunisation-flagged facility visit · N = 70,175",
    x        = "Number of recorded doses",
    y        = "Number of children",
    caption  = "The bar at zero (red) holds 35% of children. Whether this constitutes true zero-inflation is tested in the two panels below.") +
  theme_diss()

ggsave(file.path(sandbox_dir, "10_dose_distribution.png"),
       v1, width = 8, height = 4.5, dpi = 150)

#----------------------------------------------------------------------------

##########################################################
# VISUAL 2 — Observed vs Poisson vs NegBin fitted zeros   #
##########################################################

# The core diagnostic for ZINB suitability. If the observed zero bar
# towers over what BOTH Poisson and a plain negative binomial predict,
# a zero-inflated model is justified. If a negative binomial already
# captures the zeros through overdispersion alone, ZINB adds little.

# Fit a plain negative binomial to get its implied zero probability
if (have_mass) {
  nb_fit  <- MASS::glm.nb(n_doses ~ 1, data = data_doses)
  nb_mu   <- exp(coef(nb_fit)[1])
  nb_theta<- nb_fit$theta
  nb_p0   <- (nb_theta / (nb_theta + nb_mu))^nb_theta
} else {
  nb_p0 <- NA_real_
}

# Build comparison across dose values 0..7 for each distribution
max_k   <- 7
obs_tab <- data_doses %>%
  mutate(k = pmin(n_doses, max_k)) %>%
  count(k) %>% mutate(prop = n / sum(n))

k_vals  <- 0:max_k
pois_pr <- dpois(k_vals, lambda_hat); pois_pr[max_k+1] <- 1 - sum(dpois(0:(max_k-1), lambda_hat))
if (have_mass) {
  nb_pr <- dnbinom(k_vals, size = nb_theta, mu = nb_mu)
  nb_pr[max_k+1] <- 1 - sum(dnbinom(0:(max_k-1), size = nb_theta, mu = nb_mu))
} else { nb_pr <- rep(NA_real_, length(k_vals)) }

cmp <- tibble(k = k_vals,
              Observed = obs_tab$prop[match(k_vals, obs_tab$k)],
              Poisson  = pois_pr,
              `Negative binomial` = nb_pr) %>%
  pivot_longer(-k, names_to = "dist", values_to = "prop") %>%
  mutate(dist = factor(dist, levels = c("Observed","Poisson","Negative binomial")),
         k_lab = if_else(k == max_k, paste0(max_k,"+"), as.character(k)))

v2 <- ggplot(cmp, aes(x = factor(k_lab, levels = c(as.character(0:(max_k-1)), paste0(max_k,"+"))),
                      y = prop, fill = dist)) +
  geom_col(position = position_dodge(0.72), width = 0.66) +
  scale_fill_manual(values = c("Observed" = col_zd, "Poisson" = col_pois,
                               "Negative binomial" = col_nb)) +
  scale_y_continuous(labels = label_percent(accuracy = 1),
                     expand = expansion(mult = c(0, 0.05))) +
  labs(
    title    = "Figure 3.1e.  Observed dose distribution vs Poisson and negative binomial fits",
    subtitle = "If observed zeros exceed BOTH fitted distributions, a zero-inflated model is warranted",
    x        = "Number of recorded doses",
    y        = "Share of children",
    caption  = paste0("Observed zeros: ", round(obs_p0*100,1),
                      "%. Poisson-implied: ", round(pois_p0*100,1),
                      "%. Negative-binomial-implied: ",
                      ifelse(is.na(nb_p0),"n/a",paste0(round(nb_p0*100,1),"%")),
                      ". The negative binomial's overdispersion may absorb much of the excess on its own.")) +
  theme_diss()

ggsave(file.path(sandbox_dir, "10_zeroinflation_fit_comparison.png"),
       v2, width = 8.5, height = 4.8, dpi = 150)

#----------------------------------------------------------------------------

##############################################################
# VISUAL 3 — Are the zeros structurally different? (the real  #
# question behind ZINB: two populations or one?)             #
##############################################################

# ZINB assumes zeros come from two processes: structural non-starters
# and count-process zeros. If zero-dose children differ systematically
# on the predictors (esp. distance), that supports a two-population
# model. If they look like a smooth continuation of the count, a
# single-process negative binomial is more parsimonious.

dose_by_pred <- data_doses %>%
  mutate(dose_grp = if_else(n_doses >= 6, "6+", as.character(n_doses)),
         dose_grp = factor(dose_grp, levels = c(as.character(0:5), "6+"))) %>%
  group_by(dose_grp) %>%
  summarise(
    med_distance = median(hf_distance_km, na.rm = TRUE),
    mean_distance= mean(hf_distance_km, na.rm = TRUE),
    med_age      = median(age_months_at_reg, na.rm = TRUE),
    n            = n(),
    .groups = "drop")

v3a <- ggplot(dose_by_pred, aes(x = dose_grp, y = med_distance, group = 1)) +
  geom_line(colour = col_count, linewidth = 0.8) +
  geom_point(aes(size = n), colour = col_count) +
  geom_point(data = ~filter(.x, dose_grp == "0"), colour = col_zd, size = 4) +
  scale_size_continuous(range = c(2, 6), guide = "none") +
  scale_y_continuous(labels = label_number(suffix = " km")) +
  labs(subtitle = "A. Median distance to facility by dose count",
       x = "Recorded doses", y = "Median distance") +
  theme_diss(11)

v3b <- ggplot(dose_by_pred, aes(x = dose_grp, y = med_age, group = 1)) +
  geom_line(colour = col_count, linewidth = 0.8) +
  geom_point(aes(size = n), colour = col_count) +
  geom_point(data = ~filter(.x, dose_grp == "0"), colour = col_zd, size = 4) +
  scale_size_continuous(range = c(2, 6), guide = "none") +
  labs(subtitle = "B. Median age at registration by dose count",
       x = "Recorded doses", y = "Median age (months)") +
  theme_diss(11)

v3 <- (v3a | v3b) +
  plot_annotation(
    title    = "Figure 3.1f.  Do zero-dose children differ structurally, or continue the trend?",
    subtitle = "Red point = zero-dose group. A discontinuity at zero supports two populations (ZINB); a smooth trend supports one (NB)",
    caption  = "If the zero group sits off the line traced by 1, 2, 3+ dose children, its zeros are structurally distinct and a zero-inflated\nmodel is justified. If it continues the trend, overdispersion in a single negative binomial already accounts for the zeros.",
    theme = theme(plot.title = element_text(family="serif",face="bold",size=12.5),
                  plot.subtitle = element_text(family="serif",size=10,colour=grey_mid),
                  plot.caption = element_text(family="serif",size=9,colour=grey_mid,hjust=0,lineheight=1.1)))

ggsave(file.path(sandbox_dir, "10_structural_zero_check.png"),
       v3, width = 9, height = 4.5, dpi = 150)

#----------------------------------------------------------------------------

##################################################
# Optional: fit ZINB and compare to NB via AIC   #
##################################################

# Only runs if pscl is available. Provides the quantitative complement
# to the visual diagnostics: does ZINB actually beat a plain NB on fit?
# Kept minimal — no fixed effects here (pscl does not absorb them),
# so this is an indicative model comparison, NOT the thesis specification.

if (have_pscl && have_mass) {
  cat("Fitting indicative NB and ZINB (no fixed effects; pscl limitation)...\n")
  
  # Plain negative binomial with predictors
  m_nb <- MASS::glm.nb(n_doses ~ hf_distance_km + age_months_at_reg + gender_female,
                       data = data_doses)
  
  # Zero-inflated negative binomial: same count predictors,
  # distance in the zero-inflation part (structural-zero process)
  m_zinb <- pscl::zeroinfl(
    n_doses ~ hf_distance_km + age_months_at_reg + gender_female |
      hf_distance_km + age_months_at_reg,
    data = data_doses, dist = "negbin")
  
  aic_nb   <- AIC(m_nb)
  aic_zinb <- AIC(m_zinb)
  
  cat("\nModel comparison (lower AIC = better fit):\n")
  cat("  Negative binomial AIC:          ", round(aic_nb, 0), "\n")
  cat("  Zero-inflated NB AIC:           ", round(aic_zinb, 0), "\n")
  cat("  AIC improvement from ZINB:      ", round(aic_nb - aic_zinb, 0), "\n")
  cat("  (Vuong test available via pscl::vuong(m_nb, m_zinb) if desired)\n\n")
  
  # Save a compact summary
  zinb_summary <- tibble(
    model = c("Negative binomial", "Zero-inflated NB"),
    aic   = c(aic_nb, aic_zinb),
    note  = c("single count process",
              "structural zeros + count process"))
  write_csv(zinb_summary, file.path(sandbox_dir, "10_zinb_vs_nb_aic.csv"))
} else {
  cat("pscl and/or MASS not installed — skipping ZINB fit.\n")
  cat("Install with: install.packages(c('pscl','MASS'))\n\n")
}

#----------------------------------------------------------------------------

# Save the dose-count dataset for any further exploration
saveRDS(data_doses, file.path(sandbox_dir, "10_dose_counts_dataset.rds"))
write_csv(
  data_doses %>% dplyr::select(pseudo_id, state, lga_name, n_doses,
                               zero_dose_penta, zero_dose_truly,
                               hf_distance_km, age_months_at_reg),
  file.path(sandbox_dir, "10_dose_counts_dataset.csv"))

cat("Sandbox outputs written to:", sandbox_dir, "\n")
cat("  10_dose_distribution.png            (Prabin's requested figure)\n")
cat("  10_zeroinflation_fit_comparison.png (ZINB justification test 1)\n")
cat("  10_structural_zero_check.png        (ZINB justification test 2)\n")
cat("  10_dose_counts_dataset.rds/.csv     (for further exploration)\n")
if (have_pscl) cat("  10_zinb_vs_nb_aic.csv               (model comparison)\n")

#--------------------------(END)------------------------------#