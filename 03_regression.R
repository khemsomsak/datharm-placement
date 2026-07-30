########################################################
#  Child-Level Regression Analysis                     #
#  ZD Predictors & Tracing Effectiveness               #
#  Created on 28/5/2026                                #
#  Last Updated 30/7/2026 - excluded implausible ages  #
########################################################

# Reset environment -----------------------------------------------------

rm(list = ls())
setwd("C:/Users/HP/Documents/GitHub/datharm-placement")

# Turn off scientific notation globally
options(scipen = 999)
Sys.setlocale("LC_TIME", "English")

#Set link shortcuts
home         <- "C:/Users/HP/Documents/GitHub/datharm-placement"
mchtrack_dir <- file.path(home, "03_output/01_mchtrack_data")
out_dir      <- file.path(home, "03_output/03_regression")
dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)

#Routine Packages
library(janitor)
library(tidyverse)
library(dplyr)
library(tidyr)
library(ggplot2)
library(readr)
library(lubridate)
library(broom)
library(fixest)        # feglm() for logistic with fixed effects
library(modelsummary)  # clean regression output tables

#----------------------------------------------------------------------------

###################
# Import Database #
###################

# 1. Load cleaned MCHTrack tables from 01_mchtrack_import.R outputs ----------

data_ll_clean    <- readRDS(file.path(mchtrack_dir, "01_linelisted_clean.rds"))
data_dt_clean    <- readRDS(file.path(mchtrack_dir, "01_defaultertracing_clean.rds"))
data_any_vaccine <- readRDS(file.path(mchtrack_dir, "01_any_vaccine_flag.rds"))
data_fv_clean    <- readRDS(file.path(mchtrack_dir, "01_facility_visits_clean.rds"))


# 2. Row count checks ---------------------------------------------------------

cat("Loaded row counts:\n")
cat("  linelisted:       ", nrow(data_ll_clean),    "\n")
cat("  defaulterTracing: ", nrow(data_dt_clean),    "\n")
cat("  any_vaccine_flag: ", nrow(data_any_vaccine), "\n\n")


############
# Analysis #
############

#----------------------------#Model A: Zero-Dose Predictors#----------------#

# 3. Build child-level analysis dataset for Model A ---------------------------

#Filter to children only; join vaccine contact flag for truly-ZD definition ----
data_model_a <- data_ll_clean %>%
  filter(woman_or_child == "child") %>%
  left_join(data_any_vaccine,
            by = c("pseudo_id" = "patient_id")) %>%
  mutate(
    
    # Definition 1 (primary): MCHTrack operational penta-ZD flag, 12-23m window
    zero_dose_penta    = as.integer(zero_dose == TRUE),
    
    # Definition 2: truly zero dose — no vaccine of any kind recorded in MCHTrack
    # note: cannot detect off-network BCG/OPV0 so this is a lower bound on true ZD
    zero_dose_truly    = as.integer(is.na(has_any_vaccine_record)),
    
    # Definition 4: age-flexible — penta-ZD flag regardless of age window
    zero_dose_ageflex  = as.integer(zero_dose == TRUE),
    
    # age at registration in months
    age_months_at_reg  = age_years * 12 +
      coalesce(age_months, 0) +
      coalesce(age_weeks,  0) / 4.33,
    
    # binary gender
    gender_female      = as.integer(gender == "female"),
    
    # primary sample excludes Rimi LGA backfill
    in_primary_sample  = !rimi_flag,
    
    # implausible age (>180m) excluded as data-entry error -- Decision Log, Appendix A
    age_implausible    = !is.na(age_months_at_reg) & age_months_at_reg > 180
    
  ) %>%
  filter(!is.na(hf_distance_km))

#Apply: implausible-age exclusion ----
cat("Model A: excluding", sum(data_model_a$age_implausible), "implausible-age rows\n\n")
data_model_a <- data_model_a %>% filter(!age_implausible)

#Validate: zero-dose rates and sample composition ----
cat("Model A dataset:\n")
data_model_a %>%
  filter(in_primary_sample) %>%
  summarise(
    n_children     = n(),
    zd_penta_pct   = round(mean(zero_dose_penta,  na.rm = TRUE) * 100, 1),
    zd_truly_pct   = round(mean(zero_dose_truly,  na.rm = TRUE) * 100, 1),
    pct_female     = round(mean(gender_female,     na.rm = TRUE) * 100, 1),
    median_dist_km = round(median(hf_distance_km, na.rm = TRUE), 2),
    median_age_reg = round(median(age_months_at_reg, na.rm = TRUE), 1)
  ) %>%
  print()
cat("\n")

# 4. Model A1: primary definition, LGA fixed effects -------------------------

m_a1 <- feglm(
  zero_dose_penta ~ hf_distance_km + gender_female + age_months_at_reg
  | lga_name,
  data   = data_model_a %>% filter(in_primary_sample),
  family = binomial,
  vcov   = ~facility_ward   # cluster SE at ward level
)

# 5. Model A2: robustness — truly zero dose definition -----------------------

m_a2 <- feglm(
  zero_dose_truly ~ hf_distance_km + gender_female + age_months_at_reg
  | lga_name,
  data   = data_model_a %>% filter(in_primary_sample),
  family = binomial,
  vcov   = ~facility_ward
)

# 6. Model A3: robustness — include Rimi LGA ----------------------------------

m_a3 <- feglm(
  zero_dose_penta ~ hf_distance_km + gender_female + age_months_at_reg
  | lga_name,
  data   = data_model_a,   # full sample including Rimi
  family = binomial,
  vcov   = ~facility_ward
)

# 7. Model A4: robustness — state interaction on distance ---------------------
# tests whether distance effect differs between urban Kano and rural Katsina

m_a4 <- feglm(
  zero_dose_penta ~ hf_distance_km * state + gender_female + age_months_at_reg
  | lga_name,
  data   = data_model_a %>% filter(in_primary_sample),
  family = binomial,
  vcov   = ~facility_ward
)

# 8. Regression table: Model A ------------------------------------------------

modelsummary(
  list("Primary (penta-ZD)"  = m_a1,
       "Truly ZD"            = m_a2,
       "Incl. Rimi"          = m_a3,
       "State × distance"    = m_a4),
  stars       = c("*" = 0.1, "**" = 0.05, "***" = 0.01),
  coef_rename = c(
    "hf_distance_km"              = "HF distance (km)",
    "gender_female"               = "Female",
    "age_months_at_reg"           = "Age at registration (months)",
    "stateKatsina"                = "Katsina",
    "hf_distance_km:stateKatsina" = "HF distance × Katsina"
  ),
  gof_map = c("nobs","r.squared","FE: lga_name"),
  output  = file.path(out_dir, "01_model_a_zerodose_predictors.txt")
)

modelsummary(
  list("Primary" = m_a1, "Truly ZD" = m_a2,
       "Incl. Rimi" = m_a3, "State × dist" = m_a4),
  stars = c("*" = 0.1, "**" = 0.05, "***" = 0.01)
)

# 9. Ward-level residuals from Model A1 ---------------------------------------
# wards with large positive residuals = higher ZD than predictors explain
# operationally useful as a targeting output for programme team

data_ward_residuals <- data_model_a %>%
  filter(in_primary_sample, !is.na(facility_ward)) %>%
  mutate(fitted = predict(m_a1, newdata = ., type = "response")) %>%
  group_by(state, lga_name, facility_ward) %>%
  summarise(
    n_children      = n(),
    zd_observed_pct = round(mean(zero_dose_penta, na.rm = TRUE) * 100, 1),
    zd_fitted_pct   = round(mean(fitted,           na.rm = TRUE) * 100, 1),
    residual        = zd_observed_pct - zd_fitted_pct,
    .groups = "drop"
  ) %>%
  filter(n_children >= 30) %>%
  arrange(desc(residual))

cat("Ward residuals — top 10 (higher ZD than model predicts):\n")
print(head(data_ward_residuals, 10))
cat("\nWard residuals — bottom 10 (lower ZD than model predicts):\n")
print(tail(data_ward_residuals, 10))
cat("\n")


#----------------------------#Model B: Tracing Effectiveness#---------------#

# 10. Build child-level analysis dataset for Model B --------------------------

#Build lag time: days between last facility visit and tracing contact ----
# join most recent pre-tracing visit date per patient from facility visits

data_last_visit <- data_fv_clean %>%
  filter(woman_or_child == "child") %>%
  select(patient_id, visit_date) %>%
  rename(fv_visit_date = visit_date)

data_dt_lagged <- data_dt_clean %>%
  mutate(tracing_date = as.Date(created_on)) %>%
  left_join(data_last_visit, by = c("patient_id"),
            relationship = "many-to-many") %>%
  filter(fv_visit_date < tracing_date) %>%
  group_by(id) %>%
  slice_max(fv_visit_date, n = 1, with_ties = FALSE) %>%
  ungroup() %>%
  mutate(
    days_since_visit = as.numeric(tracing_date - fv_visit_date)
  ) %>%
  select(id, fv_visit_date, tracing_date, days_since_visit)

#Join back onto tracing records ----
data_dt_clean <- data_dt_clean %>%
  left_join(data_dt_lagged, by = "id")

cat("Lag time variable — coverage:\n")
cat("  Records with lag time: ",
    sum(!is.na(data_dt_clean$days_since_visit)), "\n")
cat("  Records without match:",
    sum( is.na(data_dt_clean$days_since_visit)), "\n")
cat("  Median days since visit:",
    round(median(data_dt_clean$days_since_visit, na.rm = TRUE), 0), "\n\n")

data_model_b <- data_dt_clean %>%
  mutate(
    age_years  = as.numeric(age_years),
    age_months = as.numeric(age_months),
    age_weeks  = as.numeric(age_weeks),
    
    # Outcome 1 (primary): strict — confirmed return to MCHTrack only
    # most defensible given off-network records are verbal and unverified
    recovered_strict     = as.integer(tracing_outcome == "yes_ok"),
    
    # Outcome 2: permissive — includes unverified off-network verbal reports
    # sensitivity check only; inflated by unconfirmed vaccination status
    recovered_permissive = as.integer(tracing_outcome %in%
                                        c("yes_ok", "yes_off_network_care")),
    
    # Outcome 3: child reached — any contact regardless of service delivery
    child_reached        = as.integer(tracing_outcome %in%
                                        c("yes_ok", "yes_off_network_care",
                                          "service_not_offered")),
    
    # age at tracing in months
    age_months_tracing   = age_years * 12 + coalesce(age_months, 0),
    
    # days since last facility visit — constructed above
    # NA for children with no prior visit record in MCHTrack
    days_since_visit     = as.numeric(days_since_visit),
    
    # tracing method: sms/phone = 1, home visit = 0
    method_sms           = as.integer(
      str_detect(tolower(tracing_method), "sms|phone|call")
    ),
    
    # primary sample excludes Rimi LGA
    in_primary_sample    = !rimi_flag,
    
    # implausible age (>180m) excluded as data-entry error -- Decision Log, Appendix A
    age_implausible      = !is.na(age_months_tracing) & age_months_tracing > 180
    
  ) %>%
  filter(!is.na(tracing_outcome))

#Apply: implausible-age exclusion ----
cat("Model B: excluding", sum(data_model_b$age_implausible), "implausible-age rows\n\n")
data_model_b <- data_model_b %>% filter(!age_implausible)

#Validate: outcome distribution and sample composition ----
cat("Model B dataset:\n")
data_model_b %>%
  filter(in_primary_sample) %>%
  summarise(
    n_traced             = n(),
    strict_pct           = round(mean(recovered_strict,     na.rm = TRUE) * 100, 1),
    permissive_pct       = round(mean(recovered_permissive, na.rm = TRUE) * 100, 1),
    reached_pct          = round(mean(child_reached,        na.rm = TRUE) * 100, 1),
    pct_sms              = round(mean(method_sms,           na.rm = TRUE) * 100, 1),
    pct_with_lag_time     = round(mean(!is.na(days_since_visit), na.rm = TRUE) * 100, 1)
  ) %>%
  print()
cat("\n")

#Validate: sanity exclusions on other predictors ----
# hf_distance_km: >100km->NA already applied upstream in 01_mchtrack_import.R
# to all three source tables, so Model A and Model B share the same rule.
# days_since_visit: only capped at <=300 for display (Fig 3.5B, Table 4.1),
# not excluded from m_b1-m_b4 -- flagged here, not yet resolved.
n_lag_over_300 <- sum(data_model_b$days_since_visit > 300, na.rm = TRUE)
cat("days_since_visit > 300 days:", n_lag_over_300, "of",
    sum(!is.na(data_model_b$days_since_visit)), "-- not excluded from models\n\n")

# 10b. Model B0: TRUE full sample — no lag-time restriction ------------------
# FIX (13/7/2026): every Model B spec below includes days_since_visit, and
# feglm() does complete-case listwise deletion, so m_b1 was already silently
# restricted to the lag-time-available subset — there was no model anywhere
# in this script representing the true full in_primary_sample. This is the
# direct cause of the Table 3.1b bug: "Full sample" and "Lag-time subset"
# were identical because only one model existed. This model drops
# days_since_visit so it runs on the full in_primary_sample instead.

m_b0_full <- feglm(
  recovered_strict ~ method_sms + hf_distance_km + age_months_tracing
  | lga_name,
  data   = data_model_b %>% filter(in_primary_sample),
  family = binomial,
  vcov   = ~lga_name
)

cat("Model B0 (full sample, no lag-time restriction) — N check:\n")
cat("  in_primary_sample rows:      ",
    nrow(data_model_b %>% filter(in_primary_sample)), "\n")
cat("  m_b0_full fitted N:          ", nobs(m_b0_full), "\n")
cat("  (difference from raw N reflects any remaining missingness in\n")
cat("   method_sms / hf_distance_km / age_months_tracing / lga_name FE\n")
cat("   singleton groups, NOT the lag-time restriction)\n\n")

# 11. Model B1: strict outcome, LGA fixed effects — primary spec ---------------
# NOTE: this spec includes days_since_visit, so it is the LAG-TIME SUBSET
# spec, not the full sample — see m_b0_full above and Section 15b below.

m_b1 <- feglm(
  recovered_strict ~ method_sms + hf_distance_km + age_months_tracing +
    days_since_visit
  | lga_name,
  data   = data_model_b %>% filter(in_primary_sample),
  family = binomial,
  vcov   = ~lga_name
)

cat("Model B1 (lag-time subset) — N check:\n")
cat("  m_b1 fitted N:               ", nobs(m_b1), "\n\n")

# 12. Model B2: robustness — permissive outcome --------------------------------

m_b2 <- feglm(
  recovered_permissive ~ method_sms + hf_distance_km + age_months_tracing +
    days_since_visit
  | lga_name,
  data   = data_model_b %>% filter(in_primary_sample),
  family = binomial,
  vcov   = ~lga_name
)

# 13. Model B3: robustness — child reached outcome -----------------------------

m_b3 <- feglm(
  child_reached ~ method_sms + hf_distance_km + age_months_tracing +
    days_since_visit
  | lga_name,
  data   = data_model_b %>% filter(in_primary_sample),
  family = binomial,
  vcov   = ~lga_name
)

# 14. Model B4: robustness — state interaction on tracing method ---------------
# tests whether SMS advantage differs between Kano and Katsina

m_b4 <- feglm(
  recovered_strict ~ method_sms * state + hf_distance_km + age_months_tracing +
    days_since_visit
  | lga_name,
  data   = data_model_b %>% filter(in_primary_sample),
  family = binomial,
  vcov   = ~lga_name
)

# 15. Regression table: Model B -----------------------------------------------
# Unchanged from the original robustness table — strict/permissive/reached/
# interaction, all fit on the lag-time subset for internal comparability.

modelsummary(
  list("Strict"         = m_b1,
       "Permissive"     = m_b2,
       "Reached"        = m_b3,
       "Method × state" = m_b4),
  stars       = c("*" = 0.1, "**" = 0.05, "***" = 0.01),
  coef_rename = c(
    "method_sms"              = "SMS / phone contact",
    "hf_distance_km"          = "HF distance (km)",
    "age_months_tracing"      = "Age at tracing (months)",
    "stateKatsina"            = "Katsina",
    "method_sms:stateKatsina" = "SMS × Katsina"
  ),
  gof_map = c("nobs","r.squared","FE: lga_name"),
  output  = file.path(out_dir, "02_model_b_tracing_effectiveness.txt")
)

modelsummary(
  list("Strict" = m_b1, "Permissive" = m_b2,
       "Reached" = m_b3, "Method × state" = m_b4),
  stars = c("*" = 0.1, "**" = 0.05, "***" = 0.01)
)

# 15b. Regression table: Table 3.1b — full sample vs lag-time subset ----------
# NEW (13/7/2026): dedicated two-column table for the dissertation's Table
# 3.1b, so 09_visualization_markdown.Rmd can source both columns from real,
# distinct model objects instead of duplicating one model into both.
# method_sms and hf_distance_km are the only two predictors common to both
# specs (age_months_tracing is common too; days_since_visit only exists in
# the lag-time subset spec, so it appears as NA/blank for the full-sample
# column, which is the correct and expected behaviour).

modelsummary(
  list("Full sample"       = m_b0_full,
       "Lag-time subset"   = m_b1),
  stars       = c("*" = 0.1, "**" = 0.05, "***" = 0.01),
  coef_rename = c(
    "method_sms"              = "SMS / phone contact",
    "hf_distance_km"          = "HF distance (km)",
    "age_months_tracing"      = "Age at tracing (months)",
    "days_since_visit"        = "Days since last visit"
  ),
  gof_map = c("nobs","r.squared","FE: lga_name"),
  title   = "Table 3.1b — Recovery model, full sample vs lag-time subset",
  output  = file.path(out_dir, "03_table_3_1b_full_vs_lagtime.txt")
)

cat("Table 3.1b comparison - sanity check (should now differ):\n")
modelsummary(
  list("Full sample" = m_b0_full, "Lag-time subset" = m_b1),
  stars = c("*" = 0.1, "**" = 0.05, "***" = 0.01)
)
cat("\n")


#--------------------#Distributions & Summary Statistics#--------------------#
# Section 16, added 30/7/2026 for Appendix D ("Distributions of the main
# outcome and predictor variables"). Covers every variable that enters
# Models A and B: continuous predictors (distance, age, lag time), binary
# predictors and outcomes (gender, method, ZD flags, recovery flags), and
# state as the recurring comparison axis throughout this dissertation.
# Rainfall, heat (UTCI), and NDVI are NOT covered here - those variables
# and their distributions live in 02_chirps_import_analysis.R,
# 06_era5_analysis.R, and 08_ndvi_analysis.R respectively, and would need
# the equivalent block added to each of those scripts if their
# distributions are wanted in the same appendix.

# 16a. Helper functions ------------------------------------------------------
# Skewness and kurtosis use the standard Fisher-Pearson moment formulas.
# Outlier counts use two conventional rules side by side (Tukey's 1.5xIQR
# fence, and |z| > 3) rather than picking one, since the two can disagree
# and the disagreement itself is informative about how a variable behaves.

skewness <- function(x) {
  x <- x[!is.na(x)]
  m <- mean(x); s <- sd(x); n <- length(x)
  (sum((x - m)^3) / n) / s^3
}

kurtosis <- function(x) {
  x <- x[!is.na(x)]
  m <- mean(x); s <- sd(x); n <- length(x)
  (sum((x - m)^4) / n) / s^4 - 3
}

n_outliers_iqr <- function(x) {
  x  <- x[!is.na(x)]
  q1 <- quantile(x, 0.25); q3 <- quantile(x, 0.75); iqr <- q3 - q1
  sum(x < q1 - 1.5 * iqr | x > q3 + 1.5 * iqr)
}

n_outliers_zscore <- function(x) {
  x <- x[!is.na(x)]
  z <- abs((x - mean(x)) / sd(x))
  sum(z > 3, na.rm = TRUE)
}

describe_continuous <- function(df, var, label, group_label = "All") {
  x <- df[[var]]
  tibble(
    group             = group_label,
    variable          = label,
    n                 = sum(!is.na(x)),
    n_missing         = sum(is.na(x)),
    pct_missing       = round(mean(is.na(x)) * 100, 2),
    mean              = round(mean(x, na.rm = TRUE), 2),
    sd                = round(sd(x, na.rm = TRUE), 2),
    min               = round(min(x, na.rm = TRUE), 2),
    p25               = unname(round(quantile(x, 0.25, na.rm = TRUE), 2)),
    median            = round(median(x, na.rm = TRUE), 2),
    p75               = unname(round(quantile(x, 0.75, na.rm = TRUE), 2)),
    p95               = unname(round(quantile(x, 0.95, na.rm = TRUE), 2)),
    max               = round(max(x, na.rm = TRUE), 2),
    skewness          = round(skewness(x), 2),
    kurtosis          = round(kurtosis(x), 2),
    n_outliers_iqr    = n_outliers_iqr(x),
    n_outliers_zscore = n_outliers_zscore(x)
  )
}

describe_categorical <- function(df, var, label, group_label = "All") {
  # level is coerced to character before counting, since the binary 0/1
  # flags (integer) and state (character) would otherwise leave bind_rows()
  # unable to stack the resulting "level" columns into one column of a
  # single type.
  df %>%
    filter(!is.na(.data[[var]])) %>%
    mutate(level_chr = as.character(.data[[var]])) %>%
    count(level = level_chr) %>%
    mutate(
      group    = group_label,
      variable = label,
      pct      = round(n / sum(n) * 100, 1)
    ) %>%
    select(group, variable, level, n, pct)
}

# 16b. Model A distributions - continuous variables --------------------------
# hf_distance_km and age_months_at_reg, overall and split by state, since
# Kano/Katsina asymmetry is a running theme throughout this dissertation.

dist_a_continuous <- bind_rows(
  describe_continuous(data_model_a %>% filter(in_primary_sample),
                      "hf_distance_km", "HF distance (km)"),
  describe_continuous(data_model_a %>% filter(in_primary_sample),
                      "age_months_at_reg", "Age at registration (months)"),
  describe_continuous(data_model_a %>% filter(in_primary_sample, state == "Kano"),
                      "hf_distance_km", "HF distance (km)", "Kano"),
  describe_continuous(data_model_a %>% filter(in_primary_sample, state == "Katsina"),
                      "hf_distance_km", "HF distance (km)", "Katsina"),
  describe_continuous(data_model_a %>% filter(in_primary_sample, state == "Kano"),
                      "age_months_at_reg", "Age at registration (months)", "Kano"),
  describe_continuous(data_model_a %>% filter(in_primary_sample, state == "Katsina"),
                      "age_months_at_reg", "Age at registration (months)", "Katsina")
)

cat("Model A - continuous variable distributions:\n")
print(dist_a_continuous)
cat("\n")

# 16c. Model A distributions - categorical variables --------------------------

dist_a_categorical <- bind_rows(
  describe_categorical(data_model_a %>% filter(in_primary_sample),
                       "gender_female", "Gender (1 = female)"),
  describe_categorical(data_model_a %>% filter(in_primary_sample),
                       "zero_dose_penta", "Zero-dose, primary definition"),
  describe_categorical(data_model_a %>% filter(in_primary_sample),
                       "zero_dose_truly", "Zero-dose, truly-ZD definition"),
  describe_categorical(data_model_a %>% filter(in_primary_sample),
                       "state", "State")
)

cat("Model A - categorical variable distributions:\n")
print(dist_a_categorical)
cat("\n")

# 16d. Model B distributions - continuous variables ---------------------------
# age_months_tracing and days_since_visit. days_since_visit is reported on
# the lag-time subset only, since it is undefined outside it by construction.

dist_b_continuous <- bind_rows(
  describe_continuous(data_model_b %>% filter(in_primary_sample),
                      "age_months_tracing", "Age at tracing (months)"),
  describe_continuous(data_model_b %>% filter(in_primary_sample, !is.na(days_since_visit)),
                      "days_since_visit", "Days since last facility visit (lag-time subset)"),
  describe_continuous(data_model_b %>% filter(in_primary_sample, state == "Kano"),
                      "age_months_tracing", "Age at tracing (months)", "Kano"),
  describe_continuous(data_model_b %>% filter(in_primary_sample, state == "Katsina"),
                      "age_months_tracing", "Age at tracing (months)", "Katsina"),
  describe_continuous(data_model_b %>% filter(in_primary_sample, state == "Kano", !is.na(days_since_visit)),
                      "days_since_visit", "Days since last facility visit (lag-time subset)", "Kano"),
  describe_continuous(data_model_b %>% filter(in_primary_sample, state == "Katsina", !is.na(days_since_visit)),
                      "days_since_visit", "Days since last facility visit (lag-time subset)", "Katsina")
)

cat("Model B - continuous variable distributions:\n")
print(dist_b_continuous)
cat("\n")

# 16e. Model B distributions - categorical variables ---------------------------

dist_b_categorical <- bind_rows(
  describe_categorical(data_model_b %>% filter(in_primary_sample),
                       "method_sms", "Tracing method (1 = SMS/phone)"),
  describe_categorical(data_model_b %>% filter(in_primary_sample),
                       "recovered_strict", "Recovered, strict definition"),
  describe_categorical(data_model_b %>% filter(in_primary_sample),
                       "recovered_permissive", "Recovered, permissive definition"),
  describe_categorical(data_model_b %>% filter(in_primary_sample),
                       "child_reached", "Child reached (any contact)"),
  describe_categorical(data_model_b %>% filter(in_primary_sample),
                       "state", "State")
)

cat("Model B - categorical variable distributions:\n")
print(dist_b_categorical)
cat("\n")

# 16f. Correlation matrices ---------------------------------------------------
# Pearson and Spearman side by side for the continuous predictors and their
# outcome, useful for reviewers checking for multicollinearity concerns or
# a nonlinear relationship a linear specification would miss.

corr_a_vars <- data_model_a %>%
  filter(in_primary_sample) %>%
  select(hf_distance_km, age_months_at_reg, gender_female, zero_dose_penta) %>%
  drop_na()

corr_a_pearson  <- round(cor(corr_a_vars, method = "pearson"),  2)
corr_a_spearman <- round(cor(corr_a_vars, method = "spearman"), 2)

corr_b_vars <- data_model_b %>%
  filter(in_primary_sample) %>%
  select(hf_distance_km, age_months_tracing, method_sms, recovered_strict) %>%
  drop_na()

corr_b_pearson  <- round(cor(corr_b_vars, method = "pearson"),  2)
corr_b_spearman <- round(cor(corr_b_vars, method = "spearman"), 2)

cat("Model A - correlation matrix (Pearson):\n");  print(corr_a_pearson);  cat("\n")
cat("Model A - correlation matrix (Spearman):\n"); print(corr_a_spearman); cat("\n")
cat("Model B - correlation matrix (Pearson):\n");  print(corr_b_pearson);  cat("\n")
cat("Model B - correlation matrix (Spearman):\n"); print(corr_b_spearman); cat("\n")

# 16g. Pre-regression missingness summary -------------------------------------
# How many rows each key variable was missing BEFORE the filters already
# applied when data_model_a/data_model_b were built (Section 3 and Section
# 10 above already drop hf_distance_km == NA and tracing_outcome == NA
# respectively, so this looks upstream of that, at the raw joined tables).

missingness_summary <- bind_rows(
  data_ll_clean %>% filter(woman_or_child == "child") %>%
    summarise(
      variable    = "hf_distance_km (Model A, pre-filter)",
      n_total     = n(),
      n_missing   = sum(is.na(hf_distance_km)),
      pct_missing = round(mean(is.na(hf_distance_km)) * 100, 2)
    ),
  data_ll_clean %>% filter(woman_or_child == "child") %>%
    summarise(
      variable    = "age fields, all three (Model A, pre-filter)",
      n_total     = n(),
      n_missing   = sum(is.na(age_years) & is.na(age_months) & is.na(age_weeks)),
      pct_missing = round(mean(is.na(age_years) & is.na(age_months) & is.na(age_weeks)) * 100, 2)
    ),
  data_dt_clean %>%
    summarise(
      variable    = "tracing_outcome (Model B, pre-filter)",
      n_total     = n(),
      n_missing   = sum(is.na(tracing_outcome)),
      pct_missing = round(mean(is.na(tracing_outcome)) * 100, 2)
    ),
  data_dt_clean %>%
    summarise(
      variable    = "days_since_visit (Model B, structural - undefined outside lag-time subset)",
      n_total     = n(),
      n_missing   = sum(is.na(days_since_visit)),
      pct_missing = round(mean(is.na(days_since_visit)) * 100, 2)
    )
)

cat("Pre-regression missingness summary:\n")
print(missingness_summary)
cat("\n")

# 16h. Bundle and write TXT companion -----------------------------------------
# RDS keeps every object queryable; TXT is a single flat file Khem can
# upload directly wherever a full R session isn't available.

distributions_summary <- list(
  model_a_continuous  = dist_a_continuous,
  model_a_categorical = dist_a_categorical,
  model_b_continuous  = dist_b_continuous,
  model_b_categorical = dist_b_categorical,
  corr_a_pearson      = corr_a_pearson,
  corr_a_spearman     = corr_a_spearman,
  corr_b_pearson      = corr_b_pearson,
  corr_b_spearman     = corr_b_spearman,
  missingness_summary = missingness_summary
)

txt_path <- file.path(out_dir, "04_distributions_summary.txt")
sink(txt_path)
cat("DISTRIBUTIONS AND SUMMARY STATISTICS - MODELS A AND B\n")
cat("Generated from 03_regression.R, Section 16\n")
cat(paste0("Run date: ", Sys.Date(), "\n"))
cat(strrep("=", 70), "\n\n")

cat("MODEL A - CONTINUOUS VARIABLES (overall and by state)\n")
cat(strrep("-", 70), "\n")
print(as.data.frame(dist_a_continuous)); cat("\n\n")

cat("MODEL A - CATEGORICAL VARIABLES\n")
cat(strrep("-", 70), "\n")
print(as.data.frame(dist_a_categorical)); cat("\n\n")

cat("MODEL B - CONTINUOUS VARIABLES (overall and by state)\n")
cat(strrep("-", 70), "\n")
print(as.data.frame(dist_b_continuous)); cat("\n\n")

cat("MODEL B - CATEGORICAL VARIABLES\n")
cat(strrep("-", 70), "\n")
print(as.data.frame(dist_b_categorical)); cat("\n\n")

cat("MODEL A - CORRELATION MATRIX (PEARSON)\n")
cat(strrep("-", 70), "\n")
print(corr_a_pearson); cat("\n\n")

cat("MODEL A - CORRELATION MATRIX (SPEARMAN)\n")
cat(strrep("-", 70), "\n")
print(corr_a_spearman); cat("\n\n")

cat("MODEL B - CORRELATION MATRIX (PEARSON)\n")
cat(strrep("-", 70), "\n")
print(corr_b_pearson); cat("\n\n")

cat("MODEL B - CORRELATION MATRIX (SPEARMAN)\n")
cat(strrep("-", 70), "\n")
print(corr_b_spearman); cat("\n\n")

cat("PRE-REGRESSION MISSINGNESS SUMMARY\n")
cat(strrep("-", 70), "\n")
print(as.data.frame(missingness_summary)); cat("\n\n")

cat(strrep("=", 70), "\n")
cat("NOTE: rainfall, heat (UTCI), and NDVI distributions are NOT included\n")
cat("here. Those variables live in 02_chirps_import_analysis.R,\n")
cat("06_era5_analysis.R, and 08_ndvi_analysis.R respectively, and would\n")
cat("need the equivalent Section 16 block added to each script if their\n")
cat("distributions are wanted in the same appendix.\n")
sink()

cat("Distributions summary written to:\n")
cat(" ", txt_path, "\n\n")


##########
# Export #
##########

#Save analysis datasets
saveRDS(data_model_a,
        file.path(out_dir, "03_model_a_dataset.rds"))
saveRDS(data_model_b,
        file.path(out_dir, "03_model_b_dataset.rds"))
saveRDS(data_ward_residuals,
        file.path(out_dir, "03_ward_residuals_model_a.rds"))
saveRDS(distributions_summary,
        file.path(out_dir, "04_distributions_summary.rds"))

cat("All outputs saved to:", out_dir, "\n")
cat("  01_model_a_zerodose_predictors.txt\n")
cat("  02_model_b_tracing_effectiveness.txt\n")
cat("  03_table_3_1b_full_vs_lagtime.txt      <- fixes Table 3.1b duplication\n")
cat("  03_model_a_dataset.rds\n")
cat("  03_model_b_dataset.rds\n")
cat("  03_ward_residuals_model_a.rds\n")
cat("  04_distributions_summary.rds           <- NEW, Section 16, for Appendix D\n")
cat("  04_distributions_summary.txt           <- NEW, same content, flat text file\n")
cat("                                             (easiest format to upload for review)\n")

#--------------------------(END)------------------------------#