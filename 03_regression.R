########################################################
#  Child-Level Regression Analysis                     #
#  ZD Predictors & Tracing Effectiveness               #
#  Created on 28/5/2026                                #
#  Last Updated 28/5/2026                              #
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
    in_primary_sample  = !rimi_flag
    
  ) %>%
  filter(!is.na(hf_distance_km))

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
    in_primary_sample    = !rimi_flag
    
  ) %>%
  filter(!is.na(tracing_outcome))

#Validate: outcome distribution and sample composition ----
cat("Model B dataset:\n")
data_model_b %>%
  filter(in_primary_sample) %>%
  summarise(
    n_traced             = n(),
    strict_pct           = round(mean(recovered_strict,     na.rm = TRUE) * 100, 1),
    permissive_pct       = round(mean(recovered_permissive, na.rm = TRUE) * 100, 1),
    reached_pct          = round(mean(child_reached,        na.rm = TRUE) * 100, 1),
    pct_sms              = round(mean(method_sms,           na.rm = TRUE) * 100, 1)
  ) %>%
  print()
cat("\n")

# 11. Model B1: strict outcome, LGA fixed effects — primary spec ---------------

m_b1 <- feglm(
  recovered_strict ~ method_sms + hf_distance_km + age_months_tracing +
    days_since_visit
  | lga_name,
  data   = data_model_b %>% filter(in_primary_sample),
  family = binomial,
  vcov   = ~lga_name
)

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

cat("All outputs saved to:", out_dir, "\n")
cat("  01_model_a_zerodose_predictors.txt\n")
cat("  02_model_b_tracing_effectiveness.txt\n")
cat("  03_model_a_dataset.rds\n")
cat("  03_model_b_dataset.rds\n")
cat("  03_ward_residuals_model_a.rds\n")

#--------------------------(END)------------------------------#