########################################################
#  NDVI Agricultural Calendar — Regression Analysis    #
#  Outcome: Daily / Monthly facility visit counts      #
#  Exposure: vim_c (NDVI), viq_c (anomaly percentile)  #
#  States: Kano (primary) + Katsina (sensitivity)      #
#  Created: June 2026                                  #
#  Last Updated: 03/7/2026                             #
#                                                      #
#  HYPOTHESIS                                          #
#  Peak agricultural greenness (high NDVI) proxies     #
#  peak farming season — caregivers face maximum       #
#  opportunity cost and time poverty during Jul-Oct,   #
#  reducing facility visits independently of climate   #
#                                                      #
#  KEY LESSONS APPLIED FROM ERA5 ANALYSIS              #
#  1. Eid days excluded (institutional closure)        #
#  2. Ramadan indicator added as explicit control      #
#  3. Weekend days treated carefully (mostly zeros)    #
#  4. Month-year FE absorbs seasonality — identifying  #
#     variation is within-month NDVI anomaly (viq_c)   #
#     rather than raw seasonal pattern                 #
#  5. Two-cluster problem: Kano only = 2 LGAs;         #
#     use robust SE, not clustered SE                  #
#  6. Katsina results flagged as sensitivity only      #
########################################################

# Reset environment -----------------------------------------------------

rm(list = ls())
setwd("C:/Users/HP/Documents/GitHub/datharm-placement")
options(scipen = 999)
Sys.setlocale("LC_TIME", "English")

home         <- "C:/Users/HP/Documents/GitHub/datharm-placement"
mchtrack_dir <- file.path(home, "03_output/01_mchtrack_data")
ndvi_dir     <- file.path(home, "03_output/07_ndvi")
out_dir      <- file.path(home, "03_output/08_ndvi_analysis")
dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)

window_start <- as.Date("2024-08-01")
window_end   <- as.Date("2026-03-31")

library(janitor)
library(tidyverse)
library(lubridate)
library(fixest)
library(modelsummary)
library(patchwork)
library(broom)

#----------------------------------------------------------------------------

###################################
# 1. Load NDVI panels             #
###################################

ndvi_monthly  <- readRDS(file.path(ndvi_dir, "07_ndvi_monthly.rds"))
ndvi_dekadal  <- readRDS(file.path(ndvi_dir, "07_ndvi_dekadal.rds"))

cat("NDVI monthly rows:", nrow(ndvi_monthly), "\n")
cat("NDVI dekadal rows:", nrow(ndvi_dekadal), "\n")
cat("States:", unique(ndvi_monthly$state), "\n")
cat("LGAs:", n_distinct(ndvi_monthly$lga_name), "\n\n")

#----------------------------------------------------------------------------

###################################
# 2. Load MCHTrack facility visits#
###################################

# --- 2a. KANO ---
fv_kano_raw <- readRDS(
  file.path(mchtrack_dir, "01_facility_visits_clean.rds")
) %>%
  filter(
    str_detect(tolower(lga_name), "ungogo|gabasawa"),
    woman_or_child == "child",
    !rimi_flag,
    !is.na(visit_date)
  ) %>%
  mutate(
    visit_date = as.Date(visit_date),
    lga_clean  = str_to_title(str_trim(
      str_remove(lga_name, regex("\\s*lga\\s*$", ignore_case = TRUE))
    ))
  ) %>%
  filter(visit_date >= window_start, visit_date <= window_end)

cat("Kano visits loaded:", nrow(fv_kano_raw), "\n")
cat("Kano LGAs:", unique(fv_kano_raw$lga_clean), "\n")

# --- 2b. KATSINA (sensitivity) ---
# Linelisted table is less affected by duplication than facility visits
# Use it to construct a proxy for enrolment-based visit activity
# NOTE: treat all Katsina results as sensitivity — flag in write-up

fv_kat_raw <- readRDS(
  file.path(mchtrack_dir, "01_linelisted_clean.rds")
) %>%
  filter(
    !rimi_flag,
    !is.na(lga_name)
  ) %>%
  mutate(
    # Linelisted has enrolment date not visit date — use as monthly proxy
    enrol_date = as.Date(registration_date),
    lga_clean  = str_to_title(str_trim(
      str_remove(lga_name, regex("\\s*lga\\s*$", ignore_case = TRUE))
    ))
  ) %>%
  filter(enrol_date >= window_start, enrol_date <= window_end)

cat("\nKatsina linelisted rows loaded:", nrow(fv_kat_raw), "\n")
cat("Katsina LGAs:", n_distinct(fv_kat_raw$lga_clean), "\n\n")

fv <- readRDS(file.path(mchtrack_dir, "01_facility_visits_clean.rds")) %>%
  filter(str_detect(tolower(lga_name), "ungogo|gabasawa"),
         woman_or_child == "child",
         !rimi_flag)

# What visit types are recorded?
cat("Track values:\n")
print(table(fv$track, useNA = "always"))

cat("\nVaccines administered (sample):\n")
print(table(fv$vaccines_administered, useNA = "always") %>% sort(decreasing = TRUE) %>% head(20))

fv %>%
  mutate(has_vaccine = !is.na(vaccines_administered) &
           vaccines_administered != "") %>%
  count(track, has_vaccine) %>%
  filter(str_detect(track, "immun")) %>%
  print()

#----------------------------------------------------------------------------

###################################
# 3. Build KANO daily panel       #
###################################

# Aggregate Kano visits to LGA x day
kano_daily <- fv_kano_raw %>%
  group_by(lga_clean, visit_date) %>%
  summarise(n_visits = n(), .groups = "drop")

# Full date spine — ensures zero-visit days are explicit
kano_spine <- expand_grid(
  lga_clean  = c("Ungogo", "Gabasawa"),
  visit_date = seq(window_start, window_end, by = "day")
)

kano_daily <- kano_spine %>%
  left_join(kano_daily, by = c("lga_clean", "visit_date")) %>%
  mutate(
    n_visits   = replace_na(n_visits, 0),
    year_month = floor_date(visit_date, "month"),
    month_num  = month(visit_date),
    year       = year(visit_date),
    dow_num    = wday(visit_date),
    dow        = wday(visit_date, label = TRUE, abbr = TRUE),
    weekend    = as.integer(dow_num %in% c(1, 7)),   # Sun=1, Sat=7
    friday     = as.integer(dow_num == 6),
    # Islamic calendar controls — identified from diagnostic
    eid_day    = visit_date %in% as.Date(c(
      "2025-03-30", "2025-03-31", "2025-04-01",  # Eid al-Fitr
      "2025-06-06", "2025-06-07", "2025-06-08"   # Eid al-Adha
    )),
    ramadan_25 = visit_date >= as.Date("2025-03-01") &
      visit_date <= as.Date("2025-03-29"),
    ym_factor  = as.factor(format(visit_date, "%Y-%m")),
    log_visits = log(n_visits + 1)
  )

cat("Kano daily panel rows:", nrow(kano_daily), "\n")
cat("Eid days flagged:", sum(kano_daily$eid_day), "\n")
cat("Ramadan days flagged:", sum(kano_daily$ramadan_25), "\n\n")

#----------------------------------------------------------------------------

###################################
# 4. Build KANO monthly panel     #
###################################

# Monthly aggregation for NDVI merge (NDVI is dekadal/monthly)
kano_monthly <- kano_daily %>%
  filter(!eid_day) %>%               # exclude Eid before aggregating
  group_by(lga_clean, year_month, month_num, year, ym_factor) %>%
  summarise(
    n_visits_total   = sum(n_visits),
    n_visits_weekday = sum(n_visits[!weekend & !friday]),
    n_active_days    = sum(n_visits > 0),
    n_weekdays       = sum(!weekend & !friday),
    mean_daily_visits = round(mean(n_visits[!weekend & !friday]), 2),
    ramadan_month    = any(ramadan_25),
    .groups = "drop"
  ) %>%
  mutate(
    log_visits_total   = log(n_visits_total + 1),
    log_visits_weekday = log(n_visits_weekday + 1)
  )

cat("Kano monthly panel rows:", nrow(kano_monthly), "\n\n")

#----------------------------------------------------------------------------

###################################
# 5. Build KATSINA monthly panel  #
###################################

kat_monthly <- fv_kat_raw %>%
  mutate(year_month = floor_date(enrol_date, "month")) %>%
  group_by(lga_clean, year_month) %>%
  summarise(
    n_enrolments = n(),
    .groups = "drop"
  ) %>%
  mutate(
    month_num  = month(year_month),
    year       = year(year_month),
    ym_factor  = as.factor(format(year_month, "%Y-%m")),
    log_enrol  = log(n_enrolments + 1),
    # Ramadan indicator
    ramadan_25 = year_month == as.Date("2025-03-01")
  )

cat("Katsina monthly panel rows:", nrow(kat_monthly), "\n\n")

#----------------------------------------------------------------------------

###################################
# 6. Merge NDVI onto panels       #
###################################

# --- 6a. Kano monthly merge ---
ndvi_kano_monthly <- ndvi_monthly %>%
  filter(state == "Kano") %>%
  select(lga_name, year_month, vim_monthly, viq_monthly,
         vim_c, viq_c, vim_above_median, ag_season)

kano_panel_m <- kano_monthly %>%
  left_join(ndvi_kano_monthly,
            by = c("lga_clean" = "lga_name", "year_month")) %>%
  filter(!is.na(vim_monthly))

cat("Kano monthly panel after NDVI merge:", nrow(kano_panel_m), "\n")
cat("Missing NDVI months:", sum(is.na(kano_panel_m$vim_monthly)), "\n")

# --- 6b. Kano daily merge ---
# Merge dekadal NDVI onto daily panel by matching dekad
# Dekad 1 = days 1-10, Dekad 2 = days 11-20, Dekad 3 = days 21-31
ndvi_kano_dek <- ndvi_dekadal %>%
  filter(state == "Kano") %>%
  select(lga_name, date, vim, viq, vim_c, viq_c,
         vim_above_median, ag_season, dekad_num)

# Create dekad-start date for matching
ndvi_kano_dek <- ndvi_kano_dek %>%
  rename(dekad_date = date)

kano_daily_dek <- kano_daily %>%
  mutate(
    dekad_num  = case_when(
      day(visit_date) <= 10 ~ 1L,
      day(visit_date) <= 20 ~ 2L,
      TRUE                  ~ 3L
    ),
    dekad_date = as.Date(paste0(
      year(visit_date), "-",
      sprintf("%02d", month(visit_date)), "-",
      case_when(dekad_num == 1 ~ "01",
                dekad_num == 2 ~ "11",
                TRUE           ~ "21")
    ))
  ) %>%
  left_join(
    ndvi_kano_dek,
    by = c("lga_clean" = "lga_name", "dekad_date")
  )

cat("\nKano daily-dekadal panel rows:", nrow(kano_daily_dek), "\n")
cat("Matched NDVI:", sum(!is.na(kano_daily_dek$vim)), "\n")

# --- 6c. Katsina monthly merge ---
ndvi_kat_monthly <- ndvi_monthly %>%
  filter(state == "Katsina") %>%
  select(lga_name, year_month, vim_monthly, viq_monthly,
         vim_c, viq_c, vim_above_median, ag_season)

kat_panel_m <- kat_monthly %>%
  left_join(ndvi_kat_monthly,
            by = c("lga_clean" = "lga_name", "year_month")) %>%
  filter(!is.na(vim_monthly))

cat("Katsina monthly panel after NDVI merge:", nrow(kat_panel_m), "\n\n")

#----------------------------------------------------------------------------

###################################
# 7. Descriptive checks           #
###################################

cat("=== DESCRIPTIVE CHECKS ===\n\n")

# Mean visits by agricultural season — the key visual diagnostic
cat("--- Kano: mean weekday visits by agricultural season ---\n")
kano_panel_m %>%
  group_by(ag_season) %>%
  summarise(
    mean_visits = round(mean(mean_daily_visits, na.rm = TRUE), 1),
    sd_visits   = round(sd(mean_daily_visits, na.rm = TRUE), 1),
    n_months    = n(),
    mean_vim    = round(mean(vim_monthly), 3),
    .groups = "drop"
  ) %>%
  arrange(ag_season) %>%
  print()

# Split seasonal pattern by LGA
kano_panel_m %>%
  group_by(lga_clean, ag_season) %>%
  summarise(
    mean_visits = round(mean(mean_daily_visits, na.rm = TRUE), 1),
    mean_vim    = round(mean(vim_monthly), 3),
    n           = n(),
    .groups     = "drop"
  ) %>%
  arrange(lga_clean, ag_season) %>%
  print()

# Correlation of vim with visits — unadjusted
cat("\n--- Kano: Pearson correlation vim vs mean_daily_visits ---\n")
for (lga in c("Ungogo", "Gabasawa")) {
  sub <- kano_panel_m %>% filter(lga_clean == lga)
  r   <- cor(sub$vim_monthly, sub$mean_daily_visits, use = "complete.obs")
  cat(lga, ": r =", round(r, 3), "\n")
}

#----------------------------------------------------------------------------

###################################
# 8. KANO monthly regressions     #
###################################

# NOTE ON IDENTIFICATION STRATEGY
# Month-year FE absorbs the raw seasonal pattern (including the
# agricultural calendar itself as a seasonal phenomenon).
# The identifying variation after month-year FE is the WITHIN-MONTH
# deviation of NDVI from its seasonal norm — captured by viq_c
# (the anomaly percentile relative to 5-year baseline).
# vim_c captures the overall level including seasonal variation —
# better used WITHOUT month-year FE, or with only LGA FE.
# Both are run below for comparison.

cat("\n=== KANO MONTHLY REGRESSIONS ===\n\n")

# N1: vim + LGA FE only
n1 <- feols(log_visits_weekday ~ vim_c | lga_clean,
            data = kano_panel_m, vcov = "hetero")

# N2: vim + LGA FE + month-year FE (primary)
n2 <- feols(log_visits_weekday ~ vim_c | lga_clean + ym_factor,
            data = kano_panel_m, vcov = "hetero")

# EDIT 1: LGA-specific NDVI effect via interaction — avoids singleton
# FE problem that arises when filtering to one LGA with month-year FE
# (single LGA makes each LGA-month a singleton, absorbing all variation)
n2_int <- feols(log_visits_weekday ~ vim_c : lga_clean | lga_clean + ym_factor,
                data = kano_panel_m, vcov = "hetero")

cat("--- LGA-specific NDVI effects (interaction model) ---\n")
etable(n2_int, digits = 3, se.below = TRUE)

# N3: viq anomaly — within-baseline deviation
n3 <- feols(log_visits_weekday ~ viq_c | lga_clean + ym_factor,
            data = kano_panel_m, vcov = "hetero")

# EDIT 2: define n5 and n6 before first use in etable/modelsummary
# n5: binary peak agricultural season indicator
n5 <- feols(log_visits_weekday ~ vim_above_median | lga_clean,
            data = kano_panel_m, vcov = "hetero")

# n6: Poisson count model
n6 <- feglm(n_visits_weekday ~ vim_c | lga_clean + ym_factor,
            data = kano_panel_m, family = poisson())

# N4: Poisson (same as n6 for table ordering — kept for clarity)
n4 <- feglm(n_visits_weekday ~ vim_c | lga_clean + ym_factor,
            data = kano_panel_m, family = poisson())

# EDIT 3: single etable call covering all six models (removed earlier
# partial etable that referenced only n1-n4 before n5/n6 were defined)
cat("--- Kano monthly results ---\n")
etable(n1, n2, n3, n4, n5, n6,
       title    = "NDVI and facility visits — Kano monthly panel",
       digits   = 3,
       se.below = TRUE)

# EDIT 4: column labels updated to match actual model objects
modelsummary(
  list("N1: vim, LGA FE"       = n1,
       "N2: vim, +Month-yr FE" = n2,
       "N3: viq anomaly"       = n3,
       "N4: Poisson"           = n4,
       "N5: Binary peak ag"    = n5,
       "N6: LGA interaction"   = n2_int),
  stars   = c("*" = 0.1, "**" = 0.05, "***" = 0.01),
  gof_map = c("nobs", "r.squared", "adj.r.squared"),
  title   = "NDVI agricultural calendar and facility visits — Kano",
  output  = file.path(out_dir, "08_regression_kano_monthly.docx")
)

cat("\nTable saved to:", file.path(out_dir, "08_regression_kano_monthly.docx"), "\n")

#----------------------------------------------------------------------------

###################################
# 9. KANO daily regressions       #
###################################

# Daily panel provides more power than monthly
# Key advantage: within-month day-level variation in NDVI
# (carried from the dekad the day falls in)
# Excludes: Eid days, weekends
# Controls: dow_num FE, month-year FE, Ramadan
# NOTE: vim_c in D2 showed implausible coefficient (3.551, SE 2.311)
# in prior run — likely near-collinearity when month-year FE absorbs
# most within-dekad variation. Retained for transparency; interpret
# D3 (viq anomaly) and D4 (binary) as more reliable daily specs.

kano_panel_d <- kano_daily_dek %>%
  filter(!eid_day, !weekend, !friday, !is.na(vim))

cat("\n=== KANO DAILY REGRESSIONS ===\n")
cat("Panel rows (weekdays, no Eid):", nrow(kano_panel_d), "\n\n")

# D1: vim + LGA FE + DOW FE
d1 <- feols(log_visits ~ vim_c | lga_clean + dow_num,
            data  = kano_panel_d,
            vcov  = "hetero")

# D2: vim + LGA FE + DOW FE + month-year FE (primary)
# Caution: within-month NDVI variation from dekad is narrow after
# month-year FE absorption — coefficient may be poorly identified
d2 <- feols(log_visits ~ vim_c + ramadan_25 |
              lga_clean + dow_num + ym_factor,
            data  = kano_panel_d,
            vcov  = "hetero")

# D3: viq anomaly — within-baseline deviation
d3 <- feols(log_visits ~ viq_c + ramadan_25 |
              lga_clean + dow_num + ym_factor,
            data  = kano_panel_d,
            vcov  = "hetero")

# D4: binary peak ag season
d4 <- feols(log_visits ~ vim_above_median + ramadan_25 |
              lga_clean + dow_num + ym_factor,
            data  = kano_panel_d,
            vcov  = "hetero")

cat("--- Kano daily results ---\n")
etable(d1, d2, d3, d4,
       title    = "NDVI and facility visits — Kano daily panel",
       digits   = 3,
       se.below = TRUE)

modelsummary(
  list("D1: vim+DOW"        = d1,
       "D2: +Month-yr FE"   = d2,
       "D3: viq anomaly"    = d3,
       "D4: Binary peak"    = d4),
  stars   = c("*" = 0.1, "**" = 0.05, "***" = 0.01),
  gof_map = c("nobs", "r.squared", "adj.r.squared"),
  title   = "NDVI and facility visits — Kano daily panel",
  output  = file.path(out_dir, "08_regression_kano_daily.docx")
)

#----------------------------------------------------------------------------

###################################
# 10. KATSINA sensitivity         #
###################################

# Uses monthly enrolment counts (linelisted) not facility visits
# NDVI is unaffected by MCHTrack duplication issues
# Treat all results as exploratory sensitivity analysis

cat("\n=== KATSINA SENSITIVITY ANALYSIS ===\n")
cat("NOTE: Katsina facility visits data has known duplication issues\n")
cat("Using linelisted enrolment counts as proxy outcome\n\n")

# K1: vim + LGA FE (many LGAs — clustering viable here)
k1 <- feols(log_enrol ~ vim_c | lga_clean,
            data    = kat_panel_m,
            cluster = ~lga_clean)

# K2: vim + LGA FE + month-year FE + Ramadan
k2 <- feols(log_enrol ~ vim_c + ramadan_25 |
              lga_clean + ym_factor,
            data    = kat_panel_m,
            cluster = ~lga_clean)

# K3: viq anomaly
k3 <- feols(log_enrol ~ viq_c + ramadan_25 |
              lga_clean + ym_factor,
            data    = kat_panel_m,
            cluster = ~lga_clean)

cat("--- Katsina sensitivity results ---\n")
etable(k1, k2, k3,
       title    = "NDVI and enrolments — Katsina (sensitivity)",
       digits   = 3,
       se.below = TRUE)

modelsummary(
  list("K1: vim LGA FE"    = k1,
       "K2: +Month-yr FE"  = k2,
       "K3: viq anomaly"   = k3),
  stars   = c("*" = 0.1, "**" = 0.05, "***" = 0.01),
  gof_map = c("nobs", "r.squared", "adj.r.squared"),
  title   = "NDVI and enrolments — Katsina sensitivity",
  output  = file.path(out_dir, "08_regression_katsina.docx")
)

# EDIT 7: renamed second k2 estimate to k2b to avoid silent overwrite
# K2b: vim + LGA FE + month-year FE without Ramadan control
# (robustness check — Ramadan absorbed by month-year FE anyway at monthly res.)
k2b <- feols(log_enrol ~ vim_c | lga_clean + ym_factor,
             data    = kat_panel_m,
             cluster = ~lga_clean)

cat("\n--- K2b robustness (no Ramadan control) ---\n")
etable(k2b, digits = 3, se.below = TRUE)

# Kano dekadal aggregation — 3 obs per LGA per month
d2_dek <- feols(log_visits ~ vim_c | lga_clean + ym_factor,
                data  = kano_daily_dek %>%
                  filter(!eid_day, !weekend, !friday, !is.na(vim)) %>%
                  group_by(lga_clean, dekad_date, ym_factor) %>%
                  summarise(log_visits = log(sum(n_visits)+1),
                            vim_c = first(vim_c), .groups="drop"),
                vcov = "hetero")

cat("\n--- Kano dekadal aggregation ---\n")
etable(d2_dek, digits = 3, se.below = TRUE)

# EDIT 6: removed duplicate etable(k1, k2, k3) call that appeared
# after the k2b block in the original — now uses k2b for the final
# three-way Katsina comparison
cat("\n--- Final Katsina comparison (K1 / K2 / K3) ---\n")
etable(k1, k2, k3, digits = 3, se.below = TRUE)

#----------------------------------------------------------------------------

###################################
# 11. Visualisations              #
###################################

pal <- c("Ungogo" = "#D84A38", "Gabasawa" = "#1D6FA4")

# -- Plot 1: Monthly vim vs visits scatter per LGA --------------------

p1 <- ggplot(kano_panel_m,
             aes(x = vim_monthly, y = mean_daily_visits,
                 colour = lga_clean)) +
  geom_point(size = 3, alpha = 0.75) +
  geom_smooth(method = "lm", se = TRUE, linewidth = 0.9) +
  scale_colour_manual(values = pal) +
  labs(
    title   = "Monthly NDVI vs mean daily visits — Kano",
    x       = "Monthly mean NDVI (vim)",
    y       = "Mean weekday visits per day",
    colour  = NULL,
    caption = "Each point = one LGA-month · Unadjusted relationship"
  ) +
  theme_minimal(base_size = 12) +
  theme(plot.title = element_text(face = "bold"),
        legend.position = "top")

# -- Plot 2: vim time series vs monthly visits ------------------------

ts_compare <- kano_panel_m %>%
  select(lga_clean, year_month, vim_monthly, mean_daily_visits) %>%
  pivot_longer(cols = c(vim_monthly, mean_daily_visits),
               names_to = "variable", values_to = "value") %>%
  mutate(variable = recode(variable,
                           "vim_monthly"       = "NDVI (vim)",
                           "mean_daily_visits" = "Mean daily visits"
  ))

p2 <- ggplot(ts_compare %>%
               filter(lga_clean == "Ungogo"),
             aes(x = year_month, y = value, colour = variable)) +
  geom_line(linewidth = 1) +
  geom_point(size = 2) +
  scale_x_date(date_breaks = "2 months", date_labels = "%b %Y") +
  scale_colour_manual(values = c(
    "NDVI (vim)"         = "#1D9E75",
    "Mean daily visits"  = "#D84A38"
  )) +
  facet_wrap(~variable, scales = "free_y", ncol = 1) +
  labs(
    title    = "NDVI and visit trends — Ungogo",
    subtitle = "Aligned time series to reveal co-movement or divergence",
    x        = NULL, y = NULL, colour = NULL
  ) +
  theme_minimal(base_size = 12) +
  theme(plot.title      = element_text(face = "bold"),
        legend.position = "none",
        axis.text.x     = element_text(angle = 30, hjust = 1))

# -- Plot 3: Mean visits by agricultural season -----------------------

season_visits <- kano_panel_m %>%
  group_by(lga_clean, ag_season) %>%
  summarise(
    mean_v = mean(mean_daily_visits, na.rm = TRUE),
    se_v   = sd(mean_daily_visits, na.rm = TRUE) / sqrt(n()),
    .groups = "drop"
  )

p3 <- ggplot(season_visits,
             aes(x = ag_season, y = mean_v, fill = lga_clean)) +
  geom_col(position = "dodge", alpha = 0.85, width = 0.65) +
  geom_errorbar(aes(ymin = mean_v - se_v, ymax = mean_v + se_v),
                position = position_dodge(0.65),
                width = 0.2, linewidth = 0.7) +
  scale_fill_manual(values = pal) +
  labs(
    title    = "Mean daily visits by agricultural season — Kano",
    subtitle = "If opportunity cost drives visits, peak growing should be lowest",
    x        = NULL, y = "Mean weekday visits per day",
    fill     = NULL,
    caption  = "Error bars = SE · Unadjusted for other covariates"
  ) +
  theme_minimal(base_size = 12) +
  theme(plot.title      = element_text(face = "bold"),
        legend.position = "top",
        axis.text.x     = element_text(angle = 30, hjust = 1))

# -- Plot 4: Coefficient plot — monthly specs -------------------------
# EDIT 5: removed feglm (n4/n6) from tidy() calls — broom::tidy
# handles feglm output differently and can produce malformed plots.
# Coefficient plot uses feols models only (n1, n2, n3, n5).

coef_monthly <- bind_rows(
  tidy(n1) %>% mutate(model = "N1: vim, LGA FE"),
  tidy(n2) %>% mutate(model = "N2: vim, +Month-yr FE"),
  tidy(n3) %>% mutate(model = "N3: viq anomaly"),
  tidy(n5) %>% mutate(model = "N5: Binary peak ag")
) %>%
  filter(str_detect(term, "vim|viq")) %>%
  mutate(model = fct_inorder(model))

p4 <- ggplot(coef_monthly,
             aes(x = estimate, y = model)) +
  geom_vline(xintercept = 0, linetype = "dashed",
             colour = "#888780", linewidth = 0.7) +
  geom_errorbarh(aes(xmin = estimate - std.error,
                     xmax = estimate + std.error),
                 height = 0.2, linewidth = 0.8, colour = "#888780") +
  geom_point(size = 4,
             colour = ifelse(coef_monthly$estimate < 0,
                             "#D84A38", "#1D6FA4")) +
  labs(
    title   = "NDVI coefficient — monthly specifications",
    subtitle = "Kano · Robust SE · Horizontal bars = ±1 SE",
    x       = "Coefficient on log(visits)",
    y       = NULL
  ) +
  theme_minimal(base_size = 12) +
  theme(plot.title = element_text(face = "bold"))

# -- Combine ----------------------------------------------------------

combined <- (p1 + p3) / (p2 + p4) +
  plot_annotation(
    title    = "NDVI agricultural calendar and facility visits — Kano",
    subtitle = "Programme window Aug 2024 – Mar 2026",
    theme    = theme(
      plot.title    = element_text(face = "bold", size = 14),
      plot.subtitle = element_text(size = 11)
    )
  )

ggsave(
  filename = file.path(out_dir, "08_ndvi_analysis.png"),
  plot     = combined,
  width    = 14, height = 11, dpi = 300
)

cat("\nPlots saved to:", out_dir, "\n")

#----------------------------------------------------------------------------

###################################
# 12. Save panels                 #
###################################

saveRDS(kano_panel_m, file.path(out_dir, "08_kano_panel_monthly.rds"))
saveRDS(kano_panel_d, file.path(out_dir, "08_kano_panel_daily.rds"))
saveRDS(kat_panel_m,  file.path(out_dir, "08_katsina_panel_monthly.rds"))

write_csv(kano_panel_m, file.path(out_dir, "08_kano_panel_monthly.csv"))
write_csv(kat_panel_m,  file.path(out_dir, "08_katsina_panel_monthly.csv"))

cat("Panels saved.\n")
cat("\n--- Script complete ---\n")

#--------------------------(END)------------------------------#