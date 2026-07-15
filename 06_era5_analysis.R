########################################
#  06_era5_analysis.R                  #
#  Created: June 2026                  #
#  Updated: 13/7/2026                  #
########################################

# Reset environment -----------------------------------------------------

rm(list = ls())
setwd("C:/Users/HP/Documents/GitHub/datharm-placement")
options(scipen = 999)
Sys.setlocale("LC_TIME", "English")

home         <- "C:/Users/HP/Documents/GitHub/datharm-placement"
mchtrack_dir <- file.path(home, "03_output/01_mchtrack_data")
era5_dir     <- file.path(home, "03_output/05_era5")
out_dir      <- file.path(home, "03_output/06_era5_analysis")
dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)

library(janitor)
library(tidyverse)
library(lubridate)
library(fixest)
library(modelsummary)
library(patchwork)

#----------------------------------------------------------------------------

###################################
# 1. Load MCHTrack facility visits#
###################################

data_fv <- readRDS(file.path(mchtrack_dir, "01_facility_visits_clean.rds"))

cat("Facility visits loaded:", nrow(data_fv), "rows\n")

# Filter to Ungogo and Gabasawa, children only, Rimi excluded
data_fv_kano <- data_fv %>%
  filter(
    str_detect(tolower(lga_name), "ungogo|gabasawa"),
    woman_or_child == "child",
    !rimi_flag
  ) %>%
  mutate(
    visit_date = as.Date(visit_date),
    lga_clean  = str_to_title(str_trim(
      str_remove(lga_name, regex("\\s*lga\\s*$", ignore_case = TRUE))
    ))
  ) %>%
  filter(
    visit_date >= as.Date("2024-08-01"),
    visit_date <= as.Date("2026-03-31")
  )

cat("Kano visits after filter:", nrow(data_fv_kano), "\n")
cat("LGA names:", unique(data_fv_kano$lga_clean), "\n\n")

#----------------------------------------------------------------------------

###################################
# 2. Build daily outcome panel    #
###################################

# Aggregate to LGA x day
visit_daily <- data_fv_kano %>%
  group_by(lga_clean, visit_date) %>%
  summarise(
    n_visits          = n(),
    n_unique_children = n_distinct(patient_id),
    .groups = "drop"
  )

# Build full date spine so zero-visit days are not dropped
date_spine <- expand_grid(
  lga_clean  = unique(visit_daily$lga_clean),
  visit_date = seq(as.Date("2024-08-01"),
                   as.Date("2026-03-31"),
                   by = "day")
)

visit_daily <- date_spine %>%
  left_join(visit_daily, by = c("lga_clean", "visit_date")) %>%
  mutate(n_visits = replace_na(n_visits, 0))

cat("Daily outcome panel rows:", nrow(visit_daily), "\n")
cat("Zero-visit days:", sum(visit_daily$n_visits == 0), "\n\n")

#----------------------------------------------------------------------------

###################################
# 3. Load daily UTCI series       #
###################################

utci_daily <- readRDS(file.path(era5_dir, "05_era5_utci_daily.rds")) %>%
  mutate(
    lga_clean = str_to_title(str_trim(lga))
  ) %>%
  filter(
    date >= as.Date("2024-08-01"),
    date <= as.Date("2026-03-31")
  )

cat("Daily UTCI loaded:", nrow(utci_daily), "rows\n")
cat("LGAs in UTCI:", unique(utci_daily$lga_clean), "\n\n")

#----------------------------------------------------------------------------

###################################
# 4. Join and build panel         #
###################################

panel <- visit_daily %>%
  left_join(
    utci_daily %>% select(lga_clean, date,
                          utci_daily_max, utci_daytime_mean,
                          extreme_heat_38, extreme_heat_46,
                          dow_num, weekend, month_num,
                          year_month, year, hot_season),
    by = c("lga_clean", "visit_date" = "date")
  ) %>%
  mutate(
    log_visits = log(n_visits + 1),
    utci_dt_c  = utci_daytime_mean - mean(utci_daytime_mean, na.rm = TRUE),
    ym_factor  = as.factor(format(visit_date, "%Y-%m"))
  ) %>%
  filter(!is.na(utci_daily_max)) %>%
  arrange(lga_clean, visit_date) %>%
  group_by(lga_clean) %>%
  mutate(
    heat_lag1 = lag(extreme_heat_38, 1),
    heat_lag2 = lag(extreme_heat_38, 2),
    heat_lag3 = lag(extreme_heat_38, 3)
  ) %>%
  ungroup()

cat("Panel rows after join:", nrow(panel), "\n")
cat("Rows with UTCI matched:", sum(!is.na(panel$utci_daily_max)), "\n\n")

cat("--- Visit volume by LGA ---\n")
panel %>%
  group_by(lga_clean) %>%
  summarise(
    n_days      = n(),
    mean_visits = round(mean(n_visits), 1),
    sd_visits   = round(sd(n_visits), 1),
    zero_days   = sum(n_visits == 0),
    .groups     = "drop"
  ) %>% print()

cat("\n--- Exposure balance ---\n")
panel %>%
  group_by(lga_clean) %>%
  summarise(
    pct_extreme_heat  = round(mean(extreme_heat_38) * 100, 1),
    mean_daytime_utci = round(mean(utci_daytime_mean, na.rm = TRUE), 1),
    .groups = "drop"
  ) %>% print()

#----------------------------------------------------------------------------

###################################
# 5. Regression models            #
###################################

# Outcome: log(visits + 1)
# All models cluster SE at LGA level
# ym_factor FE absorbs seasonality — leaves within-month heat variation
# as the primary source of identification

# D1: Binary heat + LGA FE only — naive baseline
d1 <- feols(log_visits ~ extreme_heat_38 | lga_clean,
            data    = panel,
            cluster = ~lga_clean)

# D2: + day-of-week FE — controls for facility closure patterns
d2 <- feols(log_visits ~ extreme_heat_38 | lga_clean + dow_num,
            data    = panel,
            cluster = ~lga_clean)

# D3: + month-year FE — primary specification
# Absorbs all seasonality; identifies within-month variation
d3 <- feols(log_visits ~ extreme_heat_38 | lga_clean + dow_num + ym_factor,
            data    = panel,
            cluster = ~lga_clean)

# D4: Continuous daytime UTCI — alternative exposure
d4 <- feols(log_visits ~ utci_dt_c | lga_clean + dow_num + ym_factor,
            data    = panel,
            cluster = ~lga_clean)

# D5: Distributed lag — heat effect over 3 days
# Tests delayed behavioural response
d5 <- feols(log_visits ~ extreme_heat_38 + heat_lag1 + heat_lag2 + heat_lag3 |
              lga_clean + dow_num + ym_factor,
            data    = panel %>% filter(!is.na(heat_lag3)),
            cluster = ~lga_clean)

cat("=== REGRESSION RESULTS ===\n\n")

etable(d1, d2, d3, d4, d5,
       title    = "UTCI heat stress and facility visits — daily panel · Kano",
       digits   = 3,
       se.below = TRUE)

modelsummary(
  list(
    "D1: LGA FE"          = d1,
    "D2: +DOW FE"         = d2,
    "D3: +Month-year FE"  = d3,
    "D4: Continuous UTCI" = d4,
    "D5: Distributed lag" = d5
  ),
  stars   = c("*" = 0.1, "**" = 0.05, "***" = 0.01),
  gof_map = c("nobs", "r.squared", "adj.r.squared"),
  title   = "Heat stress and facility visits — daily panel · Kano Aug 2024 – Mar 2026",
  output  = file.path(out_dir, "06_regression_daily.docx")
)

# ADDED 13/7/2026 — .docx cannot be parsed by 09_visualizations.R's
# parse_ms_txt() helper (needs the same pipe-delimited plain-text format
# already used for 02_model_b_tracing_effectiveness.txt). Same model list,
# .txt output, so D3/D4 heat coefficients can be pulled dynamically instead
# of hand-typed into the weather table.
modelsummary(
  list(
    "D1: LGA FE"          = d1,
    "D2: +DOW FE"         = d2,
    "D3: +Month-year FE"  = d3,
    "D4: Continuous UTCI" = d4,
    "D5: Distributed lag" = d5
  ),
  stars   = c("*" = 0.1, "**" = 0.05, "***" = 0.01),
  gof_map = c("nobs", "r.squared", "adj.r.squared"),
  title   = "Heat stress and facility visits — daily panel · Kano Aug 2024 – Mar 2026",
  output  = file.path(out_dir, "06_regression_daily.txt")
)

cat("\nTable saved to:", file.path(out_dir, "06_regression_daily.docx"), "\n")
cat("Parseable copy saved to:", file.path(out_dir, "06_regression_daily.txt"), "\n")

#----------------------------------------------------------------------------

###################################
# 5b. Negative binomial comparison#
###################################

# ADDED 13/7/2026 — c155 (Aisha): argues negative binomial (or a zero-
# inflated Poisson) is likely the BETTER primary choice for this outcome,
# not just a robustness check. Log-transformed OLS assumes a log-normal
# relationship that count data does not follow well, the "+1" adjustment
# makes coefficients hard to interpret cleanly, and OLS residual
# assumptions are difficult to satisfy for counts. The overdispersion this
# argument rests on is the same overdispersion already confirmed in
# Figure 2.3 (V/M = 62.4 Gabasawa, V/M = 97.8 Ungogo) — that check used
# this exact panel, so no separate diagnostic is needed here.
#
# D3 is named the primary OLS spec above, so D3_nb is the direct
# negative-binomial counterpart: same FE structure, same clustering,
# outcome switched from log(n_visits + 1) to raw n_visits (fenegbin
# models the count directly, no log-transform needed).

d3_nb <- fenegbin(n_visits ~ extreme_heat_38 | lga_clean + dow_num + ym_factor,
                  data    = panel,
                  cluster = ~lga_clean)

# D4_nb: continuous exposure counterpart, same logic as D3_nb vs D3
d4_nb <- fenegbin(n_visits ~ utci_dt_c | lga_clean + dow_num + ym_factor,
                  data    = panel,
                  cluster = ~lga_clean)

# Small helper for the significance check below — not defined elsewhere in
# this script (09_visualization_markdown.Rmd has its own copy in its setup
# chunk; duplicated here so this script runs standalone).
star2 <- function(coef, se) {
  t <- abs(coef / se)
  if (t > 2.576) "***" else if (t > 1.96) "**" else if (t > 1.645) "*" else "n.s."
}

cat("=== NEGATIVE BINOMIAL COMPARISON — HEAT ===\n\n")

etable(d3, d3_nb, d4, d4_nb,
       title    = "OLS (log-visits) vs negative binomial — heat exposure",
       digits   = 4,
       se.below = TRUE)

modelsummary(
  list(
    "D3: OLS — binary heat"           = d3,
    "D3_nb: NB — binary heat"         = d3_nb,
    "D4: OLS — continuous UTCI"       = d4,
    "D4_nb: NB — continuous UTCI"     = d4_nb
  ),
  stars   = c("*" = 0.1, "**" = 0.05, "***" = 0.01),
  gof_map = c("nobs", "r.squared"),
  title   = "Heat stress — OLS vs negative binomial, Kano daily panel",
  output  = file.path(out_dir, "06_regression_nb_comparison.txt")
)

cat("\nTable saved to:", file.path(out_dir, "06_regression_nb_comparison.txt"), "\n")
cat("\nDirection/significance check (should agree if OLS retention is justified):\n")
cat("  D3 significant:   ", star2(coef(d3)["extreme_heat_38"],   se(d3)["extreme_heat_38"]),   "\n")
cat("  D3_nb significant:", star2(coef(d3_nb)["extreme_heat_38"],se(d3_nb)["extreme_heat_38"]),"\n")
cat("  D4 significant:   ", star2(coef(d4)["utci_dt_c"],         se(d4)["utci_dt_c"]),          "\n")
cat("  D4_nb significant:", star2(coef(d4_nb)["utci_dt_c"],      se(d4_nb)["utci_dt_c"]),       "\n\n")
cat("NOTE: this table answers whether OLS and NB agree for THIS variable.\n")
cat("It does not by itself settle whether OLS or NB should be the primary\n")
cat("spec across all three weather variables — that is a document-wide\n")
cat("methods decision (c155) that should be made once with Prabin and\n")
cat("applied consistently to heat, precipitation and NDVI together.\n\n")

#----------------------------------------------------------------------------

###################################
# 6. Visualisations               #
###################################

pal <- c("Ungogo" = "#D84A38", "Gabasawa" = "#1D6FA4")

# -- Plot 1: Scatter — daytime UTCI vs log visits ----------------------

p1 <- ggplot(panel, aes(x = utci_daytime_mean, y = log_visits,
                        colour = lga_clean)) +
  geom_point(alpha = 0.15, size = 1) +
  geom_smooth(method = "lm", se = TRUE, linewidth = 1) +
  geom_vline(xintercept = 38, linetype = "dashed",
             colour = "#BA7517", linewidth = 0.7) +
  scale_colour_manual(values = pal) +
  labs(
    title   = "Daytime UTCI vs facility visits — raw relationship",
    x       = "Daytime mean UTCI (°C, local 09:00–18:00)",
    y       = "Log facility visits",
    colour  = NULL,
    caption = "Dashed = 38°C threshold · Each point = one LGA-day"
  ) +
  theme_minimal(base_size = 12) +
  theme(plot.title      = element_text(face = "bold"),
        legend.position = "top")

# -- Plot 2: Visits time series with heat days shaded ------------------

extreme_days <- panel %>%
  filter(extreme_heat_38 == 1) %>%
  select(visit_date) %>%
  distinct()

p2 <- ggplot(panel, aes(x = visit_date)) +
  geom_rect(
    data        = extreme_days,
    aes(xmin = visit_date - 0.5, xmax = visit_date + 0.5,
        ymin = -Inf, ymax = Inf),
    fill        = "#BA7517", alpha = 0.08,
    inherit.aes = FALSE
  ) +
  geom_line(aes(y = n_visits, colour = lga_clean),
            alpha = 0.6, linewidth = 0.4) +
  geom_smooth(aes(y = n_visits, colour = lga_clean),
              method = "loess", span = 0.08,
              se = FALSE, linewidth = 1) +
  scale_colour_manual(values = pal) +
  scale_x_date(date_breaks = "2 months", date_labels = "%b %Y") +
  labs(
    title    = "Daily facility visits across programme window",
    subtitle = "Orange shading = extreme heat days (UTCI ≥ 38°C)",
    x        = NULL, y = "Visits per day", colour = NULL
  ) +
  theme_minimal(base_size = 12) +
  theme(plot.title      = element_text(face = "bold"),
        legend.position = "top",
        axis.text.x     = element_text(angle = 30, hjust = 1))

# -- Plot 3: Mean visits on extreme vs non-extreme days ----------------

visit_by_heat <- panel %>%
  mutate(heat_label = if_else(
    extreme_heat_38 == 1,
    "Extreme heat\n(UTCI ≥ 38°C)",
    "Non-extreme\n(UTCI < 38°C)"
  )) %>%
  group_by(lga_clean, heat_label) %>%
  summarise(
    mean_visits = mean(n_visits),
    se_visits   = sd(n_visits) / sqrt(n()),
    .groups     = "drop"
  )

p3 <- ggplot(visit_by_heat, aes(x = heat_label, y = mean_visits,
                                fill = lga_clean)) +
  geom_col(position = "dodge", alpha = 0.85, width = 0.6) +
  geom_errorbar(aes(ymin = mean_visits - se_visits,
                    ymax = mean_visits + se_visits),
                position = position_dodge(0.6),
                width = 0.2, linewidth = 0.7) +
  scale_fill_manual(values = pal) +
  labs(
    title   = "Mean daily visits: extreme heat vs other days",
    subtitle = "Error bars = ±1 SE · Unadjusted",
    x       = NULL, y = "Mean visits per day", fill = NULL,
    caption = "Raw comparison — not controlling for seasonality or DOW"
  ) +
  theme_minimal(base_size = 12) +
  theme(plot.title      = element_text(face = "bold"),
        legend.position = "top")

# -- Plot 4: Distributed lag coefficients from D5 ----------------------

lag_coefs <- broom::tidy(d5) %>%
  filter(str_detect(term, "extreme_heat|heat_lag")) %>%
  mutate(
    lag_day = case_when(
      term == "extreme_heat_38" ~ "Day 0\n(same day)",
      term == "heat_lag1"       ~ "Day -1\n(yesterday)",
      term == "heat_lag2"       ~ "Day -2",
      term == "heat_lag3"       ~ "Day -3"
    ),
    lag_day = factor(lag_day,
                     levels = c("Day -3", "Day -2",
                                "Day -1\n(yesterday)",
                                "Day 0\n(same day)"))
  )

p4 <- ggplot(lag_coefs, aes(x = lag_day, y = estimate)) +
  geom_col(fill = "#7F77DD", alpha = 0.8, width = 0.5) +
  geom_errorbar(aes(ymin = estimate - std.error,
                    ymax = estimate + std.error),
                width = 0.2, linewidth = 0.8) +
  geom_hline(yintercept = 0, linewidth = 0.7) +
  labs(
    title   = "Distributed lag: heat effect over 3 days",
    subtitle = "Model D5 · LGA + DOW + month-year FE · Clustered SE",
    x       = NULL,
    y       = "Coefficient on log(visits + 1)",
    caption = "Error bars = ±1 SE"
  ) +
  theme_minimal(base_size = 12) +
  theme(plot.title = element_text(face = "bold"))

# -- Combine -----------------------------------------------------------

combined <- (p1 + p2) / (p3 + p4) +
  plot_annotation(
    title    = "ERA5 UTCI heat stress and facility visits — daily panel",
    subtitle = "Ungogo & Gabasawa LGAs · Kano · Aug 2024 – Mar 2026",
    theme    = theme(
      plot.title    = element_text(face = "bold", size = 14),
      plot.subtitle = element_text(size = 11)
    )
  )

ggsave(
  filename = file.path(out_dir, "06_heat_visits_daily.png"),
  plot     = combined,
  width    = 14, height = 10, dpi = 300
)

cat("Plots saved to:", out_dir, "\n")

#----------------------------------------------------------------------------

###################################
# 7. Save panel                   #
###################################

saveRDS(panel, file.path(out_dir, "06_panel_daily.rds"))
write_csv(panel, file.path(out_dir, "06_panel_daily.csv"))

cat("Panel saved:", nrow(panel), "LGA-day observations\n")
cat("\n--- Script complete ---\n")

#----------------------------------------------------------------------------

###################################
# 8. Facility-level heterogeneity #
###################################

# Purpose: test whether individual facilities show heterogeneous responses
# to heat stress that the LGA-level aggregation conceals.
# Key question for supervisors: is the null finding uniform across all
# facilities, or do some show strong signals in either direction?
#
# NOTE: All facilities within each LGA share the same ERA5 UTCI value —
# there is no spatial climate variation at this resolution. Facility FE
# therefore absorbs level differences in visit volume but does NOT add
# new climate variation. If heterogeneity exists, it reflects differences
# in facility-level sensitivity to heat (e.g. catchment population,
# facility type, operational schedule) rather than microclimate.
# MODIS LST at 1km resolution would be needed to test the true
# microclimate hypothesis.

cat("\n=== SECTION 8: FACILITY-LEVEL HETEROGENEITY ===\n\n")

# Load facility-level visits — requires health_center_name
data_fv_fac <- readRDS(
  file.path(mchtrack_dir, "01_facility_visits_clean.rds")
) %>%
  filter(
    str_detect(tolower(lga_name), "ungogo|gabasawa"),
    woman_or_child == "child",
    !rimi_flag
  ) %>%
  mutate(
    visit_date    = as.Date(visit_date),
    lga_clean     = str_to_title(str_trim(
      str_remove(lga_name, regex("\\s*lga\\s*$", ignore_case = TRUE))
    )),
    facility_name = str_to_title(str_trim(health_center_name))
  ) %>%
  filter(
    visit_date >= as.Date("2024-08-01"),
    visit_date <= as.Date("2026-03-31")
  )

cat("Facilities in data:\n")
data_fv_fac %>%
  count(lga_clean, facility_name) %>%
  print(n = 30)

#----------------------------------------------------------------------------

# Build facility x day panel
visit_fac <- data_fv_fac %>%
  group_by(lga_clean, facility_name, visit_date) %>%
  summarise(n_visits = n(), .groups = "drop")

# Full date spine — every facility x day
fac_list <- data_fv_fac %>%
  distinct(lga_clean, facility_name)

fac_spine <- expand_grid(
  fac_list,
  visit_date = seq(as.Date("2024-08-01"),
                   as.Date("2026-03-31"),
                   by = "day")
)

visit_fac <- fac_spine %>%
  left_join(visit_fac,
            by = c("lga_clean", "facility_name", "visit_date")) %>%
  mutate(n_visits = replace_na(n_visits, 0))

# Merge UTCI — same values for all facilities within LGA
utci_join <- utci_daily %>%
  select(lga_clean, date, utci_daily_max, utci_daytime_mean,
         extreme_heat_38, dow_num, weekend, month_num,
         year_month, year, hot_season) %>%
  mutate(visit_date = date)

panel_fac <- visit_fac %>%
  left_join(utci_join,
            by = c("lga_clean", "visit_date")) %>%
  mutate(
    log_visits = log(n_visits + 1),
    ym_factor  = as.factor(format(visit_date, "%Y-%m")),
    # Exclude Eid days — institutional closure unrelated to climate
    eid_day    = visit_date %in% as.Date(c(
      "2025-03-30", "2025-03-31", "2025-04-01",  # Eid al-Fitr 2025
      "2025-06-06", "2025-06-07", "2025-06-08"   # Eid al-Adha 2025
    )),
    # Ramadan 2025 indicator — religious calendar confounder
    ramadan_25 = visit_date >= as.Date("2025-03-01") &
      visit_date <= as.Date("2025-03-29")
  ) %>%
  filter(!is.na(utci_daily_max), !eid_day) %>%
  arrange(lga_clean, facility_name, visit_date)

cat("\nFacility panel rows:", nrow(panel_fac), "\n")
cat("Facilities:", n_distinct(panel_fac$facility_name), "\n")
cat("Eid days excluded: 6\n\n")

# Summary of volume per facility
cat("--- Volume per facility (non-zero days) ---\n")
panel_fac %>%
  filter(n_visits > 0) %>%
  group_by(lga_clean, facility_name) %>%
  summarise(
    n_active_days = n(),
    mean_visits   = round(mean(n_visits), 1),
    median_visits = round(median(n_visits), 1),
    sd_visits     = round(sd(n_visits), 1),
    pct_zero_all  = round(
      mean(panel_fac$n_visits[
        panel_fac$facility_name == first(facility_name)] == 0) * 100, 1),
    .groups = "drop"
  ) %>%
  print(n = 30)

#----------------------------------------------------------------------------

# Run facility-level regression: primary spec (D3 equivalent)
# + facility FE instead of LGA FE
# + Ramadan control added (key confounder identified in analysis)
# Note: clustered SE at facility level (23 facilities — still small)

cat("\n--- Running facility-level regressions ---\n")

# F1: Facility FE + DOW + month-year FE
f1 <- feols(log_visits ~ extreme_heat_38 | facility_name + dow_num + ym_factor,
            data    = panel_fac,
            cluster = ~facility_name)

# F2: + Ramadan control (new — recommended after calendar analysis)
f2 <- feols(log_visits ~ extreme_heat_38 + ramadan_25 |
              facility_name + dow_num + ym_factor,
            data    = panel_fac,
            cluster = ~facility_name)

# F3: Continuous UTCI with Ramadan control
f3 <- feols(log_visits ~ utci_daytime_mean + ramadan_25 |
              facility_name + dow_num + ym_factor,
            data    = panel_fac %>%
              mutate(utci_daytime_mean =
                       utci_daytime_mean -
                       mean(utci_daytime_mean, na.rm = TRUE)),
            cluster = ~facility_name)

cat("\n=== FACILITY-LEVEL REGRESSION RESULTS ===\n\n")
etable(f1, f2, f3,
       title    = "Facility-level heat and visits · Kano",
       digits   = 3,
       se.below = TRUE)

#----------------------------------------------------------------------------

# Per-facility coefficients — the heterogeneity diagnostic
# Run a separate simple regression for each facility and collect coefficients

cat("\n--- Per-facility coefficient distribution ---\n")

facilities <- unique(panel_fac$facility_name)

fac_coefs <- map_dfr(facilities, function(fac) {
  
  sub <- panel_fac %>%
    filter(facility_name == fac,
           !weekend,
           !is.na(extreme_heat_38))
  
  # Need at least 60 observations and both heat/non-heat days
  if (nrow(sub) < 60 ||
      sum(sub$extreme_heat_38) < 10 ||
      sum(sub$extreme_heat_38 == 0) < 10) {
    return(tibble(
      facility    = fac,
      lga         = unique(sub$lga_clean),
      coef        = NA_real_,
      se          = NA_real_,
      n           = nrow(sub),
      flag        = "insufficient data"
    ))
  }
  
  tryCatch({
    m <- feols(log_visits ~ extreme_heat_38 | dow_num + ym_factor,
               data = sub, vcov = "hetero")
    coef_val <- coef(m)["extreme_heat_38"]
    se_val   <- se(m)["extreme_heat_38"]
    tibble(
      facility = fac,
      lga      = unique(sub$lga_clean),
      coef     = coef_val,
      se       = se_val,
      n        = nrow(sub),
      flag     = "ok"
    )
  }, error = function(e) {
    tibble(facility=fac, lga=unique(sub$lga_clean),
           coef=NA_real_, se=NA_real_, n=nrow(sub), flag="error")
  })
})

cat("\nPer-facility coefficients:\n")
fac_coefs %>%
  filter(flag == "ok") %>%
  arrange(lga, coef) %>%
  mutate(
    ci_lo  = round(coef - 1.96*se, 3),
    ci_hi  = round(coef + 1.96*se, 3),
    sig    = if_else(abs(coef/se) > 1.96, "p<0.05", "n.s."),
    coef   = round(coef, 3),
    se     = round(se, 3)
  ) %>%
  select(lga, facility, coef, se, ci_lo, ci_hi, sig, n) %>%
  print(n = 30)

# Summary of heterogeneity
valid_coefs <- fac_coefs %>% filter(flag == "ok", !is.na(coef))

cat("\n--- Heterogeneity summary ---\n")
cat("Facilities with valid coefficients:", nrow(valid_coefs), "\n")
cat("Negative coefficients:",
    sum(valid_coefs$coef < 0), "/", nrow(valid_coefs), "\n")
cat("Positive coefficients:",
    sum(valid_coefs$coef > 0), "/", nrow(valid_coefs), "\n")
cat("Significant (p<0.05):",
    sum(abs(valid_coefs$coef / valid_coefs$se) > 1.96), "\n")
cat("SD of coefficients across facilities:",
    round(sd(valid_coefs$coef, na.rm = TRUE), 3), "\n")
cat("Range:", round(min(valid_coefs$coef), 3),
    "to", round(max(valid_coefs$coef), 3), "\n\n")

cat("Interpretation:\n")
cat("If SD is large and sign is mixed: heterogeneity exists — MODIS warranted\n")
cat("If SD is small and all near zero: null is uniform — MODIS unlikely to help\n")

# Check Tudun Fulani visit pattern over time
panel_fac %>%
  filter(facility_name == "Tudun Fulani Hf") %>%
  mutate(month = floor_date(visit_date, "month")) %>%
  group_by(month) %>%
  summarise(
    mean_visits   = round(mean(n_visits), 1),
    zero_days     = sum(n_visits == 0),
    total_days    = n(),
    mean_utci     = round(mean(utci_daytime_mean, na.rm = TRUE), 1),
    .groups = "drop"
  ) %>%
  print(n = 25)

#----------------------------------------------------------------------------

# Coefficient plot — visualise heterogeneity across facilities

valid_plot <- fac_coefs %>%
  filter(flag == "ok", !is.na(coef)) %>%
  mutate(
    sig      = abs(coef/se) > 1.96,
    ci_lo    = coef - 1.96*se,
    ci_hi    = coef + 1.96*se,
    facility = str_wrap(facility, 25),
    facility = fct_reorder(facility, coef)
  )

p_fac <- ggplot(valid_plot,
                aes(x = coef, y = facility, colour = lga)) +
  geom_vline(xintercept = 0, linewidth = 0.7,
             colour = "#888780", linetype = "dashed") +
  geom_errorbarh(aes(xmin = ci_lo, xmax = ci_hi),
                 height = 0.3, linewidth = 0.7, alpha = 0.6) +
  geom_point(aes(size = sig, shape = sig)) +
  scale_colour_manual(
    values = c("Ungogo" = "#D84A38", "Gabasawa" = "#1D6FA4")
  ) +
  scale_size_manual(
    values = c("TRUE" = 3.5, "FALSE" = 2.5),
    guide  = "none"
  ) +
  scale_shape_manual(
    values = c("TRUE" = 16, "FALSE" = 1),
    labels = c("TRUE" = "p < 0.05", "FALSE" = "n.s."),
    name   = NULL
  ) +
  labs(
    title    = "Facility-level heat coefficients — heterogeneity diagnostic",
    subtitle = "Coefficient on extreme heat day (UTCI ≥ 38°C) · Facility + DOW + month-year FE · Robust SE",
    x        = "Coefficient on log(visits + 1)",
    y        = NULL,
    colour   = "LGA",
    caption  = paste0(
      "Each point = one facility · Bars = 95% CI · ",
      "Filled = p<0.05 · Wide spread suggests microclimate investigation warranted"
    )
  ) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title      = element_text(face = "bold"),
    legend.position = "top",
    axis.text.y     = element_text(size = 9)
  )

ggsave(
  filename = file.path(out_dir, "06_facility_heterogeneity.png"),
  plot     = p_fac,
  width    = 10, height = 8, dpi = 300
)

cat("Facility heterogeneity plot saved.\n")

#----------------------------------------------------------------------------

# Save facility panel
saveRDS(panel_fac,
        file.path(out_dir, "06_panel_facility.rds"))

write_csv(fac_coefs,
          file.path(out_dir, "06_facility_coefficients.csv"))

cat("Facility panel and coefficients saved.\n")
cat("\n--- Section 8 complete ---\n")
cat("Next step: review heterogeneity summary above and coefficient plot\n")
cat("before deciding whether MODIS LST extraction is warranted\n")

#--------------------------(END)------------------------------#
