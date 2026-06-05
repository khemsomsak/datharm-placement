########################################################
#  ERA5 UTCI Heat Stress — Daily Regression Analysis   #
#  Outcome: Daily facility visit counts, Kano          #
#  Exposure: Extreme heat days (UTCI >= 38°C)          #
#  Sites: Ungogo & Gabasawa LGAs                       #
#  Created: June 2026                                  #
########################################################

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

# Zero visit day diagnostic — base R only, no pipe conflicts

# 1. Day of week breakdown
dow_tab <- table(
  weekdays(visit_daily$visit_date[visit_daily$n_visits == 0])
)
cat("--- Zero-visit days by day of week ---\n")
print(sort(dow_tab, decreasing = TRUE))

# 2. Zero rate by month
cat("\n--- Zero-visit days by month ---\n")
visit_daily$ym <- format(visit_daily$visit_date, "%Y-%m")
month_tab <- aggregate(n_visits ~ ym + lga_clean, data = visit_daily,
                       FUN = function(x) sum(x == 0))
names(month_tab)[3] <- "zero_days"
month_tab$total_days <- aggregate(n_visits ~ ym + lga_clean,
                                  data = visit_daily,
                                  FUN = length)$n_visits
month_tab$pct_zero <- round(month_tab$zero_days / month_tab$total_days * 100, 1)
print(month_tab[order(month_tab$lga_clean, month_tab$ym), ],
      row.names = FALSE)

# 3. Overall zero rate by LGA
cat("\n--- Zero rate by LGA ---\n")
lga_tab <- aggregate(n_visits ~ lga_clean, data = visit_daily,
                     FUN = function(x) c(zeros = sum(x == 0),
                                         total = length(x),
                                         pct   = round(sum(x == 0)/length(x)*100, 1)))
print(lga_tab)

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

# D6: Poisson — more appropriate for count outcome with many zeros
d6 <- feglm(n_visits ~ extreme_heat_38 | lga_clean + dow_num + ym_factor,
            data   = panel,
            family = poisson(),
            cluster = ~lga_clean)

# D3 with robust SE instead of clustered
d3_robust <- feols(log_visits ~ extreme_heat_38 | 
                     lga_clean + dow_num + ym_factor,
                   data = panel,
                   vcov = "hetero")

d6_robust <- feglm(n_visits ~ extreme_heat_38 | 
                     lga_clean + dow_num + ym_factor,
                   data   = panel,
                   family = poisson(),
                   vcov   = "hetero")

etable(d3, d3_robust, d6, d6_robust, digits = 3, se.below = TRUE)

d3_ungogo   <- feols(log_visits ~ extreme_heat_38 | dow_num + ym_factor,
                     data  = panel %>% filter(lga_clean == "Ungogo"),
                     vcov  = "hetero")

d3_gabasawa <- feols(log_visits ~ extreme_heat_38 | dow_num + ym_factor,
                     data  = panel %>% filter(lga_clean == "Gabasawa"),
                     vcov  = "hetero")

etable(d3_ungogo, d3_gabasawa, digits = 3, se.below = TRUE)

cat("=== REGRESSION RESULTS ===\n\n")

etable(d1, d2, d3, d4, d5, d6, d3_robust, d6_robust,
       title    = "UTCI heat stress and facility visits — daily panel · Kano",
       digits   = 3,
       se.below = TRUE)

modelsummary(
  list(
    "D1: LGA FE"          = d1,
    "D2: +DOW FE"         = d2,
    "D3: +Month-year FE"  = d3,
    "D3 Robust"           = d3_robust,
    "D4: Continuous UTCI" = d4,
    "D5: Distributed lag" = d5,
    "D6: Poisson"         = d6,
    "D6 Robust"           = d6_robust  
  ),
  stars   = c("*" = 0.1, "**" = 0.05, "***" = 0.01),
  gof_map = c("nobs", "r.squared", "adj.r.squared"),
  title   = "Heat stress and facility visits — daily panel · Kano Aug 2024 – Mar 2026",
  output  = file.path(out_dir, "06_regression_daily.docx")
)

cat("\nTable saved to:", file.path(out_dir, "06_regression_daily.docx"), "\n")

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