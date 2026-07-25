########################################
#  06_era5_analysis.R                  #
#  Created: June 2026                  #
#  Updated: 24/7/2026                  #
########################################

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

#----------------------------------------------------------------------------

###################################
# 2. Build daily outcome panel    #
###################################

visit_daily <- data_fv_kano %>%
  group_by(lga_clean, visit_date) %>%
  summarise(
    n_visits          = n(),
    n_unique_children = n_distinct(patient_id),
    .groups = "drop"
  )

date_spine <- expand_grid(
  lga_clean  = unique(visit_daily$lga_clean),
  visit_date = seq(as.Date("2024-08-01"),
                   as.Date("2026-03-31"),
                   by = "day")
)

visit_daily <- date_spine %>%
  left_join(visit_daily, by = c("lga_clean", "visit_date")) %>%
  mutate(n_visits = replace_na(n_visits, 0))

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

#----------------------------------------------------------------------------

###################################
# 5. Regression models            #
###################################

# Outcome log(visits + 1), SE clustered at LGA. ym_factor FE absorbs
# seasonality, leaving within-month heat variation as identification.

d1 <- feols(log_visits ~ extreme_heat_38 | lga_clean,
            data    = panel,
            cluster = ~lga_clean)

d2 <- feols(log_visits ~ extreme_heat_38 | lga_clean + dow_num,
            data    = panel,
            cluster = ~lga_clean)

# D3: reference daily FE structure. NB counterpart below (Section 5b) is
# the primary specification used in the write-up per Prabin's review.
d3 <- feols(log_visits ~ extreme_heat_38 | lga_clean + dow_num + ym_factor,
            data    = panel,
            cluster = ~lga_clean)

d4 <- feols(log_visits ~ utci_dt_c | lga_clean + dow_num + ym_factor,
            data    = panel,
            cluster = ~lga_clean)

# D5: distributed lag — tests delayed behavioural response over 3 days
d5 <- feols(log_visits ~ extreme_heat_38 + heat_lag1 + heat_lag2 + heat_lag3 |
              lga_clean + dow_num + ym_factor,
            data    = panel %>% filter(!is.na(heat_lag3)),
            cluster = ~lga_clean)

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

# .docx isn't parseable by 09/10's parse_ms_txt() helper, so the same
# table is also written as plain text — that's what gets read downstream.
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

#----------------------------------------------------------------------------

###################################
# 5b. Negative binomial — primary #
###################################

# NB is the primary specification per Prabin's second-draft review (c84) —
# resolves the open question Aisha raised here on 13/7 (c155). Same
# reasoning as 02_chirps_import_analysis.R Section 12, applied consistently
# across heat, precipitation and NDVI. OLS retained for comparison.
# Column order flipped vs the previous draft — NB leads. 10_visualizations.R's
# parser needs updating to match.

d3_nb <- fenegbin(n_visits ~ extreme_heat_38 | lga_clean + dow_num + ym_factor,
                  data    = panel,
                  cluster = ~lga_clean)

d4_nb <- fenegbin(n_visits ~ utci_dt_c | lga_clean + dow_num + ym_factor,
                  data    = panel,
                  cluster = ~lga_clean)

etable(d3_nb, d3, d4_nb, d4,
       title    = "Negative binomial (primary) vs OLS log-visits — heat exposure",
       digits   = 4,
       se.below = TRUE)

modelsummary(
  list(
    "D3_nb: NB — binary heat (primary)"     = d3_nb,
    "D3: OLS — binary heat"                 = d3,
    "D4_nb: NB — continuous UTCI (primary)" = d4_nb,
    "D4: OLS — continuous UTCI"             = d4
  ),
  stars   = c("*" = 0.1, "**" = 0.05, "***" = 0.01),
  gof_map = c("nobs", "r.squared"),
  title   = "Heat stress — negative binomial (primary) vs OLS, Kano daily panel",
  output  = file.path(out_dir, "06_regression_nb_comparison.txt")
)

#----------------------------------------------------------------------------

#############################################
# 5c. Exposure/offset + spline robustness   #
# (Prabin Dahal, 15/7/2026 review)          #
#############################################

# Same additions as 02_chirps_import_analysis.R Section 12b, same reasoning.
# Offset uses enrolled_children as exposure denominator — a cumulative
# registration stock, not a true point-in-time count; best available proxy.
# Spline tests non-linearity in the continuous UTCI term (extreme_heat_38
# is already a threshold form, so it doesn't need one).
# Saved to its own file — 10_visualizations.R reads fixed columns from
# 06_regression_daily.txt and 06_regression_nb_comparison.txt.

lga_month_path <- file.path(mchtrack_dir, "01_panel_lga_month.rds")

if (file.exists(lga_month_path)) {
  
  # enrolled_lookup's year_month is a character "%Y-%m" string; panel's is
  # a <date>. Join on a derived character key on both sides so a format
  # drift doesn't silently produce an all-NA join.
  enrolled_lookup <- readRDS(lga_month_path) %>%
    filter(state == "Kano") %>%
    mutate(
      lga_clean = str_to_title(str_remove(lga_name, regex("\\s*lga\\s*$", ignore_case = TRUE))),
      ym_key    = as.character(year_month)
    ) %>%
    filter(lga_clean %in% c("Ungogo", "Gabasawa")) %>%
    distinct(lga_clean, ym_key, enrolled_children)
  
  # Joined onto a copy — panel and its Section 7 exports stay untouched.
  panel_off <- panel %>%
    mutate(ym_key = format(year_month, "%Y-%m")) %>%
    left_join(enrolled_lookup, by = c("lga_clean", "ym_key"))
  
  panel_off_valid <- panel_off %>% filter(!is.na(enrolled_children), enrolled_children > 0)
  
  d3_off <- feols(log_visits ~ extreme_heat_38 | lga_clean + dow_num + ym_factor,
                  data    = panel_off_valid,
                  offset  = ~log(enrolled_children),
                  cluster = ~lga_clean)
  
  d4_off <- feols(log_visits ~ utci_dt_c | lga_clean + dow_num + ym_factor,
                  data    = panel_off_valid,
                  offset  = ~log(enrolled_children),
                  cluster = ~lga_clean)
  
  d3_nb_off <- fenegbin(n_visits ~ extreme_heat_38 | lga_clean + dow_num + ym_factor,
                        data    = panel_off_valid,
                        offset  = ~log(enrolled_children),
                        cluster = ~lga_clean)
  
  d4_spline <- feols(log_visits ~ splines::ns(utci_dt_c, df = 3) | lga_clean + dow_num + ym_factor,
                     data    = panel,
                     cluster = ~lga_clean)
  
  etable(d3, d3_off, d3_nb_off, d4, d4_off, d4_spline,
         title    = "Heat robustness — offset and spline specifications",
         digits   = 4,
         se.below = TRUE)
  
  modelsummary(
    list(
      "D3_off: binary heat, offset"        = d3_off,
      "D3_nb_off: NB binary heat, offset"  = d3_nb_off,
      "D4_off: continuous UTCI, offset"    = d4_off,
      "D4_spline: continuous UTCI, spline (no offset)" = d4_spline
    ),
    stars   = c("*" = 0.1, "**" = 0.05, "***" = 0.01),
    gof_map = c("nobs", "r.squared"),
    title   = "Heat — exposure-offset and spline robustness (Prabin, 15/7/2026)",
    output  = file.path(out_dir, "06_regression_daily_robustness_prabin.txt")
  )
}

#----------------------------------------------------------------------------

###################################
# 6. Visualisations               #
###################################

pal <- c("Ungogo" = "#D84A38", "Gabasawa" = "#1D6FA4")

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

#----------------------------------------------------------------------------

###################################
# 7. Save panel                   #
###################################

saveRDS(panel, file.path(out_dir, "06_panel_daily.rds"))
write_csv(panel, file.path(out_dir, "06_panel_daily.csv"))

#----------------------------------------------------------------------------

###################################
# 8. Facility-level heterogeneity #
###################################

# Tests whether individual facilities respond differently to heat than the
# LGA-level aggregation shows. All facilities within an LGA share the same
# ERA5 UTCI value, so facility FE captures sensitivity differences
# (catchment, facility type, schedule), not microclimate — MODIS LST at
# 1km resolution would be needed for that.

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

visit_fac <- data_fv_fac %>%
  group_by(lga_clean, facility_name, visit_date) %>%
  summarise(n_visits = n(), .groups = "drop")

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

# Same UTCI values for every facility within an LGA
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
    # Eid days — institutional closure unrelated to climate
    eid_day    = visit_date %in% as.Date(c(
      "2025-03-30", "2025-03-31", "2025-04-01",  # Eid al-Fitr 2025
      "2025-06-06", "2025-06-07", "2025-06-08"   # Eid al-Adha 2025
    )),
    ramadan_25 = visit_date >= as.Date("2025-03-01") &
      visit_date <= as.Date("2025-03-29")
  ) %>%
  filter(!is.na(utci_daily_max), !eid_day) %>%
  arrange(lga_clean, facility_name, visit_date)

#----------------------------------------------------------------------------

# Facility-level regression: F1 primary spec, F2 adds Ramadan control,
# F3 continuous UTCI + Ramadan. Clustered SE at facility level (23
# facilities — still small).

f1 <- feols(log_visits ~ extreme_heat_38 | facility_name + dow_num + ym_factor,
            data    = panel_fac,
            cluster = ~facility_name)

f2 <- feols(log_visits ~ extreme_heat_38 + ramadan_25 |
              facility_name + dow_num + ym_factor,
            data    = panel_fac,
            cluster = ~facility_name)

f3 <- feols(log_visits ~ utci_daytime_mean + ramadan_25 |
              facility_name + dow_num + ym_factor,
            data    = panel_fac %>%
              mutate(utci_daytime_mean =
                       utci_daytime_mean -
                       mean(utci_daytime_mean, na.rm = TRUE)),
            cluster = ~facility_name)

etable(f1, f2, f3,
       title    = "Facility-level heat and visits · Kano",
       digits   = 3,
       se.below = TRUE)

#----------------------------------------------------------------------------

# Per-facility coefficients — the heterogeneity diagnostic. Separate simple
# regression per facility, coefficients collected for the plot below.

facilities <- unique(panel_fac$facility_name)

fac_coefs <- map_dfr(facilities, function(fac) {
  
  sub <- panel_fac %>%
    filter(facility_name == fac,
           !weekend,
           !is.na(extreme_heat_38))
  
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

saveRDS(panel_fac,
        file.path(out_dir, "06_panel_facility.rds"))

write_csv(fac_coefs,
          file.path(out_dir, "06_facility_coefficients.csv"))

#--------------------------(END)------------------------------#
