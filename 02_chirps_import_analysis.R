########################################
#  02_chirps_import_analysis.R         #
#  Created: 13/5/2026                  #
#  Updated: 24/7/2026                  #
########################################

rm(list = ls())
setwd("C:/Users/HP/Documents/GitHub/datharm-placement")

options(scipen = 999)
Sys.setlocale("LC_TIME", "English")

home         <- "C:/Users/HP/Documents/GitHub/datharm-placement"
raw_dir      <- file.path(home, "02_data/03_external")
mchtrack_dir <- file.path(home, "03_output/01_mchtrack_data")
import_dir   <- file.path(home, "03_output/02_chirps_data")
out_dir      <- file.path(home, "03_output/02_chirps_analysis")
dir.create(import_dir, showWarnings = FALSE, recursive = TRUE)
dir.create(out_dir,    showWarnings = FALSE, recursive = TRUE)

library(sf)
library(terra)
library(chirps)

library(janitor)
library(tidyverse)
library(dplyr)
library(tidyr)
library(ggplot2)
library(readr)
library(readxl)
library(lubridate)
library(fixest)        # feols() / fenegbin() for panel regression
library(modelsummary)
library(patchwork)
library(scales)

#----------------------------------------------------------------------------

###################
# Import Database #
###################

# 1. Import rainfall CHIRPS dataset -----------------------------------------

data_file <- file.path(raw_dir, "nga-rainfall-subnat-5ytd.csv")
data_raw  <- read_csv(data_file, show_col_types = FALSE) %>%
  clean_names()

############
# Cleaning #
############

# 2. Filter to Kano and Katsina LGAs -----------------------------------------

data_kk <- data_raw %>%
  filter(
    adm_level == 2,
    str_starts(pcode, "NG019") | str_starts(pcode, "NG020")
  )

# 3. Aggregate dekadal to monthly --------------------------------------------

data_kk_monthly <- data_kk %>%
  mutate(
    year_month = format(as.Date(date), "%Y-%m")
  ) %>%
  group_by(pcode, year_month) %>%
  summarise(
    dekads_present          = n(),
    precip_actual_mm        = mean(as.numeric(r1h),     na.rm = TRUE),
    precip_longterm_avg_mm  = mean(as.numeric(r1h_avg), na.rm = TRUE),
    precip_anomaly_pct      = mean(as.numeric(r1q),     na.rm = TRUE),
    precip_abs_dev_mm       = mean(as.numeric(r1h) - as.numeric(r1h_avg), na.rm = TRUE),
    .groups = "drop"
  ) %>%
  filter(year_month >= "2024-08")  # MCHTrack starts Aug 2024

# Dekadal dataset — one row per pcode x dekad, no aggregation
data_kk_dekadal <- data_kk %>%
  mutate(
    date_parsed = as.Date(date),
    dekad_num   = case_when(
      day(date_parsed) <= 10 ~ "D1",
      day(date_parsed) <= 20 ~ "D2",
      TRUE                   ~ "D3"
    ),
    year_month        = format(date_parsed, "%Y-%m"),
    dekad_id          = paste0(year_month, "-", dekad_num),
    precip_actual_mm  = as.numeric(r1h),
    precip_abs_dev_mm = as.numeric(r1h) - as.numeric(r1h_avg),
    precip_anomaly_pct = as.numeric(r1q)
  ) %>%
  filter(date_parsed >= as.Date("2024-08-01")) %>%
  select(pcode, year_month, dekad_id, dekad_num,
         precip_actual_mm, precip_abs_dev_mm, precip_anomaly_pct)

# 4. LGA name lookup ----------------------------------------------------

lga_lookup <- tribble(
  ~pcode,     ~lga_name_mchtrack,
  # ── KANO ──
  "NG019001",  "Ajingi LGA",
  "NG019002",  "Albasu LGA",
  "NG019003",  "Bagwai LGA",
  "NG019004",  "Bebeji LGA",
  "NG019005",  "Bichi LGA",
  "NG019006",  "Bunkure LGA",
  "NG019007",  "Dala LGA",
  "NG019008",  "Dambatta LGA",
  "NG019009",  "Dawakin Kudu LGA",
  "NG019010",  "Dawakin Tofa LGA",
  "NG019011",  "Doguwa LGA",
  "NG019012",  "Fagge LGA",
  "NG019013",  "Gabasawa LGA",
  "NG019014",  "Garko LGA",
  "NG019015",  "Garun Mallam LGA",
  "NG019016",  "Gaya LGA",
  "NG019017",  "Gezawa LGA",
  "NG019018",  "Gwale LGA",
  "NG019019",  "Gwarzo LGA",
  "NG019020",  "Kabo LGA",
  "NG019021",  "Kano Municipal LGA",
  "NG019022",  "Karaye LGA",
  "NG019023",  "Kibiya LGA",
  # ── KATSINA ──
  "NG020001",  "Bakori LGA",
  "NG020002",  "Batagarawa LGA",
  "NG020003",  "Batsari LGA",
  "NG020004",  "Baure LGA",
  "NG020005",  "Bindawa LGA",
  "NG020006",  "Charanchi LGA",
  "NG020008",  "Dandume LGA",
  "NG020009",  "Danja LGA",
  "NG020010",  "Dan Musa LGA",
  "NG020011",  "Daura LGA",
  "NG020013",  "Dutsi LGA",
  "NG020014",  "Dutsin-Ma LGA",
  "NG020015",  "Faskari LGA",
  "NG020016",  "Funtua LGA",
  "NG020017",  "Ingawa LGA",
  "NG020018",  "Jibia LGA",
  "NG020019",  "Kafur LGA",
  "NG020020",  "Kaita LGA",
  "NG020021",  "Kankara LGA",
  "NG020022",  "Kankia LGA",
  "NG020023",  "Katsina LGA",
  "NG020024",  "Kurfi LGA",
  "NG020025",  "Kusada LGA",
  "NG020026",  "Mai'adua LGA",
  "NG020027",  "Malumfashi LGA",
  "NG020028",  "Mani LGA",
  "NG020029",  "Mashi LGA",
  "NG020030",  "Matazu LGA",
  "NG020032",  "Musawa LGA",
  "NG020033",  "Rimi LGA",
  "NG020034",  "Sabuwa LGA",
  "NG020035",  "Safana LGA",
  "NG020036",  "Sandamu LGA",
  "NG020037",  "Zango LGA",
  "NG020038",  "Ungogo LGA",
  "NG020039",  "Kiru LGA",
  "NG020040",  "Nassarawa LGA",
  "NG020041",  "Madobi LGA",
  "NG020042",  "Shanono LGA",
  "NG020043",  "Warawa LGA",
  "NG020044",  "Wudil LGA"
)

# NG020038 "Ungogo LGA" is a copy-paste error — Ungogo and Gabasawa are Kano
# LGAs, not Katsina. Override below matches 08_ndvi_analysis.R's OCHA-sourced
# pcodes (NG019013 = Ungogo, NG019006 = Gabasawa); not independently verified
# against the shapefile, so treat as a documented best guess.
lga_lookup <- lga_lookup %>%
  mutate(
    lga_name_mchtrack = case_when(
      pcode == "NG019013" ~ "Ungogo LGA",
      pcode == "NG019006" ~ "Gabasawa LGA",
      TRUE                 ~ lga_name_mchtrack
    )
  ) %>%
  filter(pcode != "NG020038")

data_final <- data_kk_monthly %>%
  left_join(lga_lookup, by = "pcode") %>%
  mutate(
    state = case_when(
      str_starts(pcode, "NG019") ~ "Kano",
      str_starts(pcode, "NG020") ~ "Katsina"
    )
  ) %>%
  select(
    state,
    lga_pcode = pcode,
    lga_name_mchtrack,
    year_month,
    precip_actual_mm,
    precip_longterm_avg_mm,
    precip_anomaly_pct,
    precip_abs_dev_mm,
    dekads_present
  ) %>%
  arrange(state, lga_pcode, year_month)

data_final_dekadal <- data_kk_dekadal %>%
  left_join(lga_lookup, by = "pcode") %>%
  mutate(
    state = case_when(
      str_starts(pcode, "NG019") ~ "Kano",
      str_starts(pcode, "NG020") ~ "Katsina"
    )
  ) %>%
  select(
    state,
    lga_pcode = pcode,
    lga_name_mchtrack,
    year_month, dekad_id, dekad_num,
    precip_actual_mm, precip_abs_dev_mm, precip_anomaly_pct
  ) %>%
  arrange(state, lga_pcode, dekad_id)

saveRDS(data_final,         file.path(import_dir, "02_chirps_data_kk_monthly.rds"))
saveRDS(data_final_dekadal, file.path(import_dir, "02_chirps_data_kk_dekadal.rds"))

#----------------------------------------------------------------------------

###############################################
# REGRESSION ANALYSIS — Precipitation, Kano   #
###############################################

# Mirrors 06_era5_analysis.R's structure so heat and precipitation are
# directly comparable. "Daily" panel maps each visit day to the 10-day
# dekad it falls in — a step function, not a true daily series, since
# CHIRPS only resolves to dekads. Genuine resolution limit, not a shortcut.

# 5. Load MCHTrack facility visits -------------------------------------------

data_fv <- readRDS(file.path(mchtrack_dir, "01_facility_visits_clean.rds"))

# Same target sites/window as 06_era5_analysis.R for comparability
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

# 6. Build daily outcome panel -----------------------------------------------

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

# 7. Build monthly outcome panel ----------------------------------------------

visit_monthly <- visit_daily %>%
  mutate(year_month = format(visit_date, "%Y-%m")) %>%
  group_by(lga_clean, year_month) %>%
  summarise(
    n_visits          = sum(n_visits),
    n_unique_children = sum(n_unique_children, na.rm = TRUE),
    n_days            = n(),
    .groups = "drop"
  )

#----------------------------------------------------------------------------

# 8. Prepare CHIRPS series for join -------------------------------------------

precip_dekadal <- data_final_dekadal %>%
  filter(state == "Kano", lga_name_mchtrack %in% c("Ungogo LGA", "Gabasawa LGA")) %>%
  mutate(
    lga_clean = str_to_title(str_remove(lga_name_mchtrack, regex("\\s*lga\\s*$", ignore_case = TRUE)))
  ) %>%
  select(lga_clean, dekad_id, dekad_num, year_month,
         precip_actual_mm, precip_abs_dev_mm, precip_anomaly_pct)

precip_monthly <- data_final %>%
  filter(state == "Kano", lga_name_mchtrack %in% c("Ungogo LGA", "Gabasawa LGA")) %>%
  mutate(
    lga_clean = str_to_title(str_remove(lga_name_mchtrack, regex("\\s*lga\\s*$", ignore_case = TRUE)))
  ) %>%
  select(lga_clean, year_month, precip_actual_mm, precip_longterm_avg_mm,
         precip_anomaly_pct, precip_abs_dev_mm)

#----------------------------------------------------------------------------

# 9. Build "daily" panel — visit day mapped to its containing dekad ----------

panel_daily <- visit_daily %>%
  mutate(
    dekad_num = case_when(
      day(visit_date) <= 10 ~ "D1",
      day(visit_date) <= 20 ~ "D2",
      TRUE                  ~ "D3"
    ),
    year_month = format(visit_date, "%Y-%m"),
    dekad_id   = paste0(year_month, "-", dekad_num)
  ) %>%
  left_join(
    precip_dekadal %>% select(lga_clean, dekad_id, precip_actual_mm,
                              precip_abs_dev_mm, precip_anomaly_pct),
    by = c("lga_clean", "dekad_id")
  ) %>%
  mutate(
    log_visits = log(n_visits + 1),
    dow_num    = wday(visit_date),
    weekend    = dow_num %in% c(1, 7),
    ym_factor  = as.factor(year_month)
  ) %>%
  filter(!is.na(precip_anomaly_pct)) %>%
  arrange(lga_clean, visit_date)

# 10. Build monthly panel ------------------------------------------------------

panel_monthly <- visit_monthly %>%
  left_join(precip_monthly, by = c("lga_clean", "year_month")) %>%
  mutate(
    log_visits = log(n_visits + 1)
  ) %>%
  filter(!is.na(precip_anomaly_pct)) %>%
  arrange(lga_clean, year_month)

#----------------------------------------------------------------------------

###################################
# 11. Regression models           #
###################################

# Outcome log(visits + 1), matching heat/NDVI. Monthly = W1 slot (LGA FE
# only), dekad-derived daily = W2 slot (LGA + DOW + month-year FE).

p1 <- feols(log_visits ~ precip_anomaly_pct | lga_clean,
            data    = panel_monthly,
            cluster = ~lga_clean)

p2 <- feols(log_visits ~ precip_anomaly_pct | lga_clean,
            data    = panel_daily,
            cluster = ~lga_clean)

p3 <- feols(log_visits ~ precip_anomaly_pct | lga_clean + dow_num,
            data    = panel_daily,
            cluster = ~lga_clean)

# P4: reference daily FE structure. The NB version below (Section 12) is
# the primary specification used in the write-up per Prabin's review.
p4 <- feols(log_visits ~ precip_anomaly_pct | lga_clean + dow_num + ym_factor,
            data    = panel_daily,
            cluster = ~lga_clean)

p5 <- feols(log_visits ~ precip_abs_dev_mm | lga_clean + dow_num + ym_factor,
            data    = panel_daily,
            cluster = ~lga_clean)

etable(p1, p2, p3, p4, p5,
       title    = "CHIRPS precipitation and facility visits · Kano",
       digits   = 3,
       se.below = TRUE)

modelsummary(
  list(
    "P1: Monthly, LGA FE"        = p1,
    "P2: Daily, LGA FE"          = p2,
    "P3: +DOW FE"                = p3,
    "P4: +Month-year FE"         = p4,
    "P5: Abs. deviation (mm)"    = p5
  ),
  stars   = c("*" = 0.1, "**" = 0.05, "***" = 0.01),
  gof_map = c("nobs", "r.squared", "adj.r.squared"),
  title   = "Precipitation and facility visits — Kano Aug 2024 – Mar 2026",
  output  = file.path(out_dir, "02_regression_precip_visits.txt")
)

#----------------------------------------------------------------------------

###################################
# 12. Negative binomial — primary #
###################################

# NB is the primary specification per Prabin's review: avoids the log(+1)
# patch, can't predict negative counts, and fits overdispersed count data
# (confirmed in 06_era5_analysis.R, Figure 2.3) better than OLS residuals
# can. OLS retained alongside for comparison.
# Column order flipped vs the previous draft — NB is now col 1, OLS col 2.
# 10_visualizations.R's parser needs updating to match.

p4_nb <- fenegbin(n_visits ~ precip_anomaly_pct | lga_clean + dow_num + ym_factor,
                  data    = panel_daily,
                  cluster = ~lga_clean)

etable(p4_nb, p4,
       title  = "Negative binomial (primary) vs OLS log-visits — precipitation",
       digits = 4, se.below = TRUE)

modelsummary(
  list("Negative binomial — counts (primary)" = p4_nb, "OLS — log(visits + 1)" = p4),
  stars   = c("*" = 0.1, "**" = 0.05, "***" = 0.01),
  gof_map = c("nobs", "r.squared"),
  title   = "Precipitation — negative binomial (primary) vs OLS",
  output  = file.path(out_dir, "02_regression_precip_nb_comparison.txt")
)

#----------------------------------------------------------------------------

#############################################
# 12b. Exposure/offset + spline robustness  #
# (Prabin Dahal, 15/7/2026 review)          #
#############################################

# Offset uses enrolled_children (01_mchtrack_import.R, Section 7) as the
# exposure denominator — a cumulative registration stock, not a true
# point-in-time count. Best available proxy, not a perfect measure.
# Spline tests for a non-linear precip_anomaly_pct relationship.
# Saved to its own file — 10_visualizations.R reads fixed column positions
# from 02_regression_precip_visits.txt and 02_regression_precip_nb_comparison.txt,
# so these models stay out of both.

lga_month_path <- file.path(mchtrack_dir, "01_panel_lga_month.rds")

if (file.exists(lga_month_path)) {
  
  enrolled_lookup <- readRDS(lga_month_path) %>%
    filter(state == "Kano") %>%
    mutate(
      lga_clean = str_to_title(str_remove(lga_name, regex("\\s*lga\\s*$", ignore_case = TRUE)))
    ) %>%
    filter(lga_clean %in% c("Ungogo", "Gabasawa")) %>%
    distinct(lga_clean, year_month, enrolled_children)
  
  # Joined onto copies — panel_daily/panel_monthly and their Section 14
  # exports stay untouched regardless of how this join turns out.
  panel_daily_off <- panel_daily %>%
    left_join(enrolled_lookup, by = c("lga_clean", "year_month"))
  
  panel_monthly_off <- panel_monthly %>%
    left_join(enrolled_lookup, by = c("lga_clean", "year_month"))
  
  panel_daily_off_valid   <- panel_daily_off   %>% filter(!is.na(enrolled_children), enrolled_children > 0)
  panel_monthly_off_valid <- panel_monthly_off %>% filter(!is.na(enrolled_children), enrolled_children > 0)
  
  p1_off <- feols(log_visits ~ precip_anomaly_pct | lga_clean,
                  data    = panel_monthly_off_valid,
                  offset  = ~log(enrolled_children),
                  cluster = ~lga_clean)
  
  p4_off <- feols(log_visits ~ precip_anomaly_pct | lga_clean + dow_num + ym_factor,
                  data    = panel_daily_off_valid,
                  offset  = ~log(enrolled_children),
                  cluster = ~lga_clean)
  
  p4_nb_off <- fenegbin(n_visits ~ precip_anomaly_pct | lga_clean + dow_num + ym_factor,
                        data    = panel_daily_off_valid,
                        offset  = ~log(enrolled_children),
                        cluster = ~lga_clean)
  
  # No offset on the spline — isolates the linearity question from the
  # exposure question.
  p4_spline <- feols(log_visits ~ splines::ns(precip_anomaly_pct, df = 3) | lga_clean + dow_num + ym_factor,
                     data    = panel_daily,
                     cluster = ~lga_clean)
  
  etable(p4, p4_off, p4_nb_off, p4_spline,
         title    = "Precipitation robustness — offset and spline specifications",
         digits   = 4,
         se.below = TRUE)
  
  modelsummary(
    list(
      "P1_off: Monthly, LGA FE, offset"     = p1_off,
      "P4_off: Daily, primary FE, offset"   = p4_off,
      "P4_nb_off: NB, primary FE, offset"   = p4_nb_off,
      "P4_spline: Daily, primary FE, spline (no offset)" = p4_spline
    ),
    stars   = c("*" = 0.1, "**" = 0.05, "***" = 0.01),
    gof_map = c("nobs", "r.squared"),
    title   = "Precipitation — exposure-offset and spline robustness (Prabin, 15/7/2026)",
    output  = file.path(out_dir, "02_regression_precip_robustness_prabin.txt")
  )
}

#----------------------------------------------------------------------------

###################################
# 13. Visualisations              #
###################################

pal <- c("Ungogo" = "#D84A38", "Gabasawa" = "#1D6FA4")

v1 <- ggplot(panel_daily, aes(x = precip_anomaly_pct, y = log_visits,
                              colour = lga_clean)) +
  geom_point(alpha = 0.15, size = 1) +
  geom_smooth(method = "lm", se = TRUE, linewidth = 1) +
  scale_colour_manual(values = pal) +
  labs(
    title   = "Precipitation anomaly vs facility visits — raw relationship",
    x       = "Precipitation anomaly (% of long-term average)",
    y       = "Log facility visits",
    colour  = NULL,
    caption = "Each point = one LGA-day, precipitation held constant within each 10-day dekad"
  ) +
  theme_minimal(base_size = 12) +
  theme(plot.title      = element_text(face = "bold"),
        legend.position = "top")

v2 <- ggplot(panel_daily, aes(x = visit_date)) +
  geom_line(aes(y = n_visits, colour = lga_clean),
            alpha = 0.6, linewidth = 0.4) +
  geom_smooth(aes(y = n_visits, colour = lga_clean),
              method = "loess", span = 0.08,
              se = FALSE, linewidth = 1) +
  scale_colour_manual(values = pal) +
  scale_x_date(date_breaks = "2 months", date_labels = "%b %Y") +
  labs(
    title    = "Daily facility visits across programme window",
    subtitle = "Same panel as Figure 3.8/06_era5_analysis.R, precipitation exposure",
    x        = NULL, y = "Visits per day", colour = NULL
  ) +
  theme_minimal(base_size = 12) +
  theme(plot.title      = element_text(face = "bold"),
        legend.position = "top",
        axis.text.x     = element_text(angle = 30, hjust = 1))

v3 <- ggplot(panel_monthly, aes(x = precip_anomaly_pct, y = n_visits,
                                colour = lga_clean, label = year_month)) +
  geom_point(size = 2.5) +
  geom_smooth(method = "lm", se = TRUE, linewidth = 0.9) +
  scale_colour_manual(values = pal) +
  labs(
    title    = "Monthly precipitation anomaly vs monthly visits",
    subtitle = "P1 specification · small-N monthly panel",
    x        = "Precipitation anomaly (% of long-term average)",
    y        = "Monthly visits", colour = NULL
  ) +
  theme_minimal(base_size = 12) +
  theme(plot.title      = element_text(face = "bold"),
        legend.position = "top")

combined <- (v1 + v3) / v2 +
  plot_annotation(
    title    = "CHIRPS precipitation and facility visits",
    subtitle = "Ungogo & Gabasawa LGAs · Kano · Aug 2024 – Mar 2026",
    theme    = theme(
      plot.title    = element_text(face = "bold", size = 14),
      plot.subtitle = element_text(size = 11)
    )
  )

ggsave(
  filename = file.path(out_dir, "02_precip_visits_panel.png"),
  plot     = combined,
  width    = 14, height = 10, dpi = 300
)

#----------------------------------------------------------------------------

###################################
# 14. Save panels                 #
###################################

saveRDS(panel_daily,   file.path(out_dir, "02_panel_daily.rds"))
saveRDS(panel_monthly, file.path(out_dir, "02_panel_monthly.rds"))
write_csv(panel_daily,   file.path(out_dir, "02_panel_daily.csv"))
write_csv(panel_monthly, file.path(out_dir, "02_panel_monthly.csv"))

#--------------------------(END)------------------------------#
