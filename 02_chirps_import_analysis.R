########################################################
#  CHIRPS Precipitation — Import, Clean & Regression   #
#  Outcome: Daily/monthly facility visit counts, Kano  #
#  Exposure: CHIRPS precipitation anomaly              #
#  Sites: Ungogo & Gabasawa LGAs                       #
#  Created on 13/5/2026                                #
#  Last Updated 13/7/2026 — added analysis component,  #
#  renamed from 02_chirps_import.R                     #
########################################################

# Reset environment -----------------------------------------------------

rm(list = ls())
setwd("C:/Users/HP/Documents/GitHub/datharm-placement")

# Turn off scientific notation globally
options(scipen = 999)
Sys.setlocale("LC_TIME", "English")

#Set link shortcuts
home         <- "C:/Users/HP/Documents/GitHub/datharm-placement"
raw_dir      <- file.path(home, "02_data/03_external")
mchtrack_dir <- file.path(home, "03_output/01_mchtrack_data")
import_dir   <- file.path(home, "03_output/02_chirps_data")
out_dir      <- file.path(home, "03_output/02_chirps_analysis")
dir.create(import_dir, showWarnings = FALSE, recursive = TRUE)
dir.create(out_dir,    showWarnings = FALSE, recursive = TRUE)

#For working with rasters
install.packages("chirps")
install.packages("sf")
install.packages("terra")

library(sf)
library(terra)
library(chirps)

#Routine Packages
library(janitor)
library(tidyverse)
library(dplyr)
library(tidyr)
library(ggplot2)
library(readr)
library(readxl)
library(lubridate)
library(fixest)        # feols() / fenegbin() for panel regression
library(modelsummary)  # clean regression output tables
library(patchwork)
library(scales)

#----------------------------------------------------------------------------

###################
# Import Database #
###################

# 1. Import rainfall CHIRPS dataset --------------------------------------------

data_file <- file.path(raw_dir, "nga-rainfall-subnat-5ytd.csv")
data_raw  <- read_csv(data_file, show_col_types = FALSE) %>%
  clean_names()


############
# Cleaning #
############

# 2. Filter the imported data to only LGAs in Katsina and Kano -----------------

data_kk <- data_raw %>%
  filter(
    adm_level == 2,
    str_starts(pcode, "NG019") | str_starts(pcode, "NG020")
  )

#Count number of rows and distinct LGA count
nrow(data_kk)
n_distinct(data_kk$pcode)

# 3. Aggregate from dekadal to monthly averages --------------------------------

data_kk_monthly <- data_kk %>%
  mutate(
    year_month = format(as.Date(date), "%Y-%m")
  ) %>%
  group_by(pcode, year_month) %>%
  summarise(
    dekads_present          = n(),                                      # should be 3; flag if not
    precip_actual_mm        = mean(as.numeric(r1h),     na.rm = TRUE),  # 1-month actual rainfall mm
    precip_longterm_avg_mm  = mean(as.numeric(r1h_avg), na.rm = TRUE),  # long-term average mm
    precip_anomaly_pct      = mean(as.numeric(r1q),     na.rm = TRUE),  # 1-month anomaly as % of LTA
    # absolute mm deviation from long-term average — supervisor suggestion
    precip_abs_dev_mm       = mean(as.numeric(r1h) - as.numeric(r1h_avg), na.rm = TRUE),
    .groups = "drop"
  ) %>%
  #Filter to the program date range (MCHTrack data starts Aug 2024)
  filter(year_month >= "2024-08")

# Dekadal dataset — keep one row per pcode × dekad, no aggregation needed
# dekad_id: YYYY-MM-D1 / D2 / D3 based on day of month
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
    # absolute mm deviation from long-term dekadal average
    precip_abs_dev_mm = as.numeric(r1h) - as.numeric(r1h_avg),
    # keep anomaly pct for comparison
    precip_anomaly_pct = as.numeric(r1q)
  ) %>%
  filter(date_parsed >= as.Date("2024-08-01")) %>%
  select(pcode, year_month, dekad_id, dekad_num,
         precip_actual_mm, precip_abs_dev_mm, precip_anomaly_pct)

# Flag any months where we have fewer than 3 dekads (incomplete months)
incomplete <- data_kk_monthly %>% filter(dekads_present < 3)
if (nrow(incomplete) > 0) {
  cat("WARNING:", nrow(incomplete), "LGA-months have fewer than 3 dekads — check dates\n")
  print(incomplete)
} else {
  cat("All LGA-months have complete dekadal coverage.\n")
}


# 4. Add the LGA name variable ----------

#Manually create look up table of pcode and LGA names
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

# ⚠ PCODE VERIFICATION FLAG — DO NOT SKIP -------------------------------
# This lookup assigns "Ungogo LGA" to NG020038, inside the Katsina block
# (pcodes starting NG020 are coded state = "Katsina" below). Ungogo, Kiru,
# Nassarawa, Madobi, Shanono, Warawa and Wudil (NG020038-044) are all real
# Kano LGA names, not Katsina — this looks like a copy/paste continuation
# error when the table was built, not a deliberate Katsina LGA list.
#
# 08_ndvi_analysis.R's lga_ref table (sourced, per its own comment, from
# OCHA Nigeria administrative boundaries) assigns:
#   Ungogo   = NG019013 (Kano)
#   Gabasawa = NG019006 (Kano)
# which conflicts with THIS file's own assignment of NG019013 = Gabasawa
# and NG019006 = Bunkure. That is three different pcode claims across the
# codebase for the same two target LGAs.
#
# The override below matches the OCHA-cited reference in 08_ndvi_analysis.R,
# since it is the only one with an explicit source. VERIFY AGAINST THE
# ACTUAL OCHA SHAPEFILE BEFORE TREATING ANY UNGOGO/GABASAWA PRECIPITATION
# RESULT BELOW AS FINAL — this override is a documented best guess, not a
# confirmed correction.

lga_lookup <- lga_lookup %>%
  mutate(
    lga_name_mchtrack = case_when(
      pcode == "NG019013" ~ "Ungogo LGA",
      pcode == "NG019006" ~ "Gabasawa LGA",
      TRUE                 ~ lga_name_mchtrack
    )
  ) %>%
  filter(pcode != "NG020038")   # drop the erroneous Katsina-block "Ungogo LGA" row

cat("PCODE OVERRIDE APPLIED — verify against OCHA shapefile before final use:\n")
cat("  NG019013 -> Ungogo LGA (Kano)\n")
cat("  NG019006 -> Gabasawa LGA (Kano)\n")
cat("  Removed erroneous NG020038 'Ungogo LGA' entry from Katsina block\n\n")

#Join lookup LGA table onto data
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

# Build dekadal final: join LGA names onto dekadal data
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


#Check for any PCODEs that didn't match the lookup
unmatched <- data_final %>% filter(is.na(lga_name_mchtrack))
if (nrow(unmatched) > 0) {
  cat("WARNING:", n_distinct(unmatched$lga_pcode), "PCODEs had no name match:\n")
  print(distinct(unmatched, lga_pcode))
} else {
  cat("All PCODEs matched successfully.\n")
}


# Save as native R object instead of CSV
saveRDS(data_final,         file.path(import_dir, "02_chirps_data_kk_monthly.rds"))
saveRDS(data_final_dekadal, file.path(import_dir, "02_chirps_data_kk_dekadal.rds"))


# Preview
print(data_final)

#----------------------------------------------------------------------------

###############################################
# REGRESSION ANALYSIS — Precipitation, Kano   #
###############################################

# Mirrors the structure of 06_era5_analysis.R (heat) so the two exposures
# are directly comparable in the write-up. Two resolutions are built:
#   - Monthly panel: matches CHIRPS's native reporting resolution.
#   - "Daily" panel: each visit day is joined to the precipitation value of
#     the 10-day dekad it falls in (same dekad logic as the cleaning step
#     above). This is a step function, not a true daily series — CHIRPS
#     here only resolves to dekads, so within a dekad every day carries an
#     identical precipitation value. State this explicitly in the write-up;
#     it is a genuine resolution limit, not a coding shortcut.

cat("\n=== SECTION: PRECIPITATION REGRESSION ANALYSIS ===\n\n")

# 5. Load MCHTrack facility visits ----------------------------------------

data_fv <- readRDS(file.path(mchtrack_dir, "01_facility_visits_clean.rds"))

cat("Facility visits loaded:", nrow(data_fv), "rows\n")

# Filter to Ungogo and Gabasawa, children only, Rimi excluded — same target
# sites and window as 06_era5_analysis.R, so heat and precipitation results
# are directly comparable.
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

# 6. Build daily outcome panel ---------------------------------------------

visit_daily <- data_fv_kano %>%
  group_by(lga_clean, visit_date) %>%
  summarise(
    n_visits          = n(),
    n_unique_children = n_distinct(patient_id),
    .groups = "drop"
  )

# Full date spine so zero-visit days are not dropped
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

# 7. Build monthly outcome panel --------------------------------------------

visit_monthly <- visit_daily %>%
  mutate(year_month = format(visit_date, "%Y-%m")) %>%
  group_by(lga_clean, year_month) %>%
  summarise(
    n_visits          = sum(n_visits),
    n_unique_children = sum(n_unique_children, na.rm = TRUE),
    n_days            = n(),
    .groups = "drop"
  )

cat("Monthly outcome panel rows:", nrow(visit_monthly), "\n\n")

#----------------------------------------------------------------------------

# 8. Prepare CHIRPS series for join -----------------------------------------

# lga_clean naming must match visit data (Title Case, no "LGA" suffix)
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

cat("Precipitation dekadal rows (target LGAs):", nrow(precip_dekadal), "\n")
cat("Precipitation monthly rows (target LGAs):", nrow(precip_monthly), "\n\n")

if (nrow(precip_dekadal) == 0 || nrow(precip_monthly) == 0) {
  cat("WARNING: no precipitation rows matched Ungogo/Gabasawa after the pcode\n")
  cat("override above. Check the override against the raw CHIRPS pcode set\n")
  cat("before proceeding — the join below will silently produce an empty panel.\n\n")
}

#----------------------------------------------------------------------------

# 9. Build "daily" panel — visit day mapped to its containing dekad --------

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

cat("Daily precip-visit panel rows after join:", nrow(panel_daily), "\n")
cat("Rows with precip matched:", sum(!is.na(panel_daily$precip_anomaly_pct)), "\n\n")

# 10. Build monthly panel ----------------------------------------------------

panel_monthly <- visit_monthly %>%
  left_join(precip_monthly, by = c("lga_clean", "year_month")) %>%
  mutate(
    log_visits = log(n_visits + 1)
  ) %>%
  filter(!is.na(precip_anomaly_pct)) %>%
  arrange(lga_clean, year_month)

cat("Monthly precip-visit panel rows after join:", nrow(panel_monthly), "\n\n")

#Validate: panel coverage summary table ----
cat("--- Panel coverage by LGA ---\n")
panel_daily %>%
  group_by(lga_clean) %>%
  summarise(
    n_days            = n(),
    mean_visits       = round(mean(n_visits), 1),
    mean_precip_mm    = round(mean(precip_actual_mm, na.rm = TRUE), 1),
    mean_anomaly_pct  = round(mean(precip_anomaly_pct, na.rm = TRUE), 1),
    .groups = "drop"
  ) %>% print()
cat("\n")

#----------------------------------------------------------------------------

###################################
# 11. Regression models           #
###################################

# Outcome: log(visits + 1), matching the heat and NDVI specifications
# Monthly spec = W1 slot (small-N panel, LGA FE only)
# Dekad-derived daily spec = W2 slot (LGA + DOW + month-year FE)

# P1: Monthly, LGA FE only — matches heat's D1-equivalent
p1 <- feols(log_visits ~ precip_anomaly_pct | lga_clean,
            data    = panel_monthly,
            cluster = ~lga_clean)

# P2: Daily (dekad-derived), LGA FE only — naive baseline
p2 <- feols(log_visits ~ precip_anomaly_pct | lga_clean,
            data    = panel_daily,
            cluster = ~lga_clean)

# P3: + day-of-week FE
p3 <- feols(log_visits ~ precip_anomaly_pct | lga_clean + dow_num,
            data    = panel_daily,
            cluster = ~lga_clean)

# P4: + month-year FE — primary daily specification
p4 <- feols(log_visits ~ precip_anomaly_pct | lga_clean + dow_num + ym_factor,
            data    = panel_daily,
            cluster = ~lga_clean)

# P5: Absolute mm deviation — alternative exposure measure
p5 <- feols(log_visits ~ precip_abs_dev_mm | lga_clean + dow_num + ym_factor,
            data    = panel_daily,
            cluster = ~lga_clean)

cat("=== REGRESSION RESULTS — PRECIPITATION (OLS) ===\n\n")

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

cat("\nTable saved to:", file.path(out_dir, "02_regression_precip_visits.txt"), "\n\n")

#----------------------------------------------------------------------------

###################################
# 12. Negative binomial check     #
###################################

# Theoretically preferred given the overdispersion already confirmed for
# this outcome in 06_era5_analysis.R (Figure 2.3) — same visit-count
# outcome, same overdispersion, so the same caveat applies here.

p4_nb <- fenegbin(n_visits ~ precip_anomaly_pct | lga_clean + dow_num + ym_factor,
                  data    = panel_daily,
                  cluster = ~lga_clean)

cat("=== NEGATIVE BINOMIAL CHECK — PRECIPITATION ===\n\n")
etable(p4, p4_nb,
       title  = "OLS (log-visits) vs negative binomial — precipitation",
       digits = 4, se.below = TRUE)

modelsummary(
  list("OLS — log(visits + 1)" = p4, "Negative binomial — counts" = p4_nb),
  stars   = c("*" = 0.1, "**" = 0.05, "***" = 0.01),
  gof_map = c("nobs", "r.squared"),
  title   = "Precipitation — OLS vs negative binomial",
  output  = file.path(out_dir, "02_regression_precip_nb_comparison.txt")
)

cat("\nTable saved to:", file.path(out_dir, "02_regression_precip_nb_comparison.txt"), "\n\n")

#----------------------------------------------------------------------------

###################################
# 13. Visualisations              #
###################################

pal <- c("Ungogo" = "#D84A38", "Gabasawa" = "#1D6FA4")

# -- Plot 1: Scatter — precipitation anomaly vs log visits --------------

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

# -- Plot 2: Visits and precipitation time series ------------------------

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

# -- Plot 3: Monthly precip anomaly vs monthly visits, by LGA ------------

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

cat("Plots saved to:", out_dir, "\n\n")

#----------------------------------------------------------------------------

###################################
# 14. Save panels                 #
###################################

saveRDS(panel_daily,   file.path(out_dir, "02_panel_daily.rds"))
saveRDS(panel_monthly, file.path(out_dir, "02_panel_monthly.rds"))
write_csv(panel_daily,   file.path(out_dir, "02_panel_daily.csv"))
write_csv(panel_monthly, file.path(out_dir, "02_panel_monthly.csv"))

cat("Panels saved:", nrow(panel_daily), "LGA-day rows,",
    nrow(panel_monthly), "LGA-month rows\n\n")

cat("All analysis outputs saved to:", out_dir, "\n")
cat("  02_regression_precip_visits.txt\n")
cat("  02_regression_precip_nb_comparison.txt\n")
cat("  02_precip_visits_panel.png\n")
cat("  02_panel_daily.rds / .csv\n")
cat("  02_panel_monthly.rds / .csv\n\n")

cat("--- Script complete ---\n")
cat("Reminder: the pcode override in Section 4 needs independent verification\n")
cat("against the OCHA shapefile before any number above is treated as final.\n")

#--------------------------(END)------------------------------#
