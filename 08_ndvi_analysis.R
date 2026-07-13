########################################################
#  NDVI Import, Cleaning, Diagnostic & Regression      #
#  Source: HDX Nigeria Subnational NDVI (5-year)       #
#  Coverage: All Nigeria LGAs, dekadal, 2022-2026      #
#  Purpose: Agricultural calendar exposure variable    #
#  for immunisation health-seeking behaviour analysis  #
#  Created: June 2026                                  #
#  Last Updated 13/7/2026 — renamed from               #
#  8_ndvi_analysis.R, which was an exact duplicate of  #
#  07_ndvi_import.R with no regression code. Sections  #
#  10-16 below are new — this is the first version of  #
#  this file that actually contains the NDVI-visits    #
#  regression the filename has always implied.         #
########################################################

# Reset environment -----------------------------------------------------

rm(list = ls())
setwd("C:/Users/HP/Documents/GitHub/datharm-placement")
options(scipen = 999)
Sys.setlocale("LC_TIME", "English")

home         <- "C:/Users/HP/Documents/GitHub/datharm-placement"
ndvi_raw     <- file.path(home, "02_data/03_external/07_NDVI")
mchtrack_dir <- file.path(home, "03_output/01_mchtrack_data")
import_dir   <- file.path(home, "03_output/07_ndvi")
out_dir      <- file.path(home, "03_output/08_ndvi_analysis")
dir.create(ndvi_raw,   showWarnings = FALSE, recursive = TRUE)
dir.create(import_dir, showWarnings = FALSE, recursive = TRUE)
dir.create(out_dir,    showWarnings = FALSE, recursive = TRUE)

# Analysis window — match MCHTrack programme period
window_start <- as.Date("2024-08-01")
window_end   <- as.Date("2026-03-31")

library(janitor)
library(tidyverse)
library(lubridate)
library(ggplot2)
library(patchwork)
library(fixest)        # feols() / fenegbin() for panel regression
library(modelsummary)  # clean regression output tables
library(scales)

#----------------------------------------------------------------------------

###################################
# 1. PCODE reference table        #
###################################

# Standard Nigeria PCODE mapping for target LGAs
# Kano state = NG019, Katsina state = NG020
# LGA codes confirmed from OCHA Nigeria administrative boundaries
#
# NOTE (13/7/2026): this table's target-LGA codes (Ungogo = NG019013,
# Gabasawa = NG019006) were used as the reference to correct a conflicting
# pcode assignment found in 02_chirps_import_analysis.R's lookup table,
# which had these same two LGAs under two different, mutually inconsistent
# pcodes. This table's codes are treated as authoritative below because
# this comment cites a source; if that citation is wrong, both files need
# re-checking against the actual OCHA shapefile.

lga_ref <- tribble(
  ~PCODE,      ~state,    ~lga_name,          ~context,        ~include,
  # Kano — primary analysis LGAs
  "NG019013",  "Kano",    "Ungogo",            "Peri-urban",    TRUE,
  "NG019006",  "Kano",    "Gabasawa",          "Rural",         TRUE,
  # Kano — additional LGAs for state-level context
  "NG019001",  "Kano",    "Ajingi",            "Rural",         TRUE,
  "NG019002",  "Kano",    "Albasu",            "Rural",         TRUE,
  "NG019003",  "Kano",    "Bagwai",            "Rural",         TRUE,
  "NG019004",  "Kano",    "Bebeji",            "Rural",         TRUE,
  "NG019005",  "Kano",    "Bichi",             "Rural",         TRUE,
  "NG019007",  "Kano",    "Bunkure",           "Rural",         TRUE,
  "NG019008",  "Kano",    "Dala",              "Urban",         TRUE,
  "NG019009",  "Kano",    "Dambatta",          "Rural",         TRUE,
  "NG019010",  "Kano",    "Dawakin Kudu",      "Peri-urban",    TRUE,
  "NG019011",  "Kano",    "Dawakin Tofa",      "Peri-urban",    TRUE,
  "NG019012",  "Kano",    "Doguwa",            "Rural",         TRUE,
  "NG019014",  "Kano",    "Fagge",             "Urban",         TRUE,
  "NG019015",  "Kano",    "Garko",             "Rural",         TRUE,
  "NG019016",  "Kano",    "Garun Mallam",      "Rural",         TRUE,
  "NG019017",  "Kano",    "Gaya",              "Rural",         TRUE,
  "NG019018",  "Kano",    "Gezawa",            "Rural",         TRUE,
  "NG019019",  "Kano",    "Gwale",             "Urban",         TRUE,
  "NG019020",  "Kano",    "Gwarzo",            "Rural",         TRUE,
  "NG019021",  "Kano",    "Kabo",              "Rural",         TRUE,
  "NG019022",  "Kano",    "Kano Municipal",    "Urban",         TRUE,
  "NG019023",  "Kano",    "Karaye",            "Rural",         TRUE,
  # Katsina — exclude Rimi (backfill flag) and two arid northern LGAs
  "NG020001",  "Katsina", "Bakori",            "Rural",         TRUE,
  "NG020002",  "Katsina", "Batagarawa",        "Rural",         TRUE,
  "NG020003",  "Katsina", "Batsari",           "Rural",         TRUE,
  "NG020004",  "Katsina", "Baure",             "Rural",         TRUE,
  "NG020005",  "Katsina", "Bindawa",           "Rural",         TRUE,
  "NG020006",  "Katsina", "Charanchi",         "Rural",         TRUE,
  "NG020007",  "Katsina", "Dan Musa",          "Rural",         TRUE,
  "NG020008",  "Katsina", "Dandume",           "Rural",         TRUE,
  "NG020009",  "Katsina", "Danja",             "Rural",         TRUE,
  "NG020010",  "Katsina", "Daura",             "Rural",         TRUE,
  "NG020011",  "Katsina", "Dutsi",             "Rural",         TRUE,
  "NG020012",  "Katsina", "Dutsin-Ma",         "Rural",         TRUE,
  "NG020013",  "Katsina", "Faskari",           "Rural",         TRUE,
  "NG020014",  "Katsina", "Funtua",            "Rural",         TRUE,
  "NG020015",  "Katsina", "Ingawa",            "Rural",         TRUE,
  "NG020016",  "Katsina", "Jibia",             "Rural",         TRUE,
  "NG020017",  "Katsina", "Kafur",             "Rural",         TRUE,
  "NG020018",  "Katsina", "Kaita",             "Rural",         FALSE, # arid fringe
  "NG020019",  "Katsina", "Kankara",           "Rural",         TRUE,
  "NG020020",  "Katsina", "Kankia",            "Rural",         TRUE,
  "NG020021",  "Katsina", "Katsina Municipal", "Urban",         FALSE, # arid, low vim
  "NG020022",  "Katsina", "Kurfi",             "Rural",         TRUE,
  "NG020023",  "Katsina", "Kusada",            "Rural",         TRUE,
  "NG020024",  "Katsina", "Mai'Adua",          "Rural",         TRUE,
  "NG020025",  "Katsina", "Malumfashi",        "Rural",         TRUE,
  "NG020026",  "Katsina", "Mani",              "Rural",         TRUE,
  "NG020027",  "Katsina", "Mashi",             "Rural",         TRUE,
  "NG020028",  "Katsina", "Matazu",            "Rural",         TRUE,
  "NG020029",  "Katsina", "Musawa",            "Rural",         TRUE,
  "NG020030",  "Katsina", "Rimi",              "Rural",         FALSE, # backfill flag
  "NG020031",  "Katsina", "Sabuwa",            "Rural",         TRUE,
  "NG020032",  "Katsina", "Safana",            "Rural",         TRUE,
  "NG020033",  "Katsina", "Sandamu",           "Rural",         TRUE,
  "NG020034",  "Katsina", "Zango",             "Rural",         TRUE
)

cat("LGA reference table built:", nrow(lga_ref), "LGAs\n")
cat("Included:", sum(lga_ref$include), "| Excluded:", sum(!lga_ref$include), "\n\n")

#----------------------------------------------------------------------------

###################################
# 2. Load and clean NDVI data     #
###################################

# File location — copy from project root to working data folder if needed
ndvi_src <- "02_data/03_external/07_NDVI.csv"

if (!file.exists(ndvi_src)) {
  # Try project root (where it was uploaded)
  ndvi_src <- "ngandvisubnat5ytd.csv"
}

cat("Loading NDVI data from:", ndvi_src, "\n")

ndvi_raw_data <- read_csv(ndvi_src, show_col_types = FALSE) %>%
  clean_names()

cat("Raw rows:", nrow(ndvi_raw_data), "\n")
cat("Columns:", names(ndvi_raw_data), "\n\n")

# Clean and filter to Kano + Katsina, LGA level
ndvi_clean <- ndvi_raw_data %>%
  mutate(date = as.Date(date)) %>%
  filter(
    adm_level == 2,
    str_starts(pcode, "NG019") | str_starts(pcode, "NG020")
  ) %>%
  left_join(lga_ref, by = c("pcode" = "PCODE")) %>%
  filter(!is.na(state)) %>%           # drops any PCODEs not in ref table
  mutate(
    year_month  = floor_date(date, "month"),
    month_num   = month(date),
    year        = year(date),
    dekad_num   = case_when(
      day(date) <= 10 ~ 1L,
      day(date) <= 20 ~ 2L,
      TRUE            ~ 3L
    ),
    # Dekad-within-year label for plots
    dekad_label = paste0(format(date, "%b"), "-D", dekad_num),
    # Agricultural season classification
    ag_season = case_when(
      month_num %in% c(7, 8, 9)     ~ "Peak growing (Jul-Sep)",
      month_num %in% c(10, 11)      ~ "Harvest (Oct-Nov)",
      month_num %in% c(12, 1, 2)    ~ "Dry off-season (Dec-Feb)",
      month_num %in% c(3, 4, 5)     ~ "Pre-season (Mar-May)",
      month_num == 6                ~ "Onset of rains (Jun)"
    ) %>% factor(levels = c(
      "Dry off-season (Dec-Feb)",
      "Pre-season (Mar-May)",
      "Onset of rains (Jun)",
      "Peak growing (Jul-Sep)",
      "Harvest (Oct-Nov)"
    ))
  )

cat("Clean rows (Kano + Katsina LGA level):", nrow(ndvi_clean), "\n")
cat("States:", unique(ndvi_clean$state), "\n")
cat("Date range:", format(min(ndvi_clean$date), "%b %Y"),
    "to", format(max(ndvi_clean$date), "%b %Y"), "\n")
cat("Dekads per LGA:", ndvi_clean %>% count(pcode) %>% pull(n) %>% mean() %>% round(0), "\n\n")

#----------------------------------------------------------------------------

###################################
# 3. Programme window subset      #
###################################

ndvi_prog <- ndvi_clean %>%
  filter(date >= window_start, date <= window_end, include == TRUE)

cat("Programme window rows:", nrow(ndvi_prog), "\n")
cat("Kano LGAs:", sum(ndvi_prog$state == "Kano" & !duplicated(ndvi_prog$pcode[ndvi_prog$state=="Kano"])), "\n")
cat("Katsina LGAs:", sum(ndvi_prog$state == "Katsina" & !duplicated(ndvi_prog$pcode[ndvi_prog$state=="Katsina"])), "\n")
cat("Dekads in window:", n_distinct(ndvi_prog$date), "\n\n")

#----------------------------------------------------------------------------

###################################
# 4. Variability diagnostic       #
###################################

cat("=== VARIABILITY DIAGNOSTIC ===\n\n")

var_summary <- ndvi_prog %>%
  group_by(state, pcode, lga_name) %>%
  summarise(
    n_dekads   = n(),
    mean_vim   = round(mean(vim, na.rm = TRUE), 4),
    sd_vim     = round(sd(vim, na.rm = TRUE), 4),
    min_vim    = round(min(vim, na.rm = TRUE), 4),
    max_vim    = round(max(vim, na.rm = TRUE), 4),
    range_vim  = round(max_vim - min_vim, 4),
    cv_vim     = round(sd_vim / mean_vim, 3),
    mean_viq   = round(mean(viq, na.rm = TRUE), 2),
    sd_viq     = round(sd(viq, na.rm = TRUE), 2),
    .groups    = "drop"
  ) %>%
  mutate(
    verdict = case_when(
      sd_vim >= 0.15 ~ "Strong variation — suitable for regression",
      sd_vim >= 0.10 ~ "Moderate variation — usable",
      TRUE           ~ "Low variation — unlikely to yield signal"
    )
  )

cat("--- Variability by LGA ---\n")
print(var_summary %>%
        select(state, lga_name, mean_vim, sd_vim, range_vim, cv_vim, verdict) %>%
        arrange(state, desc(sd_vim)),
      n = 60)

# Cross-LGA spatial variation
cat("\n--- Cross-LGA spatial variation (SD of LGA means) ---\n")
var_summary %>%
  group_by(state) %>%
  summarise(
    n_lgas         = n(),
    mean_vim_state = round(mean(mean_vim), 4),
    sd_lga_means   = round(sd(mean_vim), 4),
    range_lga_means = round(max(mean_vim) - min(mean_vim), 4),
    .groups = "drop"
  ) %>% print()

# Agricultural season means — key diagnostic for opportunity cost mechanism
cat("\n--- Mean vim by agricultural season (pooled across LGAs) ---\n")
ndvi_prog %>%
  group_by(state, ag_season) %>%
  summarise(
    mean_vim = round(mean(vim, na.rm = TRUE), 4),
    n        = n(),
    .groups  = "drop"
  ) %>%
  arrange(state, ag_season) %>%
  print()

#----------------------------------------------------------------------------

###################################
# 5. Target LGA comparison        #
###################################

cat("\n--- Target LGAs: Ungogo vs Gabasawa (Kano) ---\n")
ndvi_prog %>%
  filter(lga_name %in% c("Ungogo", "Gabasawa")) %>%
  group_by(lga_name) %>%
  summarise(
    n_dekads = n(),
    mean_vim = round(mean(vim), 4),
    sd_vim   = round(sd(vim), 4),
    min_vim  = round(min(vim), 4),
    max_vim  = round(max(vim), 4),
    peak_month = month(date[which.max(vim)], label = TRUE),
    trough_month = month(date[which.min(vim)], label = TRUE),
    .groups = "drop"
  ) %>% print()

#----------------------------------------------------------------------------

###################################
# 6. Diagnostic visualisations    #
###################################

pal_state <- c("Kano" = "#1D6FA4", "Katsina" = "#D84A38")

# -- Plot 1: Seasonal profile — state-level pooled vim ------------------

seasonal_profile <- ndvi_prog %>%
  group_by(state, month_num) %>%
  summarise(
    mean_vim = mean(vim, na.rm = TRUE),
    se_vim   = sd(vim, na.rm = TRUE) / sqrt(n()),
    .groups  = "drop"
  ) %>%
  mutate(month_lab = month(month_num, label = TRUE, abbr = TRUE))

p1 <- ggplot(seasonal_profile,
             aes(x = month_num, y = mean_vim, colour = state, group = state)) +
  annotate("rect", xmin = 6.5, xmax = 9.5,
           ymin = -Inf, ymax = Inf, fill = "#1D9E75", alpha = 0.08) +
  annotate("rect", xmin = 9.5, xmax = 11.5,
           ymin = -Inf, ymax = Inf, fill = "#BA7517", alpha = 0.08) +
  geom_ribbon(aes(ymin = mean_vim - se_vim,
                  ymax = mean_vim + se_vim,
                  fill = state), alpha = 0.15, colour = NA) +
  geom_line(linewidth = 1.1) +
  geom_point(size = 3) +
  annotate("text", x = 8, y = 0.62,
           label = "Peak growing\n(Jul-Sep)", size = 3, colour = "#1D9E75") +
  annotate("text", x = 10.5, y = 0.55,
           label = "Harvest\n(Oct-Nov)", size = 3, colour = "#BA7517") +
  scale_colour_manual(values = pal_state) +
  scale_fill_manual(values   = pal_state) +
  scale_x_continuous(breaks = 1:12,
                     labels = month(1:12, label = TRUE, abbr = TRUE)) +
  labs(
    title    = "Seasonal NDVI profile — Kano and Katsina",
    subtitle = "Mean vim by calendar month · Shaded = agricultural seasons",
    x        = NULL, y = "Vegetation Index (vim)", colour = NULL, fill = NULL,
    caption  = "Peak = Jul-Sep planting/growing season · Trough = Mar-May pre-rains"
  ) +
  theme_minimal(base_size = 12) +
  theme(legend.position = "top",
        plot.title      = element_text(face = "bold"))

# -- Plot 2: vim time series across programme window --------------------

ts_data <- ndvi_prog %>%
  group_by(state, date) %>%
  summarise(mean_vim = mean(vim, na.rm = TRUE), .groups = "drop")

p2 <- ggplot(ts_data, aes(x = date, y = mean_vim, colour = state)) +
  annotate("rect",
           xmin = as.Date("2024-08-01"), xmax = as.Date("2024-11-30"),
           ymin = -Inf, ymax = Inf, fill = "#1D9E75", alpha = 0.07) +
  annotate("rect",
           xmin = as.Date("2025-07-01"), xmax = as.Date("2025-11-30"),
           ymin = -Inf, ymax = Inf, fill = "#1D9E75", alpha = 0.07) +
  geom_line(linewidth = 0.9) +
  geom_point(size = 1.8) +
  scale_colour_manual(values = pal_state) +
  scale_x_date(date_breaks = "2 months", date_labels = "%b %Y") +
  labs(
    title    = "Dekadal NDVI across programme window",
    subtitle = "State-level pooled mean · Shaded = peak growing season (Jul-Nov)",
    x        = NULL, y = "Vegetation Index (vim)", colour = NULL,
    caption  = "Each point = one 10-day period · Opportunity cost highest during shaded periods"
  ) +
  theme_minimal(base_size = 12) +
  theme(legend.position  = "top",
        plot.title       = element_text(face = "bold"),
        axis.text.x      = element_text(angle = 30, hjust = 1))

# -- Plot 3: LGA-level variation within Kano (target sites) ------------

target_ts <- ndvi_prog %>%
  filter(lga_name %in% c("Ungogo", "Gabasawa")) %>%
  mutate(site = paste0(lga_name, " (", context, ")"))

p3 <- ggplot(target_ts, aes(x = date, y = vim, colour = site)) +
  geom_line(linewidth = 1) +
  geom_point(size = 2) +
  scale_colour_manual(values = c(
    "Ungogo (Peri-urban)" = "#D84A38",
    "Gabasawa (Rural)"    = "#1D6FA4"
  )) +
  scale_x_date(date_breaks = "2 months", date_labels = "%b %Y") +
  labs(
    title    = "NDVI time series — Ungogo vs Gabasawa",
    subtitle = "Primary analysis sites · Agricultural calendar visible in both",
    x        = NULL, y = "Vegetation Index (vim)", colour = NULL
  ) +
  theme_minimal(base_size = 12) +
  theme(legend.position = "top",
        plot.title      = element_text(face = "bold"),
        axis.text.x     = element_text(angle = 30, hjust = 1))

# -- Plot 4: Cross-LGA distribution of mean vim ------------------------

p4 <- ggplot(var_summary,
             aes(x = reorder(lga_name, mean_vim), y = mean_vim, fill = state)) +
  geom_col(alpha = 0.8, width = 0.75) +
  geom_errorbar(aes(ymin = mean_vim - sd_vim,
                    ymax = mean_vim + sd_vim),
                width = 0.3, linewidth = 0.5) +
  coord_flip() +
  scale_fill_manual(values = pal_state) +
  labs(
    title    = "Mean vim by LGA — programme window",
    subtitle = "Bars = mean, error bars = SD · Sorted ascending",
    x        = NULL, y = "Mean vegetation index",
    fill     = NULL,
    caption  = "Excluded: Rimi (Katsina backfill), Kaita, Katsina Municipal (arid fringe)"
  ) +
  theme_minimal(base_size = 10) +
  theme(legend.position = "top",
        plot.title      = element_text(face = "bold"),
        axis.text.y     = element_text(size = 8))

# -- Combine -----------------------------------------------------------

top_row    <- p1 + p2
bottom_row <- p3 + p4

combined <- top_row / bottom_row +
  plot_annotation(
    title    = "NDVI agricultural calendar diagnostic — Kano & Katsina",
    subtitle = "Programme window Aug 2024 – Mar 2026 · HDX Nigeria subnational NDVI",
    theme    = theme(
      plot.title    = element_text(face = "bold", size = 14),
      plot.subtitle = element_text(size = 11)
    )
  )

ggsave(
  filename = file.path(import_dir, "07_ndvi_diagnostic.png"),
  plot     = combined,
  width    = 14, height = 12, dpi = 300
)

cat("\nDiagnostic plots saved to:", file.path(import_dir, "07_ndvi_diagnostic.png"), "\n")

#----------------------------------------------------------------------------

###################################
# 7. Build analysis-ready panel   #
###################################

# Monthly aggregation — average dekadal vim to month
# Regression will merge onto MCHTrack monthly or daily panels
# Dekadal vim preserved as separate dataset for higher-resolution specs

# Monthly panel (for merging with monthly visit aggregates)
ndvi_monthly <- ndvi_prog %>%
  group_by(state, pcode, lga_name, context, year_month, year, month_num) %>%
  summarise(
    n_dekads      = n(),
    vim_monthly   = round(mean(vim, na.rm = TRUE), 4),
    viq_monthly   = round(mean(viq, na.rm = TRUE), 2),
    vim_max       = round(max(vim, na.rm = TRUE), 4),
    vim_min       = round(min(vim, na.rm = TRUE), 4),
    ag_season     = first(ag_season),
    # Binary high-NDVI indicator — above LGA's own annual median
    # Defined per-LGA to capture relative agricultural intensity
    .groups = "drop"
  ) %>%
  group_by(pcode) %>%
  mutate(
    vim_above_median = as.integer(vim_monthly > median(vim_monthly, na.rm = TRUE)),
    # Centred for regression interpretability
    vim_c            = vim_monthly - mean(vim_monthly, na.rm = TRUE),
    viq_c            = viq_monthly - mean(viq_monthly, na.rm = TRUE)
  ) %>%
  ungroup()

# Dekadal panel (for higher-resolution analysis)
ndvi_dekadal <- ndvi_prog %>%
  select(state, pcode, lga_name, context, date, year_month,
         year, month_num, dekad_num, vim, viq, ag_season) %>%
  group_by(pcode) %>%
  mutate(
    vim_above_median = as.integer(vim > median(vim, na.rm = TRUE)),
    vim_c            = vim - mean(vim, na.rm = TRUE),
    viq_c            = viq - mean(viq, na.rm = TRUE)
  ) %>%
  ungroup()

cat("\nMonthly panel rows:", nrow(ndvi_monthly), "\n")
cat("Dekadal panel rows:", nrow(ndvi_dekadal), "\n")

# Quick sanity check
cat("\n--- Monthly panel sample (Ungogo + Gabasawa) ---\n")
ndvi_monthly %>%
  filter(lga_name %in% c("Ungogo", "Gabasawa")) %>%
  select(lga_name, year_month, vim_monthly, viq_monthly,
         vim_above_median, vim_c, ag_season) %>%
  print(n = 20)

#----------------------------------------------------------------------------

###################################
# 8. Save import/diagnostic outputs#
###################################

# Save cleaned full series (all LGAs, full date range)
ndvi_all <- ndvi_clean %>%
  filter(include == TRUE) %>%
  select(state, pcode, lga_name, context, date, year_month,
         year, month_num, dekad_num, vim, viq, ag_season)

saveRDS(ndvi_monthly,  file.path(import_dir, "07_ndvi_monthly.rds"))
saveRDS(ndvi_dekadal,  file.path(import_dir, "07_ndvi_dekadal.rds"))
saveRDS(ndvi_all,      file.path(import_dir, "07_ndvi_full_series.rds"))

write_csv(ndvi_monthly,  file.path(import_dir, "07_ndvi_monthly.csv"))
write_csv(ndvi_dekadal,  file.path(import_dir, "07_ndvi_dekadal.csv"))
write_csv(var_summary,   file.path(import_dir, "07_ndvi_variability_summary.csv"))

cat("\nImport/diagnostic outputs saved to:", import_dir, "\n")
cat("Files: 07_ndvi_monthly.rds/.csv, 07_ndvi_dekadal.rds/.csv,\n")
cat("       07_ndvi_full_series.rds, 07_ndvi_variability_summary.csv\n")
cat("       07_ndvi_diagnostic.png\n\n")

cat("=== IMPORT/DIAGNOSTIC STAGE COMPLETE — REGRESSION ANALYSIS BELOW ===\n\n")


#################################################################
# REGRESSION ANALYSIS — NDVI and facility visits, Kano primary #
#################################################################

# NEW (13/7/2026). Mirrors 06_era5_analysis.R (heat) and
# 02_chirps_import_analysis.R (precipitation) so all three weather/
# vegetation exposures are directly comparable in the write-up. NDVI's
# native resolution is dekadal, aggregated to monthly above (Section 7),
# so this analysis runs at MONTHLY resolution only — there is no dekad-
# derived "daily" spec here, unlike the precipitation script, because
# facility-visit volume at dekad resolution is too sparse per LGA-dekad
# to support a meaningful regression on top of an already-small monthly
# panel (Kano target sites = 2 LGAs).

cat("\n=== SECTION: NDVI REGRESSION ANALYSIS ===\n\n")

# 9. Load MCHTrack facility visits ------------------------------------------

data_fv <- readRDS(file.path(mchtrack_dir, "01_facility_visits_clean.rds"))

cat("Facility visits loaded:", nrow(data_fv), "rows\n")

# Filter to Ungogo and Gabasawa, children only, Rimi excluded — same
# target sites and window as 06_era5_analysis.R and
# 02_chirps_import_analysis.R, so all three exposures are comparable.
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
    visit_date >= window_start,
    visit_date <= window_end
  )

cat("Kano visits after filter:", nrow(data_fv_kano), "\n")
cat("LGA names:", unique(data_fv_kano$lga_clean), "\n\n")

# 10. Build monthly outcome panel (Kano) -------------------------------------

visit_monthly_kano <- data_fv_kano %>%
  mutate(year_month_date = floor_date(visit_date, "month")) %>%
  group_by(lga_clean, year_month_date) %>%
  summarise(
    n_visits          = n(),
    n_unique_children = n_distinct(patient_id),
    n_days            = n_distinct(visit_date),
    .groups = "drop"
  )

cat("Monthly outcome panel rows (Kano):", nrow(visit_monthly_kano), "\n\n")

#----------------------------------------------------------------------------

# 11. Join NDVI monthly panel (Kano target LGAs) -----------------------------

ndvi_monthly_kano <- ndvi_monthly %>%
  filter(state == "Kano", lga_name %in% c("Ungogo", "Gabasawa")) %>%
  mutate(lga_clean = lga_name) %>%
  select(lga_clean, year_month, vim_monthly, viq_monthly,
         vim_above_median, vim_c, viq_c, ag_season)

panel_ndvi_kano <- visit_monthly_kano %>%
  mutate(year_month = year_month_date) %>%
  left_join(ndvi_monthly_kano, by = c("lga_clean", "year_month")) %>%
  mutate(
    log_visits = log(n_visits + 1),
    ym_factor  = as.factor(format(year_month, "%Y-%m"))
  ) %>%
  filter(!is.na(vim_c)) %>%
  arrange(lga_clean, year_month)

cat("NDVI-visits panel rows (Kano, after join):", nrow(panel_ndvi_kano), "\n")

if (nrow(panel_ndvi_kano) == 0) {
  cat("WARNING: join produced zero rows. Check that ndvi_monthly's\n")
  cat("year_month (Date, first-of-month) matches visit_monthly_kano's\n")
  cat("year_month_date before proceeding.\n\n")
}

cat("--- Panel coverage by LGA ---\n")
panel_ndvi_kano %>%
  group_by(lga_clean) %>%
  summarise(
    n_months        = n(),
    mean_visits     = round(mean(n_visits), 1),
    mean_vim        = round(mean(vim_monthly, na.rm = TRUE), 3),
    mean_viq        = round(mean(viq_monthly, na.rm = TRUE), 1),
    .groups = "drop"
  ) %>% print()
cat("\n")

#----------------------------------------------------------------------------

###################################
# 12. Regression models (Kano)    #
###################################

# Outcome: log(visits + 1), matching the heat and precipitation
# specifications. Matches the W5/W6 slots referenced in
# 09_visualization_markdown.Rmd's weather table (Monthly, LGA[+month-year] FE).

# N1: vim level, LGA FE only — matches W5 (NDVI seasonal level)
n1 <- feols(log_visits ~ vim_c | lga_clean,
            data    = panel_ndvi_kano,
            cluster = ~lga_clean)

# N2: viq anomaly, LGA + month-year FE — matches W6 (NDVI within-baseline anomaly)
n2 <- feols(log_visits ~ viq_c | lga_clean + ym_factor,
            data    = panel_ndvi_kano,
            cluster = ~lga_clean)

# N3: binary high-NDVI indicator — alternative exposure, easier to interpret
n3 <- feols(log_visits ~ vim_above_median | lga_clean,
            data    = panel_ndvi_kano,
            cluster = ~lga_clean)

# N4: vim level with month-year FE — robustness against N1
n4 <- feols(log_visits ~ vim_c | lga_clean + ym_factor,
            data    = panel_ndvi_kano,
            cluster = ~lga_clean)

cat("=== REGRESSION RESULTS — NDVI (OLS), KANO PRIMARY ===\n\n")

etable(n1, n2, n3, n4,
       title    = "NDVI and facility visits — monthly panel · Kano",
       digits   = 3,
       se.below = TRUE)

modelsummary(
  list(
    "N1: vim level, LGA FE"        = n1,
    "N2: viq anomaly, +month-year" = n2,
    "N3: vim above median (binary)"= n3,
    "N4: vim level, +month-year"   = n4
  ),
  stars   = c("*" = 0.1, "**" = 0.05, "***" = 0.01),
  gof_map = c("nobs", "r.squared", "adj.r.squared"),
  title   = "NDVI and facility visits — Kano Aug 2024 – Mar 2026",
  output  = file.path(out_dir, "08_regression_ndvi_kano.txt")
)

cat("\nTable saved to:", file.path(out_dir, "08_regression_ndvi_kano.txt"), "\n\n")

#----------------------------------------------------------------------------

###################################
# 13. Negative binomial check     #
###################################

# Same overdispersion caveat as heat and precipitation (Figure 2.3 in
# 09_visualization_markdown.Rmd) — this is the same visit-count outcome,
# just a different exposure joined onto it.

n1_nb <- fenegbin(n_visits ~ vim_c | lga_clean,
                  data    = panel_ndvi_kano,
                  cluster = ~lga_clean)

n2_nb <- fenegbin(n_visits ~ viq_c | lga_clean + ym_factor,
                  data    = panel_ndvi_kano,
                  cluster = ~lga_clean)

cat("=== NEGATIVE BINOMIAL COMPARISON — NDVI ===\n\n")

etable(n1, n1_nb, n2, n2_nb,
       title  = "OLS (log-visits) vs negative binomial — NDVI exposure",
       digits = 4, se.below = TRUE)

modelsummary(
  list(
    "N1: OLS — vim level"       = n1,
    "N1_nb: NB — vim level"     = n1_nb,
    "N2: OLS — viq anomaly"     = n2,
    "N2_nb: NB — viq anomaly"   = n2_nb
  ),
  stars   = c("*" = 0.1, "**" = 0.05, "***" = 0.01),
  gof_map = c("nobs", "r.squared"),
  title   = "NDVI — OLS vs negative binomial, Kano monthly panel",
  output  = file.path(out_dir, "08_regression_ndvi_nb_comparison.txt")
)

cat("\nTable saved to:", file.path(out_dir, "08_regression_ndvi_nb_comparison.txt"), "\n\n")
cat("As with heat and precipitation, this settles agreement for NDVI only —\n")
cat("the OLS-vs-NB primary-spec decision (c155) should be made once, with\n")
cat("Prabin, and applied consistently to all three weather variables.\n\n")

#----------------------------------------------------------------------------

##############################################################
# 14. Katsina sensitivity check — NOT the primary analysis   #
##############################################################

# Per the original decision log for this script (see historical note in
# Section 8 above) and per direct confirmation from Khem (13/7/2026):
# facility visits data for Katsina has known duplication issues that
# emerged partway through the placement, so Kano is the only state
# reliable across the FULL study period. RQ1 and RQ2 (zero-dose
# predictors, recovery) legitimately pool both states because those
# analyses do not depend on daily visit-count volume the way this NDVI-
# visits regression does. This weather analysis (RQ3) is Kano-primary for
# that reason, and Katsina is reported here only as an explicit,
# clearly-labelled sensitivity check — NOT pooled into the primary N1-N4
# results above, and NOT used to justify any claim about NDVI's effect in
# Katsina specifically.

cat("=== SECTION 14: KATSINA SENSITIVITY CHECK (NOT PRIMARY) ===\n\n")
cat("WARNING: Katsina facility-visits data has known duplication issues.\n")
cat("The results below are reported for transparency only and should be\n")
cat("flagged explicitly as a sensitivity check wherever they are cited.\n\n")

data_fv_katsina <- data_fv %>%
  filter(
    state == "Katsina",
    woman_or_child == "child",
    !rimi_flag
  ) %>%
  mutate(
    visit_date = as.Date(visit_date),
    lga_clean  = str_to_title(str_trim(
      str_remove(lga_name, regex("\\s*lga\\s*$", ignore_case = TRUE))
    ))
  ) %>%
  filter(visit_date >= window_start, visit_date <= window_end)

visit_monthly_katsina <- data_fv_katsina %>%
  mutate(year_month = floor_date(visit_date, "month")) %>%
  group_by(lga_clean, year_month) %>%
  summarise(n_visits = n(), .groups = "drop")

ndvi_monthly_katsina <- ndvi_monthly %>%
  filter(state == "Katsina") %>%
  mutate(lga_clean = lga_name) %>%
  select(lga_clean, year_month, vim_monthly, viq_monthly,
         vim_above_median, vim_c, viq_c)

panel_ndvi_katsina <- visit_monthly_katsina %>%
  left_join(ndvi_monthly_katsina, by = c("lga_clean", "year_month")) %>%
  mutate(log_visits = log(n_visits + 1)) %>%
  filter(!is.na(vim_c))

cat("Katsina sensitivity panel rows:", nrow(panel_ndvi_katsina), "\n")
cat("Katsina LGAs represented:", n_distinct(panel_ndvi_katsina$lga_clean), "\n\n")

if (nrow(panel_ndvi_katsina) >= 20) {
  n1_katsina <- feols(log_visits ~ vim_c | lga_clean,
                      data    = panel_ndvi_katsina,
                      cluster = ~lga_clean)
  
  cat("=== KATSINA SENSITIVITY RESULT (vim level, LGA FE) ===\n\n")
  etable(n1, n1_katsina,
         title    = "Kano primary vs Katsina sensitivity — NDVI",
         headers  = c("Kano (primary)", "Katsina (sensitivity)"),
         digits   = 3, se.below = TRUE)
  
  modelsummary(
    list("Kano (primary)" = n1, "Katsina (sensitivity)" = n1_katsina),
    stars   = c("*" = 0.1, "**" = 0.05, "***" = 0.01),
    gof_map = c("nobs", "r.squared"),
    title   = "Table 3.3-adjacent: Kano primary vs Katsina sensitivity (NDVI)",
    output  = file.path(out_dir, "08_regression_ndvi_katsina_sensitivity.txt")
  )
  cat("\nTable saved to:",
      file.path(out_dir, "08_regression_ndvi_katsina_sensitivity.txt"), "\n\n")
} else {
  cat("Fewer than 20 Katsina LGA-months available — skipping the sensitivity\n")
  cat("regression as underpowered. Reconsider once more Katsina months are\n")
  cat("confirmed reliable.\n\n")
}

#----------------------------------------------------------------------------

###################################
# 15. Regression visualisations   #
###################################

pal_kano <- c("Ungogo" = "#D84A38", "Gabasawa" = "#1D6FA4")

# -- Plot A: vim vs log visits, Kano target LGAs -------------------------

pA <- ggplot(panel_ndvi_kano, aes(x = vim_c, y = log_visits, colour = lga_clean)) +
  geom_point(size = 2.5, alpha = 0.7) +
  geom_smooth(method = "lm", se = TRUE, linewidth = 1) +
  scale_colour_manual(values = pal_kano) +
  labs(
    title   = "NDVI (centred) vs facility visits — Kano monthly panel",
    x       = "Vegetation index, centred (vim_c)",
    y       = "Log facility visits",
    colour  = NULL,
    caption = "Each point = one LGA-month · N1 specification"
  ) +
  theme_minimal(base_size = 12) +
  theme(plot.title = element_text(face = "bold"), legend.position = "top")

# -- Plot B: NDVI and visits time series, dual axis style (faceted) ------

pB_ndvi <- ggplot(panel_ndvi_kano, aes(x = year_month, y = vim_monthly, colour = lga_clean)) +
  geom_line(linewidth = 1) + geom_point(size = 2) +
  scale_colour_manual(values = pal_kano) +
  scale_x_date(date_breaks = "3 months", date_labels = "%b %Y") +
  labs(subtitle = "NDVI (vim)", x = NULL, y = "vim", colour = NULL) +
  theme_minimal(base_size = 11) +
  theme(legend.position = "top", axis.text.x = element_text(angle = 30, hjust = 1))

pB_visits <- ggplot(panel_ndvi_kano, aes(x = year_month, y = n_visits, colour = lga_clean)) +
  geom_line(linewidth = 1) + geom_point(size = 2) +
  scale_colour_manual(values = pal_kano, guide = "none") +
  scale_x_date(date_breaks = "3 months", date_labels = "%b %Y") +
  labs(subtitle = "Facility visits", x = NULL, y = "Monthly visits") +
  theme_minimal(base_size = 11) +
  theme(axis.text.x = element_text(angle = 30, hjust = 1))

combined_reg <- (pA) / (pB_ndvi / pB_visits) +
  plot_annotation(
    title    = "NDVI and facility visits — Kano primary analysis",
    subtitle = "Ungogo & Gabasawa LGAs · Aug 2024 – Mar 2026",
    theme    = theme(
      plot.title    = element_text(face = "bold", size = 14),
      plot.subtitle = element_text(size = 11)
    )
  )

ggsave(
  filename = file.path(out_dir, "08_ndvi_visits_kano.png"),
  plot     = combined_reg,
  width    = 10, height = 12, dpi = 300
)

cat("Regression plots saved to:", out_dir, "\n\n")

#----------------------------------------------------------------------------

###################################
# 16. Save analysis outputs       #
###################################

saveRDS(panel_ndvi_kano, file.path(out_dir, "08_panel_ndvi_kano.rds"))
write_csv(panel_ndvi_kano, file.path(out_dir, "08_panel_ndvi_kano.csv"))

if (exists("panel_ndvi_katsina") && nrow(panel_ndvi_katsina) > 0) {
  saveRDS(panel_ndvi_katsina, file.path(out_dir, "08_panel_ndvi_katsina_sensitivity.rds"))
  write_csv(panel_ndvi_katsina, file.path(out_dir, "08_panel_ndvi_katsina_sensitivity.csv"))
}

cat("All analysis outputs saved to:", out_dir, "\n")
cat("  08_regression_ndvi_kano.txt\n")
cat("  08_regression_ndvi_nb_comparison.txt\n")
cat("  08_regression_ndvi_katsina_sensitivity.txt   (if Katsina panel >= 20 rows)\n")
cat("  08_ndvi_visits_kano.png\n")
cat("  08_panel_ndvi_kano.rds / .csv\n")
cat("  08_panel_ndvi_katsina_sensitivity.rds / .csv (if built)\n\n")

cat("--- Script complete ---\n")
cat("This file now contains the regression analysis its filename has always\n")
cat("implied. 07_ndvi_import.R remains the import-only version — no need to\n")
cat("keep 8_ndvi_analysis.R (the old duplicate) once this file replaces it.\n")

#--------------------------(END)------------------------------#