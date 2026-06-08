########################################################
#  NDVI Import, Cleaning & Variability Diagnostic      #
#  Source: HDX Nigeria Subnational NDVI (5-year)       #
#  Coverage: All Nigeria LGAs, dekadal, 2022-2026      #
#  Purpose: Agricultural calendar exposure variable    #
#  for immunisation health-seeking behaviour analysis  #
#  Created: June 2026                                  #
########################################################

# Reset environment -----------------------------------------------------

rm(list = ls())
setwd("C:/Users/HP/Documents/GitHub/datharm-placement")
options(scipen = 999)
Sys.setlocale("LC_TIME", "English")

home     <- "C:/Users/HP/Documents/GitHub/datharm-placement"
ndvi_raw <- file.path(home, "02_data/03_external/07_NDVI")
out_dir  <- file.path(home, "03_output/07_ndvi")
dir.create(ndvi_raw, showWarnings = FALSE, recursive = TRUE)
dir.create(out_dir,  showWarnings = FALSE, recursive = TRUE)

# Analysis window — match MCHTrack programme period
window_start <- as.Date("2024-08-01")
window_end   <- as.Date("2026-03-31")

library(janitor)
library(tidyverse)
library(lubridate)
library(ggplot2)
library(patchwork)

#----------------------------------------------------------------------------

###################################
# 1. PCODE reference table        #
###################################

# Standard Nigeria PCODE mapping for target LGAs
# Kano state = NG019, Katsina state = NG020
# LGA codes confirmed from OCHA Nigeria administrative boundaries

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
# 6. Visualisations               #
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
  filename = file.path(out_dir, "07_ndvi_diagnostic.png"),
  plot     = combined,
  width    = 14, height = 12, dpi = 300
)

cat("\nDiagnostic plots saved to:", file.path(out_dir, "07_ndvi_diagnostic.png"), "\n")

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
# 8. Save outputs                 #
###################################

# Save cleaned full series (all LGAs, full date range)
ndvi_all <- ndvi_clean %>%
  filter(include == TRUE) %>%
  select(state, pcode, lga_name, context, date, year_month,
         year, month_num, dekad_num, vim, viq, ag_season)

saveRDS(ndvi_monthly,  file.path(out_dir, "07_ndvi_monthly.rds"))
saveRDS(ndvi_dekadal,  file.path(out_dir, "07_ndvi_dekadal.rds"))
saveRDS(ndvi_all,      file.path(out_dir, "07_ndvi_full_series.rds"))

write_csv(ndvi_monthly,  file.path(out_dir, "07_ndvi_monthly.csv"))
write_csv(ndvi_dekadal,  file.path(out_dir, "07_ndvi_dekadal.csv"))
write_csv(var_summary,   file.path(out_dir, "07_ndvi_variability_summary.csv"))

cat("\nOutputs saved to:", out_dir, "\n")
cat("Files: 07_ndvi_monthly.rds/.csv, 07_ndvi_dekadal.rds/.csv,\n")
cat("       07_ndvi_full_series.rds, 07_ndvi_variability_summary.csv\n")
cat("       07_ndvi_diagnostic.png\n")

#----------------------------------------------------------------------------

###################################
# 9. Decision log                 #
###################################

cat("\n=== DECISION LOG ===\n")
cat("Proceed to 08_ndvi_analysis.R when:\n")
cat("  - sd_vim >= 0.15 confirmed for target LGAs (check above)\n")
cat("  - Seasonal pattern shows clear peak-trough aligned with agricultural calendar\n")
cat("  - Programme window NDVI series looks clean (no gaps or anomalies)\n\n")
cat("Primary exposure variables for regression:\n")
cat("  vim_c            — centred continuous NDVI (monthly mean)\n")
cat("  viq_c            — centred percentile vs 5yr baseline (anomaly control)\n")
cat("  vim_above_median — binary indicator of high-NDVI period (LGA-specific)\n\n")
cat("Katsina note: include as sensitivity analysis only\n")
cat("  Facility visits data has known duplication issues\n")
cat("  NDVI variable itself is unaffected — anomaly is MCHTrack-side only\n")
cat("  Flag explicitly in methods if Katsina results reported\n")
cat("===================\n")
cat("\n--- Script complete ---\n")