########################################
#  08_ndvi_analysis.R                  #
#  Created: June 2026                  #
#  Updated: 24/7/2026                  #
########################################

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

window_start <- as.Date("2024-08-01")
window_end   <- as.Date("2026-03-31")

library(janitor)
library(tidyverse)
library(lubridate)
library(ggplot2)
library(patchwork)
library(fixest)        # feols() / fenegbin() for panel regression
library(modelsummary)
library(scales)

#----------------------------------------------------------------------------

###################################
# 1. PCODE reference table        #
###################################

# Ungogo = NG019013, Gabasawa = NG019006, sourced from OCHA Nigeria admin
# boundaries. Used as the reference to fix a conflicting pcode assignment
# in 02_chirps_import_analysis.R's own lookup for these same two LGAs.

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

#----------------------------------------------------------------------------

###################################
# 2. Load and clean NDVI data     #
###################################

ndvi_src <- "02_data/03_external/07_NDVI.csv"

if (!file.exists(ndvi_src)) {
  ndvi_src <- "ngandvisubnat5ytd.csv"  # fallback: project root
}

ndvi_raw_data <- read_csv(ndvi_src, show_col_types = FALSE) %>%
  clean_names()

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
    dekad_label = paste0(format(date, "%b"), "-D", dekad_num),
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

#----------------------------------------------------------------------------

###################################
# 3. Programme window subset      #
###################################

ndvi_prog <- ndvi_clean %>%
  filter(date >= window_start, date <= window_end, include == TRUE)

#----------------------------------------------------------------------------

###################################
# 4. Variability diagnostic       #
###################################

# Feeds 07_ndvi_variability_summary.csv (Section 8) and Plot 4 (Section 6)

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

#----------------------------------------------------------------------------

###################################
# 7. Build analysis-ready panel   #
###################################

# Monthly aggregation for merging onto MCHTrack visit panels; dekadal kept
# separate for any higher-resolution use.

ndvi_monthly <- ndvi_prog %>%
  group_by(state, pcode, lga_name, context, year_month, year, month_num) %>%
  summarise(
    n_dekads      = n(),
    vim_monthly   = round(mean(vim, na.rm = TRUE), 4),
    viq_monthly   = round(mean(viq, na.rm = TRUE), 2),
    vim_max       = round(max(vim, na.rm = TRUE), 4),
    vim_min       = round(min(vim, na.rm = TRUE), 4),
    ag_season     = first(ag_season),
    .groups = "drop"
  ) %>%
  group_by(pcode) %>%
  mutate(
    vim_above_median = as.integer(vim_monthly > median(vim_monthly, na.rm = TRUE)),
    vim_c            = vim_monthly - mean(vim_monthly, na.rm = TRUE),
    viq_c            = viq_monthly - mean(viq_monthly, na.rm = TRUE)
  ) %>%
  ungroup()

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

#----------------------------------------------------------------------------

###################################
# 8. Save import/diagnostic outputs#
###################################

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

#################################################################
# REGRESSION ANALYSIS — NDVI and facility visits, Kano primary #
#################################################################

# Mirrors 06_era5_analysis.R and 02_chirps_import_analysis.R so all three
# exposures are comparable. NDVI's native resolution is dekadal, aggregated
# to monthly above — no dekad-derived "daily" spec here, since visit volume
# at dekad resolution is too sparse per LGA-dekad (Kano target = 2 LGAs).

# 9. Load MCHTrack facility visits ------------------------------------------

data_fv <- readRDS(file.path(mchtrack_dir, "01_facility_visits_clean.rds"))

# Same target sites/window as 06_era5_analysis.R and 02_chirps_import_analysis.R
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

#----------------------------------------------------------------------------

###################################
# 12. Regression models (Kano)    #
###################################

# Outcome log(visits + 1), matching heat/precipitation. Matches the W5/W6
# weather-table slots (Monthly, LGA[+month-year] FE).

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

#----------------------------------------------------------------------------

###################################
# 13. Negative binomial — primary #
###################################

# NB is the primary specification per Prabin's second-draft review (c84),
# applied consistently with 02/06. OLS retained for comparison. Column
# order flipped vs the previous draft — NB leads. 10_visualizations.R's
# parser needs updating to match.
# N1/N1_nb (vim level, LGA FE only) is the W5 headline spec and stays null
# under both. Section 13b's N4 (vim_c + month-year FE, offset-adjusted)
# comes back significant under both OLS and NB — flagged there, not
# resolved here, since it's an interpretive call rather than a labelling one.

n1_nb <- fenegbin(n_visits ~ vim_c | lga_clean,
                  data    = panel_ndvi_kano,
                  cluster = ~lga_clean)

n2_nb <- fenegbin(n_visits ~ viq_c | lga_clean + ym_factor,
                  data    = panel_ndvi_kano,
                  cluster = ~lga_clean)

etable(n1_nb, n1, n2_nb, n2,
       title  = "Negative binomial (primary) vs OLS log-visits — NDVI exposure",
       digits = 4, se.below = TRUE)

modelsummary(
  list(
    "N1_nb: NB — vim level (primary)"   = n1_nb,
    "N1: OLS — vim level"               = n1,
    "N2_nb: NB — viq anomaly (primary)" = n2_nb,
    "N2: OLS — viq anomaly"             = n2
  ),
  stars   = c("*" = 0.1, "**" = 0.05, "***" = 0.01),
  gof_map = c("nobs", "r.squared"),
  title   = "NDVI — negative binomial (primary) vs OLS, Kano monthly panel",
  output  = file.path(out_dir, "08_regression_ndvi_nb_comparison.txt")
)

#----------------------------------------------------------------------------

#############################################
# 13b. Exposure/offset + spline robustness  #
# (Prabin Dahal, 15/7/2026 review)          #
#############################################

# Same additions as 02 Section 12 and 06 Section 5c. Applied to N4 (vim_c,
# +month-year FE) and N2 (viq_c, +month-year FE) — the richest-FE specs,
# not N1/N3, matching heat's D3/D4 pattern. Kano-primary panel only; the
# Katsina check in Section 14 is untouched.
# Saved to its own file — 10_visualizations.R reads fixed columns from
# 08_regression_ndvi_kano.txt and 08_regression_ndvi_nb_comparison.txt.
#
# FLAG: N4_off and N4_nb_off (vim_c, offset-adjusted) both come back
# significant here, unlike N1/N1_nb above and unlike heat/precipitation's
# offset checks. This sits underneath Table 3.3's NDVI row and hasn't been
# resolved — worth a decision on whether it's a genuine finding or an
# artefact of the small (~40 LGA-month) panel before it goes further than
# a robustness footnote.

lga_month_path <- file.path(mchtrack_dir, "01_panel_lga_month.rds")

if (file.exists(lga_month_path)) {
  
  # enrolled_lookup's year_month is character "%Y-%m"; panel_ndvi_kano's is
  # a <date>. Join on a derived character key on both sides.
  enrolled_lookup <- readRDS(lga_month_path) %>%
    filter(state == "Kano") %>%
    mutate(
      lga_clean = str_to_title(str_remove(lga_name, regex("\\s*lga\\s*$", ignore_case = TRUE))),
      ym_key    = as.character(year_month)
    ) %>%
    filter(lga_clean %in% c("Ungogo", "Gabasawa")) %>%
    distinct(lga_clean, ym_key, enrolled_children)
  
  # Joined onto a copy — panel_ndvi_kano and its Section 16 exports stay untouched.
  panel_ndvi_off <- panel_ndvi_kano %>%
    mutate(ym_key = format(year_month, "%Y-%m")) %>%
    left_join(enrolled_lookup, by = c("lga_clean", "ym_key"))
  
  panel_ndvi_off_valid <- panel_ndvi_off %>% filter(!is.na(enrolled_children), enrolled_children > 0)
  
  n4_off <- feols(log_visits ~ vim_c | lga_clean + ym_factor,
                  data    = panel_ndvi_off_valid,
                  offset  = ~log(enrolled_children),
                  cluster = ~lga_clean)
  
  n2_off <- feols(log_visits ~ viq_c | lga_clean + ym_factor,
                  data    = panel_ndvi_off_valid,
                  offset  = ~log(enrolled_children),
                  cluster = ~lga_clean)
  
  n4_nb_off <- fenegbin(n_visits ~ vim_c | lga_clean + ym_factor,
                        data    = panel_ndvi_off_valid,
                        offset  = ~log(enrolled_children),
                        cluster = ~lga_clean)
  
  # vim_c only, no offset — isolates linearity from the exposure question.
  # df kept small given the limited number of LGA-months.
  n4_spline <- feols(log_visits ~ splines::ns(vim_c, df = 3) | lga_clean + ym_factor,
                     data    = panel_ndvi_kano,
                     cluster = ~lga_clean)
  
  etable(n4, n4_off, n4_nb_off, n2, n2_off, n4_spline,
         title    = "NDVI robustness — offset and spline specifications",
         digits   = 4,
         se.below = TRUE)
  
  modelsummary(
    list(
      "N4_off: vim_c, +month-year, offset"    = n4_off,
      "N4_nb_off: NB vim_c, +month-year, offset" = n4_nb_off,
      "N2_off: viq_c, +month-year, offset"    = n2_off,
      "N4_spline: vim_c, +month-year, spline (no offset)" = n4_spline
    ),
    stars   = c("*" = 0.1, "**" = 0.05, "***" = 0.01),
    gof_map = c("nobs", "r.squared"),
    title   = "NDVI — exposure-offset and spline robustness (Prabin, 15/7/2026)",
    output  = file.path(out_dir, "08_regression_ndvi_robustness_prabin.txt")
  )
}

#----------------------------------------------------------------------------

##############################################################
# 14. Katsina sensitivity check — NOT the primary analysis   #
##############################################################

# Katsina facility-visits data has known duplication issues that emerged
# partway through the placement, so Kano is the only state reliable across
# the full study period for a daily/monthly visit-count outcome. RQ1/RQ2
# pool both states since those analyses don't depend on visit-count volume
# the way this one does. Katsina is reported here only as an explicit
# sensitivity check — not pooled into N1-N4, not used to claim anything
# about NDVI's effect in Katsina specifically.

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

if (nrow(panel_ndvi_katsina) >= 20) {
  n1_katsina <- feols(log_visits ~ vim_c | lga_clean,
                      data    = panel_ndvi_katsina,
                      cluster = ~lga_clean)
  
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
}

#----------------------------------------------------------------------------

###################################
# 15. Regression visualisations   #
###################################

pal_kano <- c("Ungogo" = "#D84A38", "Gabasawa" = "#1D6FA4")

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

#--------------------------(END)------------------------------#