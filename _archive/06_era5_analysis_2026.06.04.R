########################################################
#  ERA5 UTCI Heat Stress — Monthly Regression Analysis #
#  Outcome: Monthly facility visit counts              #
#  Exposure: Days above 38°C UTCI                      #
#  Sites: Ungogo & Gabasawa LGAs, Kano                 #
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

library(pandoc)
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
    year_month = floor_date(visit_date, "month"),
    month_num  = month(visit_date),
    year       = year(visit_date),
    # Standardise LGA name for join — strip "Lga" suffix, trim, title case
    lga_clean  = str_to_title(str_trim(str_remove(lga_name, regex("\\s*lga\\s*$",
                                                                  ignore_case = TRUE))))
  ) %>%
  filter(
    visit_date >= as.Date("2024-08-01"),
    visit_date <= as.Date("2026-04-30")
  )

cat("Kano visits after filter:", nrow(data_fv_kano), "\n")
cat("LGA names after cleaning:", unique(data_fv_kano$lga_clean), "\n\n")

#----------------------------------------------------------------------------

###################################
# 2. Aggregate to LGA x month     #
###################################

outcome_panel <- data_fv_kano %>%
  group_by(lga_clean, year_month) %>%
  summarise(
    n_visits           = n(),
    n_unique_children  = n_distinct(patient_id),
    .groups = "drop"
  )

cat("Outcome panel rows:", nrow(outcome_panel), "\n")
cat("LGAs in outcome panel:", unique(outcome_panel$lga_clean), "\n")
print(head(outcome_panel, 6))

#----------------------------------------------------------------------------

###################################
# 3. Load UTCI monthly data       #
###################################

# Load 2025 series — when 2024 CDS pull completes, bind it here:
# bind_rows(readRDS(file.path(era5_dir, "05_era5_utci_monthly_2024.rds")))

utci_monthly <- readRDS(file.path(era5_dir, "05_era5_utci_monthly_2025.rds")) %>%
  mutate(
    lga_clean = str_to_title(str_trim(lga))
  ) %>%
  arrange(lga_clean, year_month)

cat("\nUTCI data rows:", nrow(utci_monthly), "\n")
cat("LGAs in UTCI data:", unique(utci_monthly$lga_clean), "\n\n")

#----------------------------------------------------------------------------

###################################
# 4. Join and build panel         #
###################################

panel <- outcome_panel %>%
  left_join(
    utci_monthly %>% select(lga_clean, year_month, utci_max, utci_min,
                            utci_mean_approx, days_above_46, days_above_38,
                            days_above_32, days_above_26),
    by = c("lga_clean", "year_month")
  ) %>%
  mutate(
    month_num  = month(year_month),
    year       = year(year_month),
    hot_season = as.integer(month_num %in% c(3, 4, 5, 6)),
    log_visits = log(n_visits + 1),
    # Centre exposure for interpretability — coefficient = effect of
    # one additional extreme heat day relative to monthly average
    days_38_c  = days_above_38 - mean(days_above_38, na.rm = TRUE)
  )

cat("Panel rows:", nrow(panel), "\n")
cat("Rows with UTCI matched:", sum(!is.na(panel$days_above_38)), "\n")
cat("Rows missing UTCI (outside 2025):", sum(is.na(panel$days_above_38)), "\n\n")

# Sanity check — should see both LGAs, both years
cat("--- Panel coverage ---\n")
panel %>%
  group_by(lga_clean, year) %>%
  summarise(n_months = n(), mean_visits = round(mean(n_visits), 0),
            .groups = "drop") %>%
  print()

# Filter to matched months only for regression
panel_reg <- panel %>% filter(!is.na(days_above_38))

cat("\nRegression panel:", nrow(panel_reg), "LGA-month observations\n\n")

#----------------------------------------------------------------------------

###################################
# 5. Descriptive checks           #
###################################

cat("--- Visit volume summary ---\n")
panel_reg %>%
  group_by(lga_clean) %>%
  summarise(
    mean_visits   = round(mean(n_visits), 0),
    sd_visits     = round(sd(n_visits), 0),
    min_visits    = min(n_visits),
    max_visits    = max(n_visits),
    .groups = "drop"
  ) %>% print()

cat("\n--- Exposure summary ---\n")
panel_reg %>%
  summarise(
    mean_days_38 = round(mean(days_above_38), 1),
    sd_days_38   = round(sd(days_above_38), 1),
    min_days_38  = min(days_above_38),
    max_days_38  = max(days_above_38)
  ) %>% print()

#----------------------------------------------------------------------------

###################################
# 6. Regression models            #
###################################

# Outcome: log(facility visits + 1) — OLS with LGA fixed effects
# Cluster SE at LGA level (small cluster caveat — only 2 LGAs)
# All models use feols() from fixest for consistency with Models A and B

# M1: Exposure only + LGA FE
# Baseline — does heat predict visits ignoring seasonality?
m1 <- feols(log_visits ~ days_above_38 | lga_clean,
            data = panel_reg,
            cluster = ~lga_clean)

# M2: + month FE — absorbs seasonal confounders
# Key specification — identifies within-year variation in heat
m2 <- feols(log_visits ~ days_above_38 | lga_clean + month_num,
            data = panel_reg,
            cluster = ~lga_clean)

# M3: + hot season binary instead of month FE
# Simpler seasonality control — useful if month FE overparametrises
m3 <- feols(log_visits ~ days_above_38 + hot_season | lga_clean,
            data = panel_reg,
            cluster = ~lga_clean)

# M4: Alternative exposure — days above 32°C (strong stress threshold)
# Robustness check — lower threshold captures more days
m4 <- feols(log_visits ~ days_above_32 | lga_clean + month_num,
            data = panel_reg,
            cluster = ~lga_clean)

cat("=== REGRESSION RESULTS ===\n\n")

etable(m1, m2, m3, m4,
       title    = "Heat stress (UTCI) and facility visits — monthly panel",
       digits   = 3,
       se.below = TRUE)

# Save formatted table
modelsummary(
  list("M1: LGA FE"       = m1,
       "M2: +Month FE"    = m2,
       "M3: +Hot season"  = m3,
       "M4: Alt threshold"= m4),
  stars     = c("*" = 0.1, "**" = 0.05, "***" = 0.01),
  gof_map   = c("nobs", "r.squared", "adj.r.squared"),
  title     = "Heat stress and facility visits — monthly panel · Kano 2025",
  output    = file.path(out_dir, "06_regression_monthly.docx")
)

cat("\nRegression table saved to:", file.path(out_dir, "06_regression_monthly.docx"), "\n")

#----------------------------------------------------------------------------

###################################
# 7. Visualisations               #
###################################

pal <- c("Ungogo" = "#D84A38", "Gabasawa" = "#1D6FA4")

# -- Plot 1: Scatter — heat days vs log visits -------------------------

p1 <- ggplot(panel_reg, aes(x = days_above_38, y = log_visits,
                            colour = lga_clean)) +
  geom_point(size = 3.5, alpha = 0.8) +
  geom_smooth(method = "lm", se = TRUE, linewidth = 0.9) +
  scale_colour_manual(values = pal) +
  labs(
    title   = "Days above 38°C UTCI vs facility visits",
    x       = "Days above 38°C UTCI per month",
    y       = "Log facility visits",
    colour  = NULL,
    caption = "Each point = one LGA-month · Lines = OLS fit per LGA"
  ) +
  theme_minimal(base_size = 12) +
  theme(plot.title = element_text(face = "bold"),
        legend.position = "top")

# -- Plot 2: Time series overlay — visits and heat days ----------------

# Scale heat to visits axis for dual-axis overlay
visit_max <- max(panel_reg$n_visits)
heat_max  <- max(panel_reg$days_above_38)
scale_f   <- visit_max / heat_max

p2 <- ggplot(panel_reg, aes(x = year_month)) +
  geom_col(aes(y = n_visits, fill = lga_clean),
           position = "dodge", alpha = 0.6, width = 20) +
  geom_line(aes(y = days_above_38 * scale_f, colour = lga_clean,
                group = lga_clean),
            linewidth = 1.1) +
  geom_point(aes(y = days_above_38 * scale_f, colour = lga_clean),
             size = 2.5) +
  scale_fill_manual(values   = pal) +
  scale_colour_manual(values = pal) +
  scale_x_date(date_breaks = "1 month", date_labels = "%b %Y") +
  scale_y_continuous(
    name     = "Facility visits",
    sec.axis = sec_axis(~ . / scale_f,
                        name = "Days above 38°C UTCI per month")
  ) +
  labs(
    title   = "Facility visits and extreme heat days over time",
    x       = NULL, fill = NULL, colour = NULL,
    caption = "Bars = visit counts · Lines = days above 38°C UTCI"
  ) +
  theme_minimal(base_size = 12) +
  theme(plot.title      = element_text(face = "bold"),
        legend.position = "top",
        axis.text.x     = element_text(angle = 45, hjust = 1))

# -- Combine -----------------------------------------------------------

combined <- p1 / p2 +
  plot_annotation(
    title    = "ERA5 UTCI heat stress and facility visits — monthly panel",
    subtitle = "Ungogo & Gabasawa LGAs, Kano · 2025",
    theme    = theme(plot.title    = element_text(face = "bold", size = 14),
                     plot.subtitle = element_text(size = 11))
  )

ggsave(
  filename = file.path(out_dir, "06_heat_visits_monthly.png"),
  plot     = combined,
  width    = 12, height = 10, dpi = 300
)

cat("Plots saved to:", out_dir, "\n")

#----------------------------------------------------------------------------

###################################
# 8. Save panel                   #
###################################

saveRDS(panel, file.path(out_dir, "06_panel_monthly.rds"))
write_csv(panel, file.path(out_dir, "06_panel_monthly.csv"))

cat("Panel saved.\n")
cat("\n--- Script complete ---\n")
cat("Next steps:\n")
cat("  1. When 2024 CDS data arrives — uncomment bind_rows in Section 3\n")
cat("  2. If coefficient significant — upgrade to daily in 06_era5_analysis_daily.R\n")
cat("  3. If null — document as second independent null climate finding\n")