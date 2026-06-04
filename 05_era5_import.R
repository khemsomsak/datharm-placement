########################################################
#  ERA5-HEAT UTCI Import & Variability Diagnostic      #
#  Source: ECMWF CDS monthly stats, 2025               #
#  Sites: Ungogo (peri-urban) & Gabasawa (rural), Kano #
#  Created: June 2026                                  #
########################################################

# Reset environment -----------------------------------------------------

rm(list = ls())
setwd("C:/Users/HP/Documents/GitHub/datharm-placement")
options(scipen = 999)
Sys.setlocale("LC_TIME", "English")

home    <- "C:/Users/HP/Documents/GitHub/datharm-placement"
era5_dir <- file.path(home, "02_data/03_external/06_ERA5")
out_dir  <- file.path(home, "03_output/05_era5")
dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)

# MCHTrack programme window
window_start <- as.Date("2024-08-01")
window_end   <- as.Date("2026-04-30")

library(ncdf4)
library(tidyverse)
library(lubridate)
library(ggplot2)
library(patchwork)
library(janitor)

#----------------------------------------------------------------------------

###################################
# 1. Define extraction sites      #
###################################

sites <- tibble(
  lga     = c("Ungogo",  "Gabasawa"),
  context = c("Peri-urban (Ungogo)", "Rural (Gabasawa)"),
  lat     = c(12.0906,   12.10),
  lon     = c(8.4967,    8.90)
)

cat("Extraction sites:\n")
print(sites)

#----------------------------------------------------------------------------

###################################
# 2. Inspect one file             #
###################################

# Run this section first to confirm variable names before extraction.
# All 12 files share the same internal structure — inspecting one is enough.

sample_file <- list.files(era5_dir, pattern = "monthly_stats", full.names = TRUE)[1]
cat("\nInspecting:", basename(sample_file), "\n")

nc_sample <- nc_open(sample_file)
cat("\n--- Variables inside file ---\n")
print(names(nc_sample$var))
cat("\n--- Dimensions ---\n")
print(names(nc_sample$dim))
cat("\nLatitude values:", ncvar_get(nc_sample, "lat"), "\n")
cat("Longitude values:", ncvar_get(nc_sample, "lon"), "\n")

# Pull one variable to check units and shape
first_var <- names(nc_sample$var)[1]
test_pull <- ncvar_get(nc_sample, first_var)
cat("\nFirst variable:", first_var, "\n")
cat("Dimensions of array:", dim(test_pull), "\n")
cat("Sample values (first cell):", test_pull[1,1,], "\n")
cat("Note: if values are around 295-320, units are Kelvin — subtract 273.15 for Celsius\n")

nc_close(nc_sample)

#----------------------------------------------------------------------------

###################################
# 3. Loop extraction — all months #
###################################

# List all monthly stats files
monthly_files <- list.files(
  path       = era5_dir,
  pattern    = "monthly_stats.*\\.nc$",
  full.names = TRUE
) %>% sort()

cat("\nFound", length(monthly_files), "monthly files:\n")
cat(paste(basename(monthly_files), collapse = "\n"), "\n")

# Helper: extract UTCI stats at nearest grid cell to each site
extract_utci_month <- function(filepath, sites) {
  
  nc   <- nc_open(filepath)
  lats <- ncvar_get(nc, "lat")
  lons <- ncvar_get(nc, "lon")
  
  # Parse year-month from filename e.g. monthly_stats_202503
  ym_str <- str_extract(basename(filepath), "\\d{6}")
  year_month <- as.Date(paste0(ym_str, "01"), "%Y%m%d")
  
  # Identify available variable names
  var_names <- names(nc$var)
  
  # Variable names confirmed from nc inspection:
  # utci_monthly_max, utci_monthly_min (stored in Kelvin)
  # utci_days_above_38_daily_max  — days/month where daily max UTCI > 38°C (very strong stress)
  # utci_days_above_32_daily_max  — days/month where daily max UTCI > 32°C (strong stress)
  # utci_days_above_26_daily_max  — days/month where daily max UTCI > 26°C (moderate stress)
  # No direct monthly mean — approximated as (max + min) / 2
  
  cat("Month:", format(year_month, "%b %Y"), "\n")
  
  # Extract each site
  site_rows <- map_dfr(seq_len(nrow(sites)), function(i) {
    
    lat_idx <- which.min(abs(lats - sites$lat[i]))
    lon_idx <- which.min(abs(lons - sites$lon[i]))
    
    pull_val <- function(vname) {
      if (!vname %in% var_names) {
        warning("Variable not found: ", vname)
        return(NA_real_)
      }
      arr <- ncvar_get(nc, vname)
      if (length(dim(arr)) == 2) {
        arr[lon_idx, lat_idx]
      } else {
        arr[lon_idx, lat_idx, 1]
      }
    }
    
    utci_max_k <- pull_val("utci_monthly_max")
    utci_min_k <- pull_val("utci_monthly_min")
    
    tibble(
      lga              = sites$lga[i],
      context          = sites$context[i],
      year_month       = year_month,
      utci_max         = utci_max_k - 273.15,
      utci_min         = utci_min_k - 273.15,
      utci_mean_approx = ((utci_max_k + utci_min_k) / 2) - 273.15,
      # Heat stress exposure — days per month above threshold (daily max)
      days_above_46    = pull_val("utci_days_above_46_daily_max"),  # extreme stress
      days_above_38    = pull_val("utci_days_above_38_daily_max"),  # very strong stress
      days_above_32    = pull_val("utci_days_above_32_daily_max"),  # strong stress
      days_above_26    = pull_val("utci_days_above_26_daily_max"),  # moderate stress
      # Same thresholds from daily min (overnight heat burden)
      days_above_26_min = pull_val("utci_days_above_26_daily_min"),
      lat_used         = lats[lat_idx],
      lon_used         = lons[lon_idx]
    )
  })
  
  nc_close(nc)
  site_rows
}

# Run extraction loop across all 12 files
cat("\n--- Extracting UTCI values ---\n")

utci_monthly <- map_dfr(monthly_files, ~ extract_utci_month(.x, sites))

cat("\n--- Extraction complete ---\n")
glimpse(utci_monthly)

#----------------------------------------------------------------------------

###################################
# 4. Enrich and label             #
###################################

utci_monthly <- utci_monthly %>%
  mutate(
    year       = year(year_month),
    month_num  = month(year_month),
    month_lab  = month(year_month, label = TRUE, abbr = TRUE),
    hot_season = month_num %in% c(3, 4, 5),
    # UTCI stress category based on monthly max (most policy-relevant)
    stress_cat_max = case_when(
      utci_max < 9  ~ "No stress",
      utci_max < 26 ~ "Slight",
      utci_max < 32 ~ "Moderate",
      utci_max < 38 ~ "Strong",
      utci_max < 46 ~ "Very strong",
      TRUE          ~ "Extreme"
    ) %>% factor(levels = c("No stress","Slight","Moderate","Strong","Very strong","Extreme"))
  )

#----------------------------------------------------------------------------

###################################
# 5. Variability diagnostic       #
###################################

cat("\n=== VARIABILITY DIAGNOSTIC ===\n")
cat("Key question: is there sufficient monthly variation to detect an effect?\n\n")

var_summary <- utci_monthly %>%
  group_by(lga, context) %>%
  summarise(
    n_months      = n(),
    mean_utci     = round(mean(utci_mean_approx, na.rm = TRUE), 2),
    sd_utci       = round(sd(utci_mean_approx, na.rm = TRUE),   2),
    min_utci      = round(min(utci_mean_approx, na.rm = TRUE),  2),
    max_utci      = round(max(utci_mean_approx, na.rm = TRUE),  2),
    range_utci    = round(max_utci - min_utci, 2),
    n_hot_months  = sum(hot_season, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    verdict = case_when(
      sd_utci >= 5 ~ "Strong variation — proceed to regression",
      sd_utci >= 3 ~ "Moderate variation — likely usable",
      sd_utci >= 2 ~ "Marginal variation — interpret with caution",
      TRUE         ~ "Near-zero variation — likely null (same issue as CHIRPS)"
    )
  )

cat("--- Summary by LGA ---\n")
print(var_summary)

# Variability diagnostic on heat stress day counts
utci_verdict <- utci_monthly %>%
  group_by(lga, context) %>%
  summarise(
    mean_days_38 = round(mean(days_above_38), 1),
    sd_days_38   = round(sd(days_above_38), 1),
    min_days_38  = min(days_above_38),
    max_days_38  = max(days_above_38),
    range_days_38 = max(days_above_38) - min(days_above_38),
    mean_days_32 = round(mean(days_above_32), 1),
    sd_days_32   = round(sd(days_above_32), 1),
    range_days_32 = max(days_above_32) - min(days_above_32),
    .groups = "drop"
  ) %>%
  mutate(
    verdict = case_when(
      sd_days_38 >= 10 ~ "Strong variation — proceed to regression",
      sd_days_38 >= 7  ~ "Moderate variation — likely usable",
      sd_days_38 >= 4  ~ "Marginal — interpret with caution",
      TRUE             ~ "Near-zero variation — likely null"
    )
  ) %>%
  print()

# Cross-LGA gap — is peri-urban warmer? (urban heat island check)
lga_gap <- utci_monthly %>%
  select(year_month, lga, utci_mean_approx) %>%
  pivot_wider(names_from = lga, values_from = utci_mean_approx) %>%
  mutate(gap_ung_minus_gab = round(Ungogo - Gabasawa, 2))  # utci_mean_approx units)
         
         cat("\n--- Urban heat island check: Ungogo minus Gabasawa (°C UTCI) ---\n")
         cat("Mean gap:", round(mean(lga_gap$gap_ung_minus_gab, na.rm = TRUE), 2), "°C\n")
         cat("Max gap:", round(max(lga_gap$gap_ung_minus_gab, na.rm = TRUE), 2), "°C\n")
         cat("Min gap:", round(min(lga_gap$gap_ung_minus_gab, na.rm = TRUE), 2), "°C\n")
         cat("Positive = Ungogo warmer as expected if UHI present\n")
         
         # Hot season summary
         hot_summary <- utci_monthly %>%
           group_by(hot_season) %>%
           summarise(
             mean_utci = round(mean(utci_mean_approx, na.rm = TRUE), 2),
             n         = n(),
             .groups   = "drop"
           ) %>%
           mutate(season = if_else(hot_season, "Hot season (Mar-May)", "Other months"))
         
         cat("\n--- Hot season vs other months ---\n")
         print(hot_summary %>% select(season, mean_utci, n))
         
         #----------------------------------------------------------------------------
         
         ###################################
         # 6. Visualisations               #
         ###################################
         
         pal <- c("Peri-urban (Ungogo)" = "#D84A38", "Rural (Gabasawa)" = "#1D6FA4")
         
         # -- Plot 1: Time series of monthly mean UTCI -------------------------
         
         p1 <- ggplot(utci_monthly, aes(x = year_month, y = utci_mean_approx,
                                        colour = context, group = context)) +
           annotate("rect",
                    xmin = as.Date("2025-03-01"), xmax = as.Date("2025-06-01"),
                    ymin = -Inf, ymax = Inf,
                    fill = "#BA7517", alpha = 0.10) +
           geom_line(linewidth = 1) +
           geom_point(size = 3) +
           annotate("text", x = as.Date("2025-04-15"), y = max(utci_monthly$utci_mean_approx, na.rm = TRUE) + 0.8,
                    label = "Hot season", size = 3.2, colour = "#BA7517") +
           scale_colour_manual(values = pal) +
           scale_x_date(date_breaks = "1 month", date_labels = "%b %Y") +
           labs(
             title    = "Monthly mean UTCI — Ungogo vs Gabasawa, 2025",
             subtitle = "ERA5-HEAT consolidated stream · Point extraction at LGA administrative centres",
             x        = NULL, y = "UTCI approx mean (°C)", colour = NULL,
             caption  = "Shaded band = Sahelian hot season (Mar–May) · Mean = (monthly max + min) / 2"
           ) +
           theme_minimal(base_size = 12) +
           theme(legend.position = "top",
                 plot.title      = element_text(face = "bold"),
                 axis.text.x     = element_text(angle = 45, hjust = 1))
         
         # -- Plot 2: Max–mean–min range bars per month -----------------------
         
         p2 <- ggplot(utci_monthly %>% filter(lga == "Ungogo"),
                      aes(x = month_lab)) +
           geom_errorbar(aes(ymin = utci_min, ymax = utci_max),
                         width = 0.3, colour = "#888780", linewidth = 0.8) +
           geom_point(aes(y = utci_mean_approx), size = 4, colour = "#D84A38") +
           geom_hline(yintercept = 32, linetype = "dashed",
                      colour = "#BA7517", linewidth = 0.7) +
           annotate("text", x = 1, y = 33, hjust = 0,
                    label = "Strong heat stress threshold (32°C)", size = 3,
                    colour = "#BA7517") +
           labs(
             title    = "UTCI range by month — Ungogo",
             subtitle = "Bars = min/max, dots = monthly mean approx",
             x        = NULL, y = "UTCI (°C equivalent)",
             caption  = "Horizontal dashed line = strong heat stress threshold"
           ) +
           theme_minimal(base_size = 12) +
           theme(plot.title = element_text(face = "bold"))
         
         # -- Plot 3: Distribution of monthly UTCI — variation diagnostic -----
         
         p3 <- ggplot(utci_monthly, aes(x = utci_mean_approx, fill = context)) +
           geom_density(alpha = 0.45, colour = NA) +
           geom_vline(data = var_summary,
                      aes(xintercept = mean_utci, colour = context),
                      linetype = "dashed", linewidth = 0.8) +
           scale_fill_manual(values  = pal) +
           scale_colour_manual(values = pal) +
           labs(
             title    = "Distribution of monthly mean UTCI",
             subtitle = "Width of distribution = available variation for regression",
             x        = "UTCI (°C equivalent)", y = "Density",
             fill     = NULL, colour = NULL,
             caption  = "Dashed lines = LGA annual mean · Wide = sufficient variation"
           ) +
           theme_minimal(base_size = 12) +
           theme(legend.position = "top",
                 plot.title      = element_text(face = "bold"))
         
         # -- Plot 4: Ungogo minus Gabasawa gap (heat island) -----------------
         
         p4 <- ggplot(lga_gap, aes(x = year_month, y = gap_ung_minus_gab)) +
           geom_col(fill = "#7F77DD", alpha = 0.75, width = 20) +
           geom_hline(yintercept = 0, linewidth = 0.7) +
           scale_x_date(date_breaks = "1 month", date_labels = "%b") +
           labs(
             title    = "Ungogo minus Gabasawa UTCI gap",
             subtitle = "Positive = peri-urban warmer (urban heat island)",
             x        = NULL, y = "°C UTCI difference",
             caption  = "ERA5 at 0.25° resolution — may understate true UHI"
           ) +
           theme_minimal(base_size = 12) +
           theme(plot.title  = element_text(face = "bold"))
         
         # -- Plot 5: Heat stress days per month (key exposure variable) ------
         
         p5 <- ggplot(utci_monthly, aes(x = month_lab)) +
           geom_col(aes(y = days_above_38, fill = context),
                    position = "dodge", alpha = 0.85) +
           geom_col(aes(y = days_above_32, fill = context),
                    position = "dodge", alpha = 0.35, colour = NA) +
           scale_fill_manual(values = pal) +
           facet_wrap(~ context, ncol = 2) +
           labs(
             title    = "Days per month above heat stress thresholds",
             subtitle = "Dark = days above 38°C UTCI (very strong) · Light = days above 32°C (strong)",
             x        = NULL, y = "Days per month", fill = NULL,
             caption  = "Key exposure variable for regression — counts caregiver heat burden days"
           ) +
           theme_minimal(base_size = 12) +
           theme(legend.position = "none",
                 plot.title      = element_text(face = "bold"),
                 strip.text      = element_text(face = "bold"))
         
         # -- Combine ---------------------------------------------------------
         
         combined <- (p1 + p2) / (p3 + p4) / (p5 + plot_spacer()) +
           plot_annotation(
             title    = "ERA5-HEAT UTCI — variability diagnostic · Kano, 2025",
             subtitle = "Ungogo (peri-urban) vs Gabasawa (rural)",
             theme    = theme(
               plot.title    = element_text(face = "bold", size = 14),
               plot.subtitle = element_text(size = 11)
             )
           )
         
         ggsave(
           filename = file.path(out_dir, "05_era5_utci_diagnostic.png"),
           plot     = combined,
           width    = 14, height = 14, dpi = 300
         )
         
         cat("\nDiagnostic plot saved to:", file.path(out_dir, "05_era5_utci_diagnostic.png"), "\n")
         
         #----------------------------------------------------------------------------
         
         ###################################
         # 7. Save extracted series        #
         ###################################
         
         write_csv(
           utci_monthly,
           file.path(out_dir, "05_era5_utci_monthly_2025.csv")
         )
         
         saveRDS(
           utci_monthly,
           file.path(out_dir, "05_era5_utci_monthly_2025.rds")
         )
         
         cat("Extracted series saved to CSV and RDS.\n")
         
         #----------------------------------------------------------------------------
         
         ###################################
         # 8. Decision log                 #
         ###################################
         
         cat("\n=== DECISION LOG ===\n")
         cat("If sd_utci >= 3 AND range_utci >= 8: sufficient — proceed to:\n")
         cat("  (a) Submit second CDS pull for Aug-Dec 2024 + Jan-Apr 2026 monthly stats\n")
         cat("  (b) Write 06_temperature_regression.R merging UTCI onto MCHTrack panel\n\n")
         cat("If sd_utci < 2: near-zero variation — same structural issue as CHIRPS\n")
         cat("  Document as second independent null finding — strengthens limitation argument\n\n")
         cat("If UHI gap is consistently < 0.5°C: ERA5 cannot distinguish Ungogo/Gabasawa\n")
         cat("  Treat both as a single Kano UTCI series in any regression\n")
         cat("===================\n")
         
         cat("\n--- Script complete ---\n")