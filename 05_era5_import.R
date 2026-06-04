########################################################
#  ERA5-HEAT UTCI Daily Import & QC Diagnostic         #
#  Source: ECMWF CDS daily files, Aug 2024 – Apr 2026  #
#  Sites: Ungogo (peri-urban) & Gabasawa (rural), Kano #
#  Created: June 2026                                  #
########################################################

# Reset environment -----------------------------------------------------

rm(list = ls())
setwd("C:/Users/HP/Documents/GitHub/datharm-placement")
options(scipen = 999)
Sys.setlocale("LC_TIME", "English")

home     <- "C:/Users/HP/Documents/GitHub/datharm-placement"
era5_raw <- file.path(home, "02_data/03_external/06_ERA5")
out_dir  <- file.path(home, "03_output/05_era5")
dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)

# Full programme window
window_start <- as.Date("2024-08-01")
window_end   <- as.Date("2026-04-30")

library(ncdf4)
library(tidyverse)
library(lubridate)
library(ggplot2)
library(patchwork)

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
# 2. Inventory daily files        #
###################################

# Separate daily from monthly — daily files have 8-digit date, monthly have 6
all_files <- list.files(era5_raw, pattern = "\\.nc$", full.names = TRUE)

daily_files <- all_files[str_detect(basename(all_files),
                                    "utci_\\d{8}_")] %>% sort()

monthly_files <- all_files[str_detect(basename(all_files),
                                      "monthly_stats")] %>% sort()

cat("\nTotal .nc files found:", length(all_files))
cat("\nDaily files found:    ", length(daily_files))
cat("\nMonthly files found:  ", length(monthly_files), "\n")

# Parse dates from daily filenames
file_dates <- as.Date(
  str_extract(basename(daily_files), "\\d{8}"), "%Y%m%d"
)

cat("\nDaily file coverage:\n")
cat("  First date:", format(min(file_dates), "%d %b %Y"), "\n")
cat("  Last date: ", format(max(file_dates), "%d %b %Y"), "\n")
cat("  N files:   ", length(daily_files), "\n")

# Check for missing dates in window
full_date_seq  <- seq(window_start, window_end, by = "day")
missing_dates  <- full_date_seq[!full_date_seq %in% file_dates]

cat("\nExpected days in window:", length(full_date_seq))
cat("\nMissing days:           ", length(missing_dates))
if (length(missing_dates) > 0 & length(missing_dates) <= 40) {
  cat("\nMissing date list:", format(missing_dates, "%d %b %Y"), "\n")
} else if (length(missing_dates) > 40) {
  cat("\nToo many missing — check CDS pull coverage\n")
} else {
  cat(" — complete coverage\n")
}

#----------------------------------------------------------------------------

###################################
# 3. Inspect one daily file       #
###################################

nc_sample <- nc_open(daily_files[1])
cat("\n--- Sample file inspection ---\n")
cat("File:", basename(daily_files[1]), "\n")
cat("Variables:", names(nc_sample$var), "\n")
cat("Dimensions:", names(nc_sample$dim), "\n")
cat("Lat values:", ncvar_get(nc_sample, "lat"), "\n")
cat("Lon values:", ncvar_get(nc_sample, "lon"), "\n")

# Check first variable dimensions — daily files have hourly layers
first_var  <- names(nc_sample$var)[1]
test_arr   <- ncvar_get(nc_sample, first_var)
cat("Array dimensions [lon x lat x hour]:", dim(test_arr), "\n")
cat("Sample value (first cell, midday hour):", test_arr[1, 1, 12] - 273.15, "°C\n")
nc_close(nc_sample)

#----------------------------------------------------------------------------

###################################
# 4. Extraction loop — daily      #
###################################

# For each daily file:
#   - Extract hourly UTCI at each site's nearest grid cell
#   - Compute daily max (peak heat burden) and daily mean
#   - Flag extreme heat day: daily max >= 38°C

extract_utci_day <- function(filepath, sites) {
  
  nc   <- nc_open(filepath)
  lats <- ncvar_get(nc, "lat")
  lons <- ncvar_get(nc, "lon")
  
  # Parse date from filename
  date_str <- str_extract(basename(filepath), "\\d{8}")
  file_date <- as.Date(date_str, "%Y%m%d")
  
  # Get UTCI variable — first non-time/bounds variable
  var_names <- names(nc$var)
  utci_var  <- var_names[str_detect(var_names,
                                    regex("utci", ignore_case = TRUE))][1]
  
  if (is.na(utci_var)) {
    # Fallback — use first variable
    utci_var <- var_names[!var_names %in% c("time_bnds", "bnds")][1]
  }
  
  # Array dimensions: [lon x lat x hour] (24 hourly values)
  arr <- ncvar_get(nc, utci_var)
  nc_close(nc)
  
  site_rows <- map_dfr(seq_len(nrow(sites)), function(i) {
    
    lat_idx <- which.min(abs(lats - sites$lat[i]))
    lon_idx <- which.min(abs(lons - sites$lon[i]))
    
    # Extract hourly series for this grid cell
    if (length(dim(arr)) == 3) {
      hourly_k <- arr[lon_idx, lat_idx, ]   # 24 values in Kelvin
    } else {
      hourly_k <- arr[lon_idx, lat_idx]      # single value — use as-is
    }
    
    hourly_c <- hourly_k - 273.15
    
    tibble(
      lga           = sites$lga[i],
      context       = sites$context[i],
      date          = file_date,
      utci_daily_max  = max(hourly_c,  na.rm = TRUE),
      utci_daily_min  = min(hourly_c,  na.rm = TRUE),
      utci_daily_mean = mean(hourly_c, na.rm = TRUE),
      # Daytime mean: UTC hours 8-17 = local 09:00-18:00 (Kano = UTC+1)
      utci_daytime_mean = if (length(hourly_c) >= 17)
        mean(hourly_c[8:17], na.rm = TRUE)
      else mean(hourly_c, na.rm = TRUE),
      lat_used      = lats[lat_idx],
      lon_used      = lons[lon_idx],
      n_hours       = length(hourly_c)
    )
  })
  
  site_rows
}

# Filter to programme window only
daily_files_window <- daily_files[
  file_dates >= window_start & file_dates <= window_end
]

cat("\nExtracting", length(daily_files_window),
    "daily files across programme window...\n")

# Run loop with progress counter
utci_daily <- map_dfr(
  seq_along(daily_files_window),
  function(i) {
    if (i %% 50 == 0) cat("  Processed", i, "of",
                          length(daily_files_window), "files\n")
    extract_utci_day(daily_files_window[i], sites)
  }
)

cat("Extraction complete. Rows:", nrow(utci_daily), "\n\n")

#----------------------------------------------------------------------------

###################################
# 5. Enrich daily series          #
###################################

utci_daily <- utci_daily %>%
  arrange(lga, date) %>%
  mutate(
    year       = year(date),
    month_num  = month(date),
    month_lab  = month(date, label = TRUE, abbr = TRUE),
    year_month = floor_date(date, "month"),
    dow        = wday(date, label = TRUE, abbr = TRUE),
    dow_num    = wday(date),
    weekend    = as.integer(dow_num %in% c(1, 7)),
    hot_season = as.integer(month_num %in% c(3, 4, 5, 6)),
    # Primary exposure: binary extreme heat day
    extreme_heat_38 = as.integer(utci_daily_max >= 38),
    extreme_heat_32 = as.integer(utci_daily_max >= 32),
    extreme_heat_46 = as.integer(utci_daily_max >= 46),
    # Flag for QC
    implausible = as.integer(utci_daily_max > 60 | utci_daily_min < -10)
  )

cat("Enriched series glimpse:\n")
glimpse(utci_daily)

#----------------------------------------------------------------------------

###################################
# 6. Quality control checks       #
###################################

cat("\n=== QUALITY CONTROL ===\n\n")

# 6a. Missing dates
cat("--- Coverage check ---\n")
for (s in unique(utci_daily$lga)) {
  site_dates <- utci_daily %>% filter(lga == s) %>% pull(date)
  n_missing  <- sum(!full_date_seq %in% site_dates)
  cat(s, ": ", length(site_dates), "days extracted,",
      n_missing, "missing from window\n")
}

# 6b. Implausible values
cat("\n--- Implausible value check (UTCI max >60°C or min <-10°C) ---\n")
n_implausible <- sum(utci_daily$implausible, na.rm = TRUE)
cat("Implausible rows:", n_implausible, "\n")
if (n_implausible > 0) {
  cat("Flagged rows:\n")
  utci_daily %>% filter(implausible == 1) %>%
    select(lga, date, utci_daily_max, utci_daily_min) %>%
    print()
}

# 6c. Duplicate dates
cat("\n--- Duplicate date check ---\n")
dupes <- utci_daily %>%
  group_by(lga, date) %>%
  filter(n() > 1) %>%
  nrow()
cat("Duplicate LGA-date rows:", dupes, "\n")

# 6d. N hours per file
cat("\n--- Hours per file check ---\n")
utci_daily %>%
  count(n_hours) %>%
  print()

# 6e. Summary by LGA
cat("\n--- Summary statistics by LGA ---\n")
utci_daily %>%
  group_by(lga, context) %>%
  summarise(
    n_days          = n(),
    mean_daily_max  = round(mean(utci_daily_max, na.rm = TRUE), 1),
    sd_daily_max    = round(sd(utci_daily_max, na.rm = TRUE), 1),
    min_daily_max   = round(min(utci_daily_max, na.rm = TRUE), 1),
    max_daily_max   = round(max(utci_daily_max, na.rm = TRUE), 1),
    pct_above_38    = round(mean(extreme_heat_38) * 100, 1),
    pct_above_32    = round(mean(extreme_heat_32) * 100, 1),
    pct_above_46    = round(mean(extreme_heat_46) * 100, 1),
    .groups = "drop"
  ) %>%
  mutate(
    verdict = case_when(
      sd_daily_max >= 5 & pct_above_38 < 95 & pct_above_38 > 5 ~
        "Strong variation — proceed to regression",
      sd_daily_max >= 3 ~
        "Moderate variation — usable",
      TRUE ~
        "Near-zero variation — likely null"
    )
  ) %>%
  print()

#----------------------------------------------------------------------------

###################################
# 7. Variability diagnostics      #
###################################

pal <- c("Peri-urban (Ungogo)" = "#D84A38",
         "Rural (Gabasawa)"    = "#1D6FA4")

# -- Plot 1: Daily max UTCI time series --------------------------------

p1 <- ggplot(utci_daily, aes(x = date, y = utci_daily_max,
                             colour = context)) +
  annotate("rect",
           xmin = as.Date("2024-03-01"), xmax = as.Date("2024-07-01"),
           ymin = -Inf, ymax = Inf, fill = "#BA7517", alpha = 0.07) +
  annotate("rect",
           xmin = as.Date("2025-03-01"), xmax = as.Date("2025-07-01"),
           ymin = -Inf, ymax = Inf, fill = "#BA7517", alpha = 0.07) +
  annotate("rect",
           xmin = as.Date("2026-03-01"), xmax = as.Date("2026-07-01"),
           ymin = -Inf, ymax = Inf, fill = "#BA7517", alpha = 0.07) +
  geom_line(alpha = 0.6, linewidth = 0.4) +
  geom_smooth(method = "loess", span = 0.1,
              se = FALSE, linewidth = 1.1) +
  geom_hline(yintercept = 38, linetype = "dashed",
             colour = "#BA7517", linewidth = 0.7) +
  geom_hline(yintercept = 46, linetype = "dotted",
             colour = "#D84A38", linewidth = 0.7) +
  scale_colour_manual(values = pal) +
  scale_x_date(date_breaks = "2 months", date_labels = "%b %Y") +
  labs(
    title    = "Daily maximum UTCI across programme window",
    subtitle = "ERA5-HEAT · Aug 2024 – Apr 2026 · Shaded = hot season (Mar–Jun)",
    x        = NULL, y = "Daily max UTCI (°C)", colour = NULL,
    caption  = "Dashed = 38°C threshold (very strong stress) · Dotted = 46°C (extreme)"
  ) +
  theme_minimal(base_size = 12) +
  theme(legend.position = "top",
        plot.title      = element_text(face = "bold"),
        axis.text.x     = element_text(angle = 30, hjust = 1))

# -- Plot 2: Distribution of daily max — variation diagnostic ----------

p2 <- ggplot(utci_daily, aes(x = utci_daily_max, fill = context)) +
  geom_histogram(bins = 40, alpha = 0.6, position = "identity",
                 colour = NA) +
  geom_vline(xintercept = 38, linetype = "dashed",
             colour = "#BA7517", linewidth = 0.8) +
  geom_vline(xintercept = 32, linetype = "dotted",
             colour = "#888780", linewidth = 0.8) +
  scale_fill_manual(values = pal) +
  labs(
    title    = "Distribution of daily max UTCI",
    subtitle = "Key diagnostic: bimodal or wide = good variation for regression",
    x        = "Daily max UTCI (°C)", y = "Days",
    fill     = NULL,
    caption  = "Dashed = 38°C · Dotted = 32°C"
  ) +
  theme_minimal(base_size = 12) +
  theme(legend.position = "top",
        plot.title      = element_text(face = "bold"))

# -- Plot 3: Monthly extreme heat day counts ---------------------------

monthly_counts <- utci_daily %>%
  group_by(lga, context, year_month) %>%
  summarise(
    days_above_38 = sum(extreme_heat_38, na.rm = TRUE),
    days_above_32 = sum(extreme_heat_32, na.rm = TRUE),
    .groups = "drop"
  )

p3 <- ggplot(monthly_counts, aes(x = year_month, y = days_above_38,
                                 fill = context)) +
  geom_col(position = "dodge", alpha = 0.8, width = 20) +
  scale_fill_manual(values = pal) +
  scale_x_date(date_breaks = "2 months", date_labels = "%b %Y") +
  labs(
    title    = "Days per month above 38°C UTCI",
    subtitle = "Constructed from daily extraction — consistency check vs monthly stats files",
    x        = NULL, y = "Days per month", fill = NULL
  ) +
  theme_minimal(base_size = 12) +
  theme(legend.position = "top",
        plot.title      = element_text(face = "bold"),
        axis.text.x     = element_text(angle = 30, hjust = 1))

# -- Plot 4: Consistency check vs monthly stats ------------------------
# Recompute monthly max from daily and compare to utci_monthly_max

monthly_from_daily <- utci_daily %>%
  group_by(lga, year_month) %>%
  summarise(
    max_from_daily = max(utci_daily_max, na.rm = TRUE),
    .groups = "drop"
  )

monthly_stats_2025 <- readRDS(
  file.path(out_dir, "05_era5_utci_monthly_2025.rds")
) %>%
  select(lga, year_month, utci_max_monthly = utci_max) %>%
  filter(year_month >= as.Date("2025-01-01"),
         year_month <= as.Date("2025-12-01"))

consistency_check <- monthly_from_daily %>%
  filter(year(year_month) == 2025) %>%
  left_join(monthly_stats_2025, by = c("lga", "year_month")) %>%
  mutate(diff = round(max_from_daily - utci_max_monthly, 2))

cat("\n--- Consistency check: daily-derived max vs monthly stats max (2025) ---\n")
print(consistency_check)
cat("Mean absolute difference:", round(mean(abs(consistency_check$diff),
                                            na.rm = TRUE), 2), "°C\n")
cat("If < 1°C: daily and monthly files are consistent\n\n")

p4 <- ggplot(consistency_check, aes(x = utci_max_monthly,
                                    y = max_from_daily,
                                    colour = lga)) +
  geom_point(size = 3.5) +
  geom_abline(slope = 1, intercept = 0,
              linetype = "dashed", colour = "#888780") +
  scale_colour_manual(values = c("Ungogo" = "#D84A38",
                                 "Gabasawa" = "#1D6FA4")) +
  labs(
    title    = "Consistency: daily-derived max vs monthly stats max",
    subtitle = "Points should lie on 45° line if consistent",
    x        = "Monthly stats file: utci_max (°C)",
    y        = "Daily files: max of daily maxima (°C)",
    colour   = NULL,
    caption  = "2025 data only — both sources available for cross-check"
  ) +
  theme_minimal(base_size = 12) +
  theme(legend.position = "top",
        plot.title      = element_text(face = "bold"))

# -- Combine -----------------------------------------------------------

combined <- (p1 + p2) / (p3 + p4) +
  plot_annotation(
    title    = "ERA5-HEAT UTCI — daily extraction QC · Aug 2024 – Apr 2026",
    subtitle = "Ungogo (peri-urban) vs Gabasawa (rural) · Kano",
    theme    = theme(
      plot.title    = element_text(face = "bold", size = 14),
      plot.subtitle = element_text(size = 11)
    )
  )

ggsave(
  filename = file.path(out_dir, "05_era5_daily_qc.png"),
  plot     = combined,
  width    = 14, height = 10, dpi = 300
)

cat("QC plots saved to:", file.path(out_dir, "05_era5_daily_qc.png"), "\n")

#----------------------------------------------------------------------------

###################################
# 8. Save daily series            #
###################################

saveRDS(utci_daily,
        file.path(out_dir, "05_era5_utci_daily.rds"))

write_csv(utci_daily,
          file.path(out_dir, "05_era5_utci_daily.csv"))

cat("Daily series saved.\n")
cat("Rows:", nrow(utci_daily), "\n")
cat("Date range:", format(min(utci_daily$date), "%d %b %Y"),
    "to", format(max(utci_daily$date), "%d %b %Y"), "\n")

#----------------------------------------------------------------------------

###################################
# 9. Decision log                 #
###################################

cat("\n=== DECISION LOG ===\n")
cat("Check the QC summary above:\n\n")
cat("PROCEED to 06_era5_analysis_daily.R if:\n")
cat("  - sd_daily_max >= 4\n")
cat("  - pct_above_38 between 10% and 90% (not near-ceiling or floor)\n")
cat("  - Consistency check shows mean diff < 1°C\n")
cat("  - No large gaps in coverage (< 10 missing days)\n\n")
cat("STOP if:\n")
cat("  - pct_above_38 > 90% — near-ceiling, no variation to exploit\n")
cat("  - Many implausible values — data quality issue\n")
cat("  - Large gaps in coverage — check CDS pull\n")
cat("===================\n")
cat("\n--- Script complete ---\n")