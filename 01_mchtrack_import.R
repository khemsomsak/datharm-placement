########################################################
#  MCHTrack Data Import, Cleaning & Panel Construction #                           #
#  Created on 14/5/2026                                #
#  Last Updated 27/5/2026                              #
########################################################

# Reset environment -----------------------------------------------------
  
  rm(list = ls())
  setwd("C:/Users/HP/Documents/GitHub/datharm-placement")
  
  # Turn off scientific notation globally
  options(scipen = 999)
  
  #Set link shortcuts
  home    <- "C:/Users/HP/Documents/GitHub/datharm-placement"
  raw_dir <- file.path(home, "02_data/02_mchtrack")
  out_dir <- file.path(home, "03_output/01_mchtrack_data")
  dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)
  
  #Define analysis window
  window_start <- as.Date("2024-08-01")
  window_end   <- as.Date("2026-04-30")
  
  #Routine Packages
  library(janitor)
  library(tidyverse)
  library(dplyr)
  library(tidyr)
  library(ggplot2)
  library(readr)
  library(readxl)
  library(lubridate)

#----------------------------------------------------------------------------
  
###################
# Import Database # 
###################
  
# 1. Import facility visits ----------------------------------------------  
  
  data_fv_kan <- read_xlsx(file.path(raw_dir, "kan_facilityVisits.xlsx")) %>%
    clean_names() %>%
    mutate(state = "Kano")
  
  data_fv_kat <- read_xlsx(file.path(raw_dir, "kat_FacilityVisits.xlsx")) %>%
    clean_names() %>%
    mutate(state = "Katsina")

# 2. Import Linelisted patients ----------------------------------------------  

  data_ll_kan <- read_xlsx(file.path(raw_dir, "kan_lineListed.xlsx")) %>%
    clean_names() %>%
    mutate(state = "Kano")
  
  data_ll_kat <- read_xlsx(file.path(raw_dir, "kat_LineListed.xlsx")) %>%
    clean_names() %>%
    mutate(state = "Katsina")
  
# 3. Import Identified zero-dose patients ----------------------------------------------  

  data_zd_kan <- read_xlsx(file.path(raw_dir, "kan_identifiedZd.xlsx")) %>%
    clean_names() %>%
    mutate(state = "Kano")
  
  data_zd_kat <- read_xlsx(file.path(raw_dir, "kat_identifiedZd.xlsx")) %>%
    clean_names() %>%
    mutate(state = "Katsina")  
  
# 4. Import defaulter tracing ----------------------------------------------  

  data_dt_kan <- read_xlsx(file.path(raw_dir, "kan_defaulterTracing.xlsx")) %>%
    clean_names() %>%
    mutate(state = "Kano")
  
  data_dt_kat <- read_xlsx(file.path(raw_dir, "kat_defaulterTracing.xlsx")) %>%
    clean_names() %>%
    mutate(state = "Katsina")
  
# 5. Rowcounts and other sanity checks ----------------------------------------------  
  
  cat("Raw import row counts:\n")
  cat("  facility_visits: kan =", nrow(data_fv_kan), "| kat =", nrow(data_fv_kat), "\n")
  cat("  linelisted:      kan =", nrow(data_ll_kan), "| kat =", nrow(data_ll_kat), "\n")
  cat("  identifiedZd:    kan =", nrow(data_zd_kan), "| kat =", nrow(data_zd_kat), "\n")
  cat("  defaulterTracing:kan =", nrow(data_dt_kan), "| kat =", nrow(data_dt_kat), "\n\n")
  
  
############
# Analysis # 
############  
  
# 1. Define functions for processing variables to analyzable format ------------  
  
  #Parse hf_distance from text "0.43 KM" → numeric ----
  parse_hf_distance <- function(x) {
    as.numeric(str_remove_all(x, "[^0-9.]"))
  }
  
  #Parse age-at-enrollment from composite text "0years, 3months, 0weeks" ----
  parse_age_months <- function(x) {
    years  <- as.numeric(str_extract(x, "(?<=^|\\s)(\\d+)(?=year)"))
    months <- as.numeric(str_extract(x, "(\\d+)(?=month)"))
    weeks  <- as.numeric(str_extract(x, "(\\d+)(?=week)"))
    (replace_na(years, 0) * 12) +
      replace_na(months, 0) +
      round(replace_na(weeks, 0) / 4.33, 1)
  }
  
  #Parse vaccines_administered json array → tidy list column ----
  # kept as character for now; parse when needed in analysis
  # e.g. '["Penta_1","PCV_1"]' → character string, cleaned of brackets/quotes
  clean_vaccines <- function(x) {
    str_remove_all(x, '[\\[\\]\\"]') %>% str_trim()
  }
  
  #Standardise lga names → consistent title case with "LGA" suffix ----
  clean_lga_name <- function(x) {
    x %>%
      str_to_title() %>%
      str_replace("(?i)\\s*lga$", " LGA") %>%
      str_trim()
  }
  
  #Clean_ward_name: standardise ward name formatting and fix known MCHTrack errors ----
  clean_ward_name <- function(x) {
    x %>%
      str_to_title() %>%
      str_trim() %>%
      str_replace("(?i)\\s+(ward|wrd|wad|ard)\\s*(ward)?$", "") %>%
      str_trim() %>%
      str_replace("(?i)^Garindanga.*",      "Garun Danga") %>%
      str_replace("(?i)^Rijiyar$",          "Rijiyar Zaki") %>%
      str_replace("(?i)^Kumbots$",          "Kumbotso") %>%
      str_replace("(?i)^D/Tofa$",           "Dawakin Tofa") %>%
      str_replace("(?i)^Yautar Arewa$",     "Yantar Arewa") %>%
      str_replace("(?i)^Kadawa Kadawa$",    "Kadawa") %>%
      str_trim()
  }
  
# 2. Clean and stack facility_visits -----------------------------------------------  
  
  #Stack kano and katsina ----
  data_fv_raw <- bind_rows(data_fv_kan, data_fv_kat)
  
  #Clean and type-cast using defined functions ----
  data_fv_clean <- data_fv_raw %>%
    mutate(
      visit_date           = as.Date(visit_date),
      lga_name             = clean_lga_name(lga_name),
      facility_ward        = clean_ward_name(facility_ward),
      woman_or_child       = str_to_lower(str_trim(woman_or_child)),
      track                = str_to_lower(str_trim(track)),
      vaccines_administered = clean_vaccines(vaccines_administered),
      # flag immunization visits (track may be composite e.g. "immunization sick_newborn")
      is_immunization      = str_detect(track, "immunization"),
      # rimi lga backfill flag — use in sensitivity analysis
      rimi_flag            = (lga_name == "Rimi LGA")
    ) %>%
    # apply analysis window
    filter(
      !is.na(visit_date),
      visit_date >= window_start,
      visit_date <= window_end
    ) %>%
    select(
      state, lga_id, lga_name, facility_ward,
      visit_date, track, is_immunization,
      woman_or_child, patient_id,
      health_center_id, health_center_name,
      vaccines_administered, rimi_flag
    )
  
  #Validate and sanity check ----
  cat("facility_visits after cleaning:\n")
  cat("  Total rows:", nrow(data_fv_clean), "\n")
  cat("  Immunization rows:", sum(data_fv_clean$is_immunization, na.rm = TRUE), "\n")
  cat("  Date range:", as.character(min(data_fv_clean$visit_date)),
      "to", as.character(max(data_fv_clean$visit_date)), "\n")
  cat("  States:", paste(unique(data_fv_clean$state), collapse = ", "), "\n")
  cat("  LGAs (", n_distinct(data_fv_clean$lga_name), "):",
      paste(sort(unique(data_fv_clean$lga_name)), collapse = ", "), "\n\n")
  
# 3. Clean and stack linelisted -----------------------------------------------  
  
  #Stack ----
  data_ll_raw <- bind_rows(data_ll_kan, data_ll_kat)
  
  #Clean ----
  data_ll_clean <- data_ll_raw %>%
    mutate(
      registration_date = as.Date(registration_date),
      lga_name          = clean_lga_name(lga_name),
      facility_ward     = clean_ward_name(facility_ward),
      woman_or_child    = str_to_lower(str_trim(woman_or_child)),
      gender            = str_to_lower(str_trim(gender)),
      zero_dose         = as.logical(zero_dose),
      hf_distance_km    = parse_hf_distance(hf_distance),
      # exclude extreme distance values (gps errors confirmed >100km)
      hf_distance_km    = if_else(hf_distance_km > 100, NA_real_, hf_distance_km),
      rimi_flag         = (lga_name == "Rimi LGA")
    ) %>%
    select(
      state, pseudo_id, gender, woman_or_child,
      age_weeks, age_months, age_years,
      zero_dose, reasons_for_zd,
      registration_date,
      settlement_id, settlement_name,
      health_center_id, health_center_name,
      lga_id, lga_name, facility_ward,
      hf_distance_km, rimi_flag
    )
  
  # Add further definitions of Zero Dose, already in data_ll_clean as zero_dose == TRUE
  
  # Definition 2: truly zero dose — no record of ANY antigen in facility visits
  children_with_any_vaccine <- data_fv_clean %>%
    filter(woman_or_child == "child") %>%
    filter(!is.na(vaccines_administered) & vaccines_administered != "") %>%
    distinct(patient_id) %>%
    mutate(has_any_vaccine_record = TRUE)
  
  data_ll_children <- data_ll_clean %>%
    filter(woman_or_child == "child") %>%
    left_join(children_with_any_vaccine, by = c("pseudo_id" = "patient_id")) %>%
    mutate(
      # Definition 1: existing flag
      zero_dose_penta   = zero_dose,
      # Definition 2: truly zero dose — no vaccine of any kind in MCHTrack records
      # caveat: does not capture off-network vaccination
      zero_dose_truly   = is.na(has_any_vaccine_record),
      # Definition 4: age-flexible — no penta-1 regardless of age
      zero_dose_ageflex = (zero_dose_penta | age_months < 12)
    )
  
  #Validate ----
  cat("linelisted after cleaning:\n")
  cat("  Total rows:", nrow(data_ll_clean), "\n")
  cat("  Children:", sum(data_ll_clean$woman_or_child == "child", na.rm = TRUE), "\n")
  cat("  Zero-dose (TRUE):", sum(data_ll_clean$zero_dose == TRUE, na.rm = TRUE), "\n")
  cat("  hf_distance nulls after exclusion:",
      sum(is.na(data_ll_clean$hf_distance_km)), "\n")
  cat("  hf_distance median (km):",
      round(median(data_ll_clean$hf_distance_km, na.rm = TRUE), 2), "\n")
  cat("  LGAs:", n_distinct(data_ll_clean$lga_name), "\n\n")
  
# 4. Clean and stack identified Zero-dose -----------------------------------------------  
  
  #Stack — note kano has geolocation column, katsina does not; handled by bind_rows
  data_zd_raw <- bind_rows(data_zd_kan, data_zd_kat)
  
  #Clean ----
  data_zd_clean <- data_zd_raw %>%
    mutate(
      visit_date        = as.Date(visit_date),
      lga_name          = clean_lga_name(lga_name),
      facility_ward     = clean_ward_name(facility_ward),
      woman_or_child    = str_to_lower(str_trim(woman_or_child)),
      gender            = str_to_lower(str_trim(gender)),
      zero_dose         = as.logical(zero_dose),
      hf_distance_km    = parse_hf_distance(hf_distance),
      hf_distance_km    = if_else(hf_distance_km > 100, NA_real_, hf_distance_km),
      # parse composite age text into numeric months
      age_at_enroll_months = parse_age_months(age_at_enrollment),
      age_current_months   = parse_age_months(estimated_current_age),
      vaccines_administered = clean_vaccines(vaccines_administered),
      rimi_flag         = (lga_name == "Rimi LGA")
    ) %>%
    select(
      state, id, gender, woman_or_child,
      age_at_enroll_months, age_current_months,
      zero_dose, reasons_for_zd,
      visit_date, track, tracing_outcome, reasons_for_defaulting,
      settlement_name, health_center_id, health_center_name,
      lga_id, lga_name, facility_ward,
      hf_distance_km, vaccines_administered, rimi_flag
    )
  
  #Validate ----
  cat("identifiedZd after cleaning:\n")
  cat("  Total rows:", nrow(data_zd_clean), "\n")
  cat("  Zero-dose TRUE:", sum(data_zd_clean$zero_dose == TRUE, na.rm = TRUE), "\n")
  cat("  Visit date non-null:", sum(!is.na(data_zd_clean$visit_date)), "\n")
  cat("  Age at enrollment: median",
      round(median(data_zd_clean$age_at_enroll_months, na.rm = TRUE), 1),
      "months\n")
  cat("  LGAs:", n_distinct(data_zd_clean$lga_name), "\n\n")
  
# 5. Clean and stack defaulterTracing -----------------------------------------------  
  
  #Stack ----
  data_dt_raw <- bind_rows(data_dt_kan, data_dt_kat)
  
  #Clean ----
  data_dt_clean <- data_dt_raw %>%
    mutate(
      created_on        = as.Date(created_on),
      lga_name          = clean_lga_name(lga_name),
      facility_ward     = clean_ward_name(facility_ward),
      tracing_outcome   = str_to_lower(str_trim(tracing_outcome)),
      tracing_method    = str_to_lower(str_trim(tracing_method)),
      hf_distance_km    = parse_hf_distance(hf_distance),
      hf_distance_km    = if_else(hf_distance_km > 100, NA_real_, hf_distance_km),
      rimi_flag         = (lga_name == "Rimi LGA")
    ) %>%
    filter(
      !is.na(created_on),
      created_on >= window_start,
      created_on <= window_end
    ) %>%
    select(
      state, id, patient_id,
      created_on, tracing_outcome, tracing_method,
      reasons_for_defaulting, continuing_care,
      age_weeks, age_months, age_years,
      settlement_name, settlement_id,
      health_center_id, lga_id, lga_name, facility_ward,
      hf_distance_km, rimi_flag
    )
  
  #Validate ----
  cat("defaulterTracing after cleaning:\n")
  cat("  Total rows:", nrow(data_dt_clean), "\n")
  cat("  Date range:", as.character(min(data_dt_clean$created_on)),
      "to", as.character(max(data_dt_clean$created_on)), "\n")
  cat("  Tracing outcomes:\n")
  print(data_dt_clean %>% count(tracing_outcome) %>% arrange(desc(n)))
  cat("\n")
  

#----------------------------#LGA Level#--------------------------------#

# 6. Build LGA x Month panel datasets -----------------------------------------------  
  
  #Calculate immunization visit counts per LGA-month ----
  data_panel_lga_visits <- data_fv_clean %>%
    filter(is_immunization, woman_or_child == "child") %>%
    mutate(year_month = format(visit_date, "%Y-%m"),
           year_month_date = ceiling_date(
             as.Date(paste0(year_month, "-01")), "month") - days(1)) %>%
    group_by(state, lga_id, lga_name, year_month, year_month_date, rimi_flag) %>%
    summarise(
      imm_visits       = n(),
      unique_patients  = n_distinct(patient_id),
      .groups = "drop"
    )
  
# 7. Calculate enrolled children per LGA each month ----
  
  #Create dataset with only linelisted children
  data_ll_children <- data_ll_clean %>%
    filter(woman_or_child == "child") %>%
    select(state, pseudo_id, lga_id, lga_name, facility_ward,
           registration_date, zero_dose, hf_distance_km)
  
  #Build a lookup of all lga × month combinations needed ----
  data_lga_denominator  <- data_panel_lga_visits %>%
    distinct(state, lga_id, lga_name, year_month,year_month_date)  %>%
    #For each lga-month, count children registered up to and including that month ----
    rowwise() %>%
      mutate(
        enrolled_children = sum(
          data_ll_children$lga_id == lga_id &
            data_ll_children$registration_date <= year_month_date,
          na.rm = TRUE
        )
      ) %>%
      ungroup()
  
# 8. Defaulter counts per LGA-month ----
  
  data_panel_lga_defaulters <- data_dt_clean %>%
    mutate(year_month = format(created_on, "%Y-%m")) %>%
    group_by(state, lga_id, lga_name, year_month) %>%
    summarise(
      traced_total      = n(),
      traced_successful = sum(tracing_outcome == "yes_ok", na.rm = TRUE),
      .groups = "drop"
    )
  
# 9. Merge into full LGA monthly panel ----
  
  #Expand grid first so all LGA × month combos exist even with zero visits
  all_lgas   <- unique(data_panel_lga_visits[, c("state","lga_id","lga_name","rimi_flag")])
  all_months <- data.frame(
    year_month      = format(seq(window_start, window_end, by = "month"), "%Y-%m"),
    year_month_date = ceiling_date(seq(window_start, window_end, by = "month"), "month") - days(1)
  )
  
  #Static lga-level control for avg. health facility dist (do not vary by month) ----
  data_lga_controls <- data_ll_clean %>%
    filter(woman_or_child == "child") %>%
    group_by(state, lga_id, lga_name) %>%
    summarise(
      mean_hf_dist_km = round(mean(hf_distance_km, na.rm = TRUE), 3),
      .groups = "drop"
    )
  
  # join panel data
  data_panel_lga <- all_lgas %>%
    cross_join(all_months) %>%
    left_join(data_panel_lga_visits,     
              by = c("state","lga_id","lga_name","year_month","year_month_date","rimi_flag")) %>%
    left_join(data_panel_lga_defaulters, 
              by = c("state","lga_id","lga_name","year_month")) %>%
    left_join(data_lga_denominator,      
              by = c("state","lga_id","lga_name","year_month","year_month_date")) %>%
    left_join(data_lga_controls,         
              by = c("state","lga_id","lga_name")) %>%
    mutate(
      imm_visits        = replace_na(imm_visits, 0L),
      unique_patients   = replace_na(unique_patients, 0L),
      traced_total      = replace_na(traced_total, 0L),
      traced_successful = replace_na(traced_successful, 0L),
      in_window = case_when(
        state == "Katsina" & year_month_date > as.Date("2025-09-30") ~ FALSE,
        TRUE ~ TRUE
      )
    ) %>%
    filter(in_window) %>%
    select(-in_window) %>%
    arrange(state, lga_name, year_month)
  

#----------------------------#Ward Level#--------------------------------#
  
  
# 10. Build Ward x Month panel datasets ----
  
  #Immunization visits per ward-month ----
  data_panel_ward_visits <- data_fv_clean %>%
    filter(is_immunization, woman_or_child == "child", !is.na(facility_ward)) %>%
    mutate(
      year_month      = format(visit_date, "%Y-%m"),
      year_month_date = ceiling_date(as.Date(paste0(year_month, "-01")), "month") - days(1)
    ) %>%
    group_by(state, lga_id, lga_name, facility_ward, year_month, year_month_date, rimi_flag) %>%
    summarise(
      imm_visits      = n(),
      unique_patients = n_distinct(patient_id),
      .groups = "drop"
    )
  
  #Time-varying enrolled children per ward-month ----
  data_ward_denominator <- data_panel_ward_visits %>%
    distinct(state, lga_id, lga_name, facility_ward, year_month, year_month_date) %>%
    rowwise() %>%
    mutate(
      enrolled_children = sum(
        data_ll_children$lga_id        == lga_id        &
          data_ll_children$facility_ward == facility_ward &
          data_ll_children$registration_date <= year_month_date,
        na.rm = TRUE
      )
    ) %>%
    ungroup()
  
  #Static ward-level control: mean hf_distance ----
  data_ward_controls <- data_ll_clean %>%
    filter(woman_or_child == "child", !is.na(facility_ward)) %>%
    group_by(state, lga_id, lga_name, facility_ward) %>%
    summarise(
      mean_hf_dist_km = round(mean(hf_distance_km, na.rm = TRUE), 3),
      .groups = "drop"
    )
  
  #Expand grid to all ward × month combinations ----
  all_wards  <- unique(data_panel_ward_visits[, c("state","lga_id","lga_name","facility_ward","rimi_flag")])
  
  data_panel_ward <- all_wards %>%
    cross_join(all_months) %>%
    left_join(data_panel_ward_visits, 
              by = c("state","lga_id","lga_name","facility_ward","year_month","year_month_date","rimi_flag")) %>%
    left_join(data_ward_denominator,  
              by = c("state","lga_id","lga_name","facility_ward","year_month","year_month_date")) %>%
    left_join(data_ward_controls,     
              by = c("state","lga_id","lga_name","facility_ward")) %>%
    mutate(
      imm_visits      = replace_na(imm_visits, 0L),
      unique_patients = replace_na(unique_patients, 0L),
      in_window = case_when(
        state == "Katsina" & year_month_date > as.Date("2025-09-30") ~ FALSE,
        TRUE ~ TRUE
      )
    ) %>%
    filter(in_window) %>%
    select(-in_window) %>%
    arrange(state, lga_name, facility_ward, year_month)
  
  
# 11. Set threshold for consolidating wards that are subdivisions ----
  
  #Flag wards below minimum monthly visit threshold ----
  ward_threshold <- 10  # minimum avg visits/month to be treated as distinct unit
  
  ward_volume <- data_panel_ward %>%
    group_by(state, lga_name, facility_ward) %>%
    summarise(avg_monthly_visits = mean(imm_visits), .groups = "drop")
  
  data_panel_ward <- data_panel_ward %>%
    left_join(ward_volume, by = c("state","lga_name","facility_ward")) %>%
    mutate(
      ward_consolidated = if_else(
        avg_monthly_visits < ward_threshold,
        str_remove(facility_ward, "\\s+[ABC123]$") %>% str_trim(),
        facility_ward
      )
    )
  
  #Re-aggregate on consolidated ward label ----
  # rows that were separate sub-units now share the same ward_consolidated name
  # and must be summed together per LGA-month
  data_panel_ward_consolidated <- data_panel_ward %>%
    group_by(state, lga_id, lga_name, ward_consolidated, year_month, year_month_date, rimi_flag) %>%
    summarise(
      imm_visits        = sum(imm_visits,        na.rm = TRUE),
      unique_patients   = sum(unique_patients,   na.rm = TRUE),
      enrolled_children = sum(enrolled_children, na.rm = TRUE),
      mean_hf_dist_km   = mean(mean_hf_dist_km,  na.rm = TRUE),
      .groups = "drop"
    ) %>%
    rename(facility_ward = ward_consolidated)
  
# 12. Validation ----
  
  data_panel_ward %>%
    summarise(
      total_rows         = n(),
      unique_wards       = n_distinct(paste(lga_name, facility_ward)),
      unique_months      = n_distinct(year_month),
      zero_visit_cells   = sum(imm_visits == 0),
      zero_pct           = round(zero_visit_cells / total_rows * 100, 1),
      rimi_ward_rows     = sum(rimi_flag, na.rm = TRUE)
    ) %>%
    print()
  
  data_panel_ward %>%
    distinct(state, lga_name, facility_ward) %>%
    count(state, name = "n_wards") %>%
    print()

  #----------------------------#Dekadal LGA Panel#--------------------------------#
  
# 13. Build LGA x Dekad panel (supplementary — for dekadal regression) ----
  
  # immunization visits per LGA-dekad
  data_panel_lga_dekad_visits <- data_fv_clean %>%
    filter(is_immunization, woman_or_child == "child") %>%
    mutate(
      year_month = format(visit_date, "%Y-%m"),
      dekad_num  = case_when(
        day(visit_date) <= 10 ~ "D1",
        day(visit_date) <= 20 ~ "D2",
        TRUE                  ~ "D3"
      ),
      dekad_id = paste0(year_month, "-", dekad_num)
    ) %>%
    group_by(state, lga_id, lga_name, year_month, dekad_id, dekad_num, rimi_flag) %>%
    summarise(
      imm_visits      = n(),
      unique_patients = n_distinct(patient_id),
      .groups = "drop"
    )
  
  # expand grid: all LGA × dekad combos
  all_dekads <- data.frame(
    dekad_id = c(outer(
      format(seq(window_start, window_end, by = "month"), "%Y-%m"),
      c("-D1","-D2","-D3"),
      paste0
    ))
  ) %>%
    mutate(
      year_month = str_sub(dekad_id, 1, 7),
      dekad_num  = str_sub(dekad_id, 9, 10)
    )
  
  data_panel_lga_dekad <- all_lgas %>%
    cross_join(all_dekads) %>%
    left_join(data_panel_lga_dekad_visits,
              by = c("state","lga_id","lga_name","year_month","dekad_id","dekad_num","rimi_flag")) %>%
    left_join(data_lga_controls, by = c("state","lga_id","lga_name")) %>%
    mutate(
      imm_visits      = replace_na(imm_visits, 0L),
      unique_patients = replace_na(unique_patients, 0L),
      in_window = case_when(
        state == "Katsina" & year_month > "2025-09" ~ FALSE,
        TRUE ~ TRUE
      )
    ) %>%
    filter(in_window) %>%
    select(-in_window) %>%
    arrange(state, lga_name, dekad_id)
  
  # validate
  data_panel_lga_dekad %>%
    summarise(
      total_rows    = n(),
      unique_dekads = n_distinct(dekad_id),
      unique_lgas   = n_distinct(lga_name),
      zero_cells    = sum(imm_visits == 0)
    ) %>%
    print()
  
  
##########
# Export # 
##########

  #Save assembled panels as native R object instead of CSV
  saveRDS(data_panel_lga,              
          file.path(out_dir, "01_panel_lga_month.rds"))
  saveRDS(data_panel_ward,              
          file.path(out_dir, "01_panel_ward_month_full.rds"))
  saveRDS(data_panel_ward_consolidated, 
          file.path(out_dir, "01_panel_ward_month_consolidated.rds"))
  saveRDS(data_panel_lga_dekad,
          file.path(out_dir, "01_panel_lga_dekad.rds"))
  
  #Save cleaned and stacked MCHTrack tables
  saveRDS(data_fv_clean, 
          file.path(out_dir, "01_facility_visits_clean.rds"))
  saveRDS(data_ll_clean, 
          file.path(out_dir, "01_linelisted_clean.rds"))
  saveRDS(data_zd_clean, 
          file.path(out_dir, "01_identifiedzd_clean.rds"))
  saveRDS(data_dt_clean, 
          file.path(out_dir, "01_defaultertracing_clean.rds"))
  

  
#--------------------------(END)------------------------------#
  
  