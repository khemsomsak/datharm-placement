########################################################
#  Import and Clean CHIRPS Precipitation Data          #
#  Created on 13/5/2026                                #
#  Last Updated 13/5/2026                              #
########################################################

# Reset environment -----------------------------------------------------
  
  rm(list = ls())
  setwd("C:/Users/HP/Documents/GitHub/datharm-placement")
  
  # Turn off scientific notation globally
  options(scipen = 999)
  
  #Set link shortcuts
  home    <- "C:/Users/HP/Documents/GitHub/datharm-placement"
  raw_dir <- file.path(home, "02_data/03_external")
  out_dir <- file.path(home, "03_output/02_chirps_data")
  dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)
  
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

#----------------------------------------------------------------------------
  
###################
# Import Database # 
###################
  
# 1. Import rainfall CHIRPS dataset --------------------------------------------  

  data_file <- file.path(raw_dir, "nga-rainfall-subnat-5ytd.csv")
  data_raw <- read_csv(data_file, show_col_types = FALSE) %>%
    clean_names()
  
  
############
# Analysis # 
############  
  
# 1. Filter the imported data to only LGAs in Katsina and Kano -----------------  
  
  data_kk <- data_raw %>%
    filter(
      adm_level == 2,
      str_starts(pcode, "NG019") | str_starts(pcode, "NG020")
    )
  
  #Count number of rows and distinct LGA count
  nrow(data_kk)
  n_distinct(data_kk$pcode)

# 2. Aggregate from dekadal to monthly averages --------------------------------  
    
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
      .groups = "drop"
    ) %>%
    #Filter to the program date range (MCHTrack data starts Aug 2024)
    filter(year_month >= "2024-08")
  
  # Flag any months where we have fewer than 3 dekads (incomplete months)
  incomplete <- data_kk_monthly %>% filter(dekads_present < 3)
  if (nrow(incomplete) > 0) {
    cat("WARNING:", nrow(incomplete), "LGA-months have fewer than 3 dekads — check dates\n")
    print(incomplete)
  } else {
    cat("All LGA-months have complete dekadal coverage.\n")
  }
  
  
# 3. Add the LGA name variable ----------
  
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
      dekads_present
    ) %>%
    arrange(state, lga_pcode, year_month)
  
  #Check for any PCODEs that didn't match the lookup
  unmatched <- data_final %>% filter(is.na(lga_name_mchtrack))
  if (nrow(unmatched) > 0) {
    cat("WARNING:", n_distinct(unmatched$lga_pcode), "PCODEs had no name match:\n")
    print(distinct(unmatched, lga_pcode))
  } else {
    cat("All PCODEs matched successfully.\n")
  }
  

  # Save as native R object instead of CSV
  saveRDS(data_final, file.path(out_dir, "02_chirps_data_kk_monthly.rds"))
  
  # Preview
  print(data_final)
  
  
  
#--------------------------(END)------------------------------#
  
  