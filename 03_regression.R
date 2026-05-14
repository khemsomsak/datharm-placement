########################################################
#  Regression Analysis for MCHTrack and CHIRPS Data    #
#  Created on 14/5/2026                                #
#  Last Updated 14/5/2026                              #
########################################################

# Reset environment -----------------------------------------------------
  
  rm(list = ls())
  setwd("C:/Users/HP/Documents/GitHub/datharm-placement")
  
  # Turn off scientific notation globally
  options(scipen = 999)
  Sys.setlocale("LC_TIME", "English")
  
  #Set link shortcuts
  home    <- "C:/Users/HP/Documents/GitHub/datharm-placement"
  mchtrack_dir <- file.path(home, "03_output/01_mchtrack_data")
  chirps_dir   <- file.path(home, "03_output/02_chirps_data")
  out_dir      <- file.path(home, "03_output/03_analysis")
  dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)
  
  # packages
  install.packages(c("fixest", "modelsummary", "ggplot2", "scales"))
  
  #Routine Packages
  library(janitor)
  library(tidyverse)
  library(dplyr)
  library(tidyr)
  library(ggplot2)
  library(readr)
  library(lubridate)
  library(broom)
  library(fixest)        # two-way fixed effects via feols()
  library(modelsummary)  # clean regression output tables
  library(scales)        # axis formatting in plots

#----------------------------------------------------------------------------
  
###################
# Import Database # 
###################
  
  #Load MCHTrack LGA panel ----
  data_panel_lga <- readRDS(file.path(mchtrack_dir, "02_panel_lga_month.rds"))
  
  #Load CHIRPS precipitation data ----
  data_chirps <- readRDS(file.path(chirps_dir, "02_chirps_data_kk_monthly.rds"))
                         
                         
  
############
# Analysis # 
############  

  #----------------------------#Data Merge#--------------------------------#    

# 1. Merge MCHTrack panel with CHIRPS ----
  
  #Chirps uses lga_name_mchtrack; mchtrack panel uses lga_name — same values
  data_merged <- data_panel_lga %>%
    left_join(
      data_chirps %>% select(state, lga_name_mchtrack, year_month,
                             precip_actual_mm, precip_longterm_avg_mm,
                             precip_anomaly_pct, dekads_present),
      by = c("state",
             "lga_name" = "lga_name_mchtrack",
             "year_month")
    ) 
  
  #Check merge quality — flag LGA-months with no CHIRPS match
  unmatched <- data_merged %>% filter(is.na(precip_anomaly_pct))
  cat("LGA-months missing CHIRPS data:", nrow(unmatched), "\n")
  if (nrow(unmatched) > 0) print(distinct(unmatched, state, lga_name, year_month))
  cat("\n")
  
# 2. Construct analysis variables ----
  
  data_analysis <- data_merged %>%
    mutate(
      # log visits — standard for count outcomes; add 1 to handle zeros
      log_imm_visits    = log(imm_visits + 1),
      # visit rate per 100 enrolled (only meaningful where enrolled > 0)
      visit_rate        = if_else(enrolled_children > 0,
                                  imm_visits / enrolled_children * 100,
                                  NA_real_),
      # numeric month index for time trend control
      month_index       = as.integer(factor(year_month, levels = sort(unique(year_month)))),
      # lag precipitation anomaly by 1 month (lagged shock)
      precip_anomaly_lag1 = lag(precip_anomaly_pct, 1),
      # exclude rimi lga in primary spec; include as sensitivity
      in_primary_sample = !rimi_flag
    ) %>%
    # trim Katsina to data end
    filter(!(state == "Katsina" & year_month > "2025-09"))
  
  #----------------------------#Descriptive#-------------------------------#
  
# 3. Summary statistics table ----
  
  data_analysis %>%
    filter(in_primary_sample) %>%
    select(imm_visits, log_imm_visits, visit_rate,
           precip_anomaly_pct, precip_actual_mm, mean_hf_dist_km,
           enrolled_children) %>%
    summary() %>%
    print()
  
# 4. Visits over time by state ----
  
  plot_trend <- data_analysis %>%
    filter(in_primary_sample) %>%
    group_by(state, year_month) %>%
    summarise(total_visits = sum(imm_visits), .groups = "drop") %>%
    mutate(date = as.Date(paste0(year_month, "-01")))
  
  ggplot(plot_trend, aes(x = date, y = total_visits, colour = state)) +
    geom_line(linewidth = 1) +
    geom_point(size = 2) +
    scale_colour_manual(values = c("Kano" = "#002147", "Katsina" = "#C6A052")) +
    scale_x_date(date_breaks = "2 months", date_labels = "%b %y") +
    scale_y_continuous(labels = comma) +
    labs(title = "Monthly immunisation visits by state",
         subtitle = "Children only · immunization track · Rimi LGA excluded",
         x = NULL, y = "Total visits", colour = NULL) +
    theme_minimal() +
    theme(legend.position = "top",
          axis.text.x = element_text(angle = 45, hjust = 1))
  
  ggsave(file.path(out_dir, "01_visits_trend.png"),
         width = 9, height = 5, dpi = 150)
  
# 5. Precipitation anomaly over time ----
  
  plot_precip <- data_analysis %>%
    filter(in_primary_sample) %>%
    distinct(state, year_month, precip_anomaly_pct) %>%
    mutate(date = as.Date(paste0(year_month, "-01")))
  
  ggplot(plot_precip, aes(x = date, y = precip_anomaly_pct,
                          fill = precip_anomaly_pct > 0)) +
    geom_col() +
    facet_wrap(~state, ncol = 1) +
    scale_fill_manual(values = c("TRUE" = "#1D9E75", "FALSE" = "#E24B4A"),
                      labels = c("TRUE" = "Above average", "FALSE" = "Below average")) +
    scale_x_date(date_breaks = "2 months", date_labels = "%b %y") +
    labs(title = "Monthly precipitation anomaly (% vs long-term average)",
         x = NULL, y = "Anomaly (%)", fill = NULL) +
    theme_minimal() +
    theme(legend.position = "top",
          axis.text.x = element_text(angle = 45, hjust = 1))
  
  ggsave(file.path(out_dir, "02_precip_anomaly.png"),
         width = 9, height = 6, dpi = 150)
  
# 6. Scatter: precipitation anomaly vs log visits (raw correlation) ----
  
  ggplot(data_analysis %>% filter(in_primary_sample),
         aes(x = precip_anomaly_pct, y = log_imm_visits,
             colour = state, label = lga_name)) +
    geom_point(size = 2.5, alpha = 0.6) +
    geom_smooth(method = "lm", se = TRUE, linewidth = 1) +
    scale_colour_manual(values = c("Kano" = "#002147", "Katsina" = "#C6A052")) +
    labs(title = "Precipitation anomaly vs log immunisation visits",
         subtitle = "Each point = one LGA-month · raw (unadjusted) relationship",
         x = "Precipitation anomaly (% vs LTA)",
         y = "Log immunisation visits",
         colour = NULL) +
    theme_minimal() +
    theme(legend.position = "top")
  
  ggsave(file.path(out_dir, "03_scatter_precip_visits.png"),
         width = 8, height = 6, dpi = 150)
  
  #----------------------------#Regression#--------------------------------#
  
# 7. Model 1: pooled OLS baseline (no fixed effects) ----
  
  m1 <- feols(
    log_imm_visits ~ precip_anomaly_pct + mean_hf_dist_km + log(enrolled_children + 1),
    data = data_analysis %>% filter(in_primary_sample),
    vcov = "hetero"
  )
  
  # 8. Model 2: LGA fixed effects only ----
  
  m2 <- feols(
    log_imm_visits ~ precip_anomaly_pct + mean_hf_dist_km + log(enrolled_children + 1)
    | lga_name,
    data = data_analysis %>% filter(in_primary_sample),
    vcov = "hetero"
  )
  
  # 9. Model 3: two-way fixed effects (primary specification) ----
  
  m3 <- feols(
    log_imm_visits ~ precip_anomaly_pct + mean_hf_dist_km + log(enrolled_children + 1)
    | lga_name + year_month,
    data = data_analysis %>% filter(in_primary_sample),
    vcov = ~lga_name   # cluster SE at LGA level
  )
  
  # 10. Model 4: two-way FE with lagged precipitation (robustness) ----
  
  m4 <- feols(
    log_imm_visits ~ precip_anomaly_lag1 + mean_hf_dist_km + log(enrolled_children + 1)
    | lga_name + year_month,
    data = data_analysis %>% filter(in_primary_sample),
    vcov = ~lga_name
  )
  
  # 11. Model 5: include Rimi LGA (sensitivity check) ----
  
  m5 <- feols(
    log_imm_visits ~ precip_anomaly_pct + mean_hf_dist_km + log(enrolled_children + 1)
    | lga_name + year_month,
    data = data_analysis,   # full sample including Rimi
    vcov = ~lga_name
  )
  
# 12. Print regression table ----
  
  modelsummary(
    list("OLS"        = m1,
         "LGA FE"     = m2,
         "Two-way FE" = m3,
         "Lagged"     = m4,
         "Incl. Rimi" = m5),
    stars      = c("*" = 0.1, "**" = 0.05, "***" = 0.01),
    coef_rename = c(
      "precip_anomaly_pct"             = "Precip anomaly (%)",
      "precip_anomaly_lag1"            = "Precip anomaly lag-1 (%)",
      "mean_hf_dist_km"                = "Mean HF distance (km)",
      "log(enrolled_children + 1)"     = "Log enrolled children"
    ),
    gof_map    = c("nobs","r.squared","adj.r.squared","FE: lga_name","FE: year_month"),
    output     = file.path(out_dir, "04_regression_table.txt")
  )
  
  modelsummary(
    list("OLS" = m1, "LGA FE" = m2, "Two-way FE" = m3,
         "Lagged" = m4, "Incl. Rimi" = m5),
    stars = c("*" = 0.1, "**" = 0.05, "***" = 0.01)
  )
  
# 13. Coefficient plot: primary spec by state ----
  
  # kano: only 3 LGAs so two-way FE is not estimable — use LGA FE only
  # katsina: two-way FE runs but mean_hf_dist_km dropped due to collinearity with LGA FE
  m3_kano <- feols(
    log_imm_visits ~ precip_anomaly_pct | lga_name,
    data = data_analysis %>% filter(in_primary_sample, state == "Kano"),
    vcov = "hetero"
  )
  
  m3_katsina <- feols(
    log_imm_visits ~ precip_anomaly_pct | lga_name + year_month,
    data  = data_analysis %>% filter(in_primary_sample, state == "Katsina"),
    vcov  = ~lga_name
  )
  
  coef_df <- bind_rows(
    tidy(m3_kano,    conf.int = TRUE) %>% mutate(model = "Kano (LGA FE only)"),
    tidy(m3_katsina, conf.int = TRUE) %>% mutate(model = "Katsina (Two-way FE)"),
    tidy(m3,         conf.int = TRUE) %>% mutate(model = "Pooled (Two-way FE)")
  ) %>%
    filter(term == "precip_anomaly_pct")
  
  ggplot(coef_df, aes(x = model, y = estimate,
                      ymin = conf.low, ymax = conf.high,
                      colour = model)) +
    geom_hline(yintercept = 0, linetype = "dashed", colour = "grey50") +
    geom_pointrange(size = 0.8, linewidth = 1) +
    scale_colour_manual(values = c(
      "Kano (LGA FE only)"    = "#002147",
      "Katsina (Two-way FE)"  = "#C6A052",
      "Pooled (Two-way FE)"   = "#1D9E75"
    )) +
    labs(
      title    = "Effect of precipitation anomaly on log immunisation visits",
      subtitle = "Kano: LGA FE only (3 LGAs, two-way FE not estimable)\nKatsina & Pooled: two-way FE · clustered SE by LGA",
      x = NULL, y = "Coefficient estimate", colour = NULL
    ) +
    theme_minimal() +
    theme(legend.position = "none")
  
  ggsave(file.path(out_dir, "05_coef_plot.png"),
         width = 8, height = 5, dpi = 150)
  
  
##########
# Export # 
##########

  # save merged analysis dataset
  saveRDS(data_analysis, file.path(out_dir, "03_analysis_panel_lga.rds"))
  
  
#--------------------------(END)------------------------------#
  
  