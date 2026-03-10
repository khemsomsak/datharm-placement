########################################################
#  Import and Clean Sample MCHTrack Datharm Data       #
#        and Geospatial Datasets                       #
#  Created on 2/3/2026                                 #
#  Last Updated 2/3/2026                               #
########################################################

# Reset environment -----------------------------------------------------
rm(list = ls())
setwd("C:/Users/HP/Documents/GitHub/datharm-placement")

library(janitor)
library(dplyr)
library(tidyr)
library(ggplot2)
library(readr)
library(readxl)
#----------------------------------------------------------------------------
  
###################
# Import Database # 
###################

# Import tabs from sample MCHTrack dataset

  mch_linelisted <- read_xlsx("02_data/01_mchtrack-sample.xlsx", 
                               sheet = "SampleLinelisted") %>%
    clean_names()
  mch_facilityvisit <- read_xlsx("02_data/01_mchtrack-sample.xlsx", 
                                  sheet = "SampleFacility_Visit") %>%
    clean_names()
  mch_idzd <- read_xlsx("02_data/01_mchtrack-sample.xlsx", 
                                  sheet = "SampleIdentified ZD ") %>%
    clean_names()
  mch_defaulttracing <- read_xlsx("02_data/01_mchtrack-sample.xlsx", 
                                  sheet = " SampleDefaulter_tracing") %>%
    clean_names()
  mch_defaulters <- read_xlsx("02_data/01_mchtrack-sample.xlsx", 
                                   sheet = "SampleDefaulters") %>%
    clean_names()

#Import rainfall data from CHIRPS
  rainfall <- read_csv("02_data/03_nga-rainfall-subnat-full.csv") %>%
    clean_names() %>%
    rename(ten_day_rain = rfh,
           ten_day_rain_avg = rfh_avg,
           one_mnth_rain = r1h,
           one_mnth_rain_avg = r1h_avg,
           ten_day_anomaly = rfq,
           one_mnth_anomaly = r1q) %>%
    #Restrict data to only Katsina state pcode = 'NG021'
    filter(pcode == 'NG021') %>%
    #Restrict to date range of MCHTrack program
    filter(date >= '2019-01-01')

#Import conflict data from ACLED
  conflict <- read_csv("02_data/02_acled-data_2026-02-27.csv") %>%
    clean_names()

#Import light intensity data from NASA Worldview
  light <- read_csv("02_data/04_eoatlas-monthly-nightlight-batagarawa.csv") %>%
    clean_names()


############
# Analysis # 
############
  
  #Plot rainfall anomaly in program period
  ggplot(rainfall, aes(x = date, y = one_mnth_anomaly)) +
    geom_line()
  
  #Map area of MCHTrack
  data_linelisted %>% 
    group_by(lga_name) %>%
    summarise(visits_no = n())
  
  data_linelisted %>% 
    group_by(health_center_name) %>%
    summarise(visits_no = n()) %>%
    arrange(-visits_no)
  
  
##########
# Export # 
##########
  
  save(data_facilityvisit,file='/03_output/01_data_factilityvisit.rda')
  
#--------------------------(END)------------------------------#
    