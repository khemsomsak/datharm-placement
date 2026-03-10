########################################################
#  Import and Clean Sample MCHTrack Datharm Data       #
#        and Geospatial Datasets                       #
#  Created on 2/3/2026                                 #
#  Last Updated 2/3/2026                               #
########################################################

# Reset environment -----------------------------------------------------
  
  rm(list = ls())
  setwd("C:/Users/HP/Documents/GitHub/datharm-placement")
  
  #For downloading from WorldPop FTP
  install.packages("devtools")
  devtools::install_github("wpgp/wpgpDownloadR")
  #For downloading from NASA Earth Data
  install.packages("earthdatalogin")
  devtools::install_github("boettiger-lab/earthdatalogin")
  #For downloading from ACLED
  install.packages("remotes")
  remotes::install_gitlab("dickoa/racled")
  
  #For working with rasters
  install.packages("sf")
  install.packages("terra")
  
  library(devtools)
  library(wpgpDownloadR)
  library(earthdatalogin)
  library(remotes)
  library(sf)
  library(terra)
  
  #Routine Packages
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

# 1. Import tabs from sample MCHTrack dataset ----------------------------------

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


# 2. Download WorldPop Data ----------------------------------------------------

  #Explore WorldPop Data
  wpgpListCountries()  
  
  #List all WorldPop covariate rasters for Nigeria
  nga_datasets <- wpgpListCountryDatasets(ISO3 = "NGA")
  head(nga_datasets[, c("CvtName", "Description")])
  
# 3. Download NASA Data ----------------------------------------------------
  
  #Get user and login details
  edl_netrc()
  
  #Vector of IMERG file URLs for your period of interest (from GES DISC search)
  url_nasa <- ("https://gpm1.gesdisc.eosdis.nasa.gov/data/GPM_L3/GPM_3IMERGM.07/")
  
  #Read Nigeria states and keep Katsina & Kano
  nga_states <- st_read("path/to/nigeria_states.gpkg")
  states  <- nga_states[nga_states$STATE_NAME %in% c("Katsina", "Kano"), ]

  #Loop over IMERG files: open remotely, crop, summarize
  results <- lapply(url_nasa, function(u) {
    r <- rast(u, vsi = TRUE)               # open IMERG as SpatRaster
    r <- rast(r, 1)                        # pick the precipitation variable/band
    kk <- st_transform(c("Katsina", "Kano"), crs(r))
    r_kk <- mask(crop(r, vect(kk)), vect(kk))
    stats <- extract(r_kk, vect(kk), fun = mean, na.rm = TRUE)
    data.frame(date = as.Date(get_z(r)),   # depends on how time is stored in IMERG file
               state = kk$STATE_NAME,
               pr   = stats$layer)
  })
  
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
    