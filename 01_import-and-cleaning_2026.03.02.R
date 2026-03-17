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
  
  #Variable name check
  

# 1. Import and merge MCHTrack Kano + Katsina dataset --------------------------

  #Line listed
    mch_linelisted <- rbind(
      read.csv("02_data/01_mchtrack_kano/linelisted.csv") %>%
        mutate(dataset = "kano") %>%
        clean_names(),
      read.csv("02_data/02_mchtrack_katsina/linelisted.csv") %>%
        mutate(dataset = "katsina") %>%
        rename(distance_to_hf = hf_distance) %>%
        clean_names()
    )
  
  #Identified Zero-Dose
  
    #Import Kano and Katsina individually
    mch_idzd_kano <- read.csv("02_data/01_mchtrack_kano/identifiedzd.csv") %>%
      mutate(dataset = "kano") %>%
      clean_names()
    mch_idzd_katsina <- read.csv("02_data/02_mchtrack_katsina/identifiedzd.csv") %>%
      mutate(dataset = "katsina") %>%
      clean_names()
    
    #Identify non-overlapping variables
    setdiff(variable.names(mch_idzd_kano),
      variable.names(mch_idzd_katsina))
    setdiff(variable.names(mch_idzd_katsina),
            variable.names(mch_idzd_kano))
    
    #Combine overlapping variables 
    mch_idzd <- bind_rows(
      mch_idzd_kano %>% select(all_of(intersect(names(mch_idzd_kano), names(mch_idzd_katsina)))),
      mch_idzd_katsina %>% select(all_of(intersect(names(mch_idzd_kano), names(mch_idzd_katsina))))
    )
  
  #Defaulter
      
    #Import 
    mch_defaulter_kano <- read.csv("02_data/01_mchtrack_kano/defaulter.csv") %>%
      mutate(dataset = "kano") %>%
      clean_names()
    mch_defaulter_katsina <- read.csv("02_data/02_mchtrack_katsina/defaulter.csv") %>%
      mutate(dataset = "katsina") %>%
      clean_names()
    
    setdiff(variable.names(mch_defaulter_kano),
            variable.names(mch_defaulter_katsina))
    setdiff(variable.names(mch_defaulter_katsina),
            variable.names(mch_defaulter_kano))
    
  #Defaulter Tracing
    
    #Import Kano and Katsina individually
    mch_defaulter_tracing_kano <- read.csv("02_data/01_mchtrack_kano/defaulter-tracing.csv") %>%
      mutate(dataset = "kano") %>%
      clean_names()
    mch_defaulter_tracing_katsina <- read.csv("02_data/02_mchtrack_katsina/defaulter-tracing.csv") %>%
      mutate(dataset = "katsina") %>%
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
 
  #Data checks on linelisted, there are 89,177 distinct IDs.
  nrow(mch_linelisted)
  
  mch_linelisted %>%
    select(pseudo_id) %>% 
    distinct() %>%
    nrow()
  
  mch_linelisted %>%
    group_by(dataset,zero_dose) %>%
    summarise(count = n())
  
  #Data checks on overlapping ID variable between tables
  
  #Example of overlapping ID - bc93bc7d-bd9d-4ae6-a966-b711002dc2b5
  intersect(
    mch_idzd %>% 
      select(pseudo_id),
    mch_linelisted %>%
      filter(zero_dose == TRUE) %>%
      select(pseudo_id)
    )
  
  #Example of ID in linelisted but NOT idzd - 7649da34-c09c-4f0d-8793-b4f1cd5905c2
  setdiff(
    mch_linelisted %>%
      filter(zero_dose == TRUE) %>%
      select(pseudo_id),
    mch_idzd %>% 
      select(pseudo_id)
    )
  
  #Try left-joining
  left_join()
  
  
  
   
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
    