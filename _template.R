########################################################
#  Title of the File Project                           #
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
  
# 1. First step of importing data ----------------------------------------------  
  
  
############
# Analysis # 
############  
  
# 1. First step of data analysis -----------------------------------------------  
  

##########
# Export # 
##########

  # Save as native R object instead of CSV
  saveRDS(data_final, file.path(out_dir, ""))
  
  # Preview
  print(data_final)
  
  
#--------------------------(END)------------------------------#
  
  