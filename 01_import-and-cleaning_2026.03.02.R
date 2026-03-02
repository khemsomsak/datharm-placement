########################################################
#  Import and Clean Sample MCHTrack Datharm Data       #
#        and Geospatial Datasets                       #
#  Created on 2/3/2026                                 #
#  Last Updated 2/3/2026                               #
########################################################

# Reset environment -----------------------------------------------------
rm(list = ls())

library(janitor)
library(dplyr)
library(tidyr)
library(ggplot2)
library(readr)
----------------------------------------------------------------------------
  
###################
# Import Database # 
###################

# Import penalties from database
matched_data <- read.csv(header$datasets("mst/import_data.csv"))

############
# Analysis # 
############