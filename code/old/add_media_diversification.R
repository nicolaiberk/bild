# ______________________________________________
# Media framing effects
# Goal: Visualise media diversification
# Procedure: load data, visualise data
# ______________________________________________
# Date:  Wed Feb 02 16:52:43 2022
# Author: Nicolai Berk
#  R version 4.1.1 (2021-08-10)
# ______________________________________________


# Setup ####
library(tidyverse)
library(dplyr)
library(here)
library(data.table)
library(haven)

# load data ####

## URL: https://data.gesis.org/sharing/vaadinServlet/APP/connector/3/678/dl/MA_Zeitschriften_gesamt_54_09_MLFZ.sav.rar
MA_Zeit <- 
  subset(read_sav("data/media_markets/MA_Pressemedien_gesamt_54_09_MLFZ.sav"))


## URL: https://data.gesis.org/sharing/vaadinServlet/APP/connector/3/718/dl/MA_Pressemedien_gesamt_54_09_MLFZ.7z


# visualise data ####
