# ______________________________________________
# Bild
# Goal: Prepare dataset for stata estimation
# Procedure: load, prepare, save
# ______________________________________________
# Date:  Mon May 23 12:24:36 2022
# Author: Nicolai Berk
#  R version 4.1.1 (2021-08-10)
# ______________________________________________


# Setup ####
library(tidyverse)
library(dplyr)
library(here)
library(data.table)
library(haven)

# load ####
gles_p_long <- fread(here("data/raw/gles/Panel/long_cleaned.csv"))

# prepare ####
gles_p_long <- 
  gles_p_long %>% 
  mutate(Individual = lfdn,
         dv = `1130_clean`) %>% 
  select(Individual, post, treat, dv) %>% 
  filter(!is.na(post), !is.na(treat), !is.na(dv))

# test regression
lm(dv ~ post*treat, data = gles_p_long)
fixest::feglm(dv ~ post*treat | Individual, data = gles_p_long)


# save ####
write_dta(gles_p_long, here("data/processed/GLES_tobit.dta"))
