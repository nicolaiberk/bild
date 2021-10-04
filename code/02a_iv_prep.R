# ______________________________________________
# Effect of framing on issue attitudes
# Goal: Prepare independent variables
# Procedure: load, transform, save
# ______________________________________________
# Date:  Wed Sep 08 14:20:28 2021
# Author: Nicolai Berk
#  R version 4.0.3 (2020-10-10)
# ______________________________________________


# 0. Setup ####
library(tidyverse)
library(dplyr)
library(here)
library(data.table)
library(here)
library(lubridate)
library(glue)

# 1. load ####

## survey data for wave dates
attitude_dids <- read.csv(here('data/coeftable_gles_noimp.csv'))

attitude_dids$paper_name <- ''
attitude_dids$paper_name[attitude_dids$paper == 'a'] <- 'bild'
attitude_dids$paper_name[attitude_dids$paper == 'b'] <- 'falg'
attitude_dids$paper_name[attitude_dids$paper == 'c'] <- 'faz'
attitude_dids$paper_name[attitude_dids$paper == 'd'] <- 'sz'
attitude_dids$paper_name[attitude_dids$paper == 'e'] <- 'taz'
attitude_dids$paper_name[attitude_dids$paper == 'f'] <- 'welt'

load(here('data/frames_cleaned.csv'))
load(here('data/salience_cleaned.csv'))

# 2. transform ####

## 2.1 framing ####

## estimate absolute frame attention
framing_daily <- 
  framing_raw %>% 
  select(-date, -title, -url, -text) %>% 
  group_by(date_new, paper) %>% 
  summarise_all(sum) %>%
  as_tibble()

## estimate n of mig articles in frame set
framing_daily$n_mig_fra <- 
  framing_raw %>% 
  group_by(date_new, paper) %>% 
  summarise(n_mig_fra = n()) %>% 
  ungroup %>% 
  select(n_mig_fra) %>% 
  unlist()

rm(framing_raw)


## 2.2 salience ####

# estimate absolute and relative saleince
salience_daily <- 
  salience_raw %>% 
  group_by(date_new, paper) %>% 
  summarise(
    n_mig_sal = sum(mig),
    n_tot = n()
  ) %>% 
  mutate(
    share_mig = n_mig_sal/n_tot
  ) %>% 
  as_tibble()

rm(salience_raw)

# 2.3 merge ####
merged_media <- 
  salience_daily %>% 
  full_join(framing_daily) %>% 
  ## replace missing values with 0s
  mutate(
    crime    = ifelse(is.na(crime    ), 0, crime    ) ,
    capcrime = ifelse(is.na(capcrime ), 0, capcrime ) ,
    medit    = ifelse(is.na(medit    ), 0, medit    ) ,
    deport   = ifelse(is.na(deport   ), 0, deport   ) ,
    refnums  = ifelse(is.na(refnums  ), 0, refnums  ) ,
    camps    = ifelse(is.na(camps    ), 0, camps    ) ,
    labmar   = ifelse(is.na(labmar   ), 0, labmar   ) 
  ) %>% 
  ## add frame attention as share of mirgation articles
  mutate(
    crime_share    = crime    / n_mig_sal,
    capcrime_share = capcrime / n_mig_sal,
    medit_share    = medit    / n_mig_sal,
    deport_share   = deport   / n_mig_sal,
    refnums_share  = refnums  / n_mig_sal,
    camps_share    = camps    / n_mig_sal,
    labmar_share   = labmar   / n_mig_sal
  )



# 3. save ####

datum <- Sys.Date()
save(merged_media, file = here(paste0("data/media_daily_", datum, ".Rdata")))


