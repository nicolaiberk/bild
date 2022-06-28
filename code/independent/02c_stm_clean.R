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

load(here('data/mig_articles_topics2022-02-02.Rdata'))
load(here('data/salience_cleaned.csv'))

# 2. transform ####

## 2.1 framing ####

## estimate absolute frame attention
framing_daily <- 
  output %>% 
  select(-date, -title, -url, -text) %>% 
  group_by(date_new, paper) %>% 
  summarise_all(sum) %>%
  as_tibble()


## estimate n of mig articles in frame set
framing_daily$n_mig_fra <- 
  output %>% 
  group_by(date_new, paper) %>% 
  summarise(n_mig_fra = n()) %>% 
  ungroup %>% 
  select(n_mig_fra) %>% 
  unlist()

rm(output)


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
  mutate(across(english:hungary_referendum, ~is.na(.x), .names = "na_{.col}")) %>% 
  mutate(across(na_english:na_hungary_referendum, ~ifelse(NA, 0, .x), .names = "{.col}")) %>% 
  select(!starts_with("na_")) %>% 
  ## add frame attention as share of migration articles
  mutate(across(english:hungary_referendum, ~(.x/n_mig_sal), .names = "{.col}_share" )) %>% 
  mutate(paper = 
           case_when(paper %in% c("weltonline", "welt") ~ "Welt",
                     paper == "taz"  ~ "TAZ",
                     paper == "spon" ~ "Spiegel",
                     paper == "faz"  ~ "FAZ",
                     paper == "sz"   ~ "SZ",
                     paper == "bild" ~ "Bild"))



# 3. save ####

datum <- Sys.Date()
save(merged_media, file = here(paste0("data/media_daily_", datum, ".Rdata")))


