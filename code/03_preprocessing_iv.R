# ______________________________________________
# Medoia effects on opinion formation
# Goal: Preprocess media data
# Procedure: load, generate daily attention table
# ______________________________________________
# Date:  Mon Sep 20 11:58:18 2021
# Author: Nicolai Berk
#  R version 4.1.1 (2021-08-10)
# ______________________________________________


# 0. Setup ####
library(tidyverse)
library(dplyr)
library(here)
library(data.table)
library(here)
library(lubridate)
library(glue)
library(RcppRoll)

# 1. load and clean ####

## 1.1 survey data ####
attitude_dids <- read.csv(here('data/coeftable_gles_noimp.csv'))

attitude_dids$paper_name <- ''
attitude_dids$paper_name[attitude_dids$paper == 'a'] <- 'bild'
attitude_dids$paper_name[attitude_dids$paper == 'b'] <- 'falg'
attitude_dids$paper_name[attitude_dids$paper == 'c'] <- 'faz'
attitude_dids$paper_name[attitude_dids$paper == 'd'] <- 'sz'
attitude_dids$paper_name[attitude_dids$paper == 'e'] <- 'taz'
attitude_dids$paper_name[attitude_dids$paper == 'f'] <- 'welt'
attitude_dids$paper_name[attitude_dids$paper == 'g'] <- 'other'

## 1.2 framing ####
load(here('data/mig_articles_topics.Rdata'))
framing_raw <- output; rm(output)

frames <- 
  framing_raw %>% 
  ungroup() %>% 
  select(7:13) %>% 
  colnames()

# fix dates
framing_raw$date_new <- NA_Date_
framing_raw$date_new[framing_raw$paper == "bild"] <- framing_raw$date[framing_raw$paper == "bild"] %>% as.Date()
framing_raw$date_new[framing_raw$paper == "faz"] <- framing_raw$date[framing_raw$paper=="faz"] %>% as.numeric() %>% as.Date(origin = "1970-01-01")
framing_raw$date_new[framing_raw$paper == "spon"] <- framing_raw$date[framing_raw$paper=="spon"] %>% as.Date(format = "%d.%m.%Y")
framing_raw$date_new[framing_raw$paper == "sz"] <- framing_raw$date[framing_raw$paper=="sz"] %>% as.integer() %>% as.Date(origin = "1970-01-01")
framing_raw$date_new[framing_raw$paper == "taz"] <- framing_raw$date[framing_raw$paper=="taz"] %>% as.integer() %>% as.Date(origin = "1970-01-01")
framing_raw$date_new[framing_raw$paper == "welt"] <- framing_raw$date[framing_raw$paper=="welt"] %>% as.integer() %>% as.Date(origin = "1970-01-01")
framing_raw <- framing_raw[framing_raw$date_new >= as.Date('2013-01-01'),]
framing_raw <- framing_raw %>% filter(!is.na(date_new))

save(framing_raw, file = here('data/frames_cleaned.csv'))


## 1.3 salience ####
salience <- fread(here('data/BERT_estimates_cleaned.csv'), header = T)
salience <- salience[2:nrow(salience),] # remove duplicate header row

## use urls to identify paper
salience$urlhead <- substr(salience$link, 1, 20) # get different start urls
tb <- table(salience$urlhead, useNA = 'ifany')
urlheaders <- names(tb[tb > 500])

salience$paper <- NA_character_
salience$paper[salience$urlhead == "https://taz.de/Archi"] <- 'taz'
salience$paper[salience$urlhead == "https://www.bild.de/"] <- 'bild'
salience$paper[salience$urlhead == "https://www.faz.net/"] <- 'faz'
salience$paper[salience$urlhead == "https://www.jetzt.de"] <- 'sz'
salience$paper[salience$urlhead == "https://www.spiegel."] <- 'spon'
salience$paper[salience$urlhead == "https://www.sueddeut"] <- 'sz'
salience$paper[salience$urlhead == "https://www.welt.de/"] <- 'welt'
prop.table(table(salience$paper, useNA = 'ifany')) %>% round(3) # 99.9% assigned

## fix date
salience <- salience %>% 
  filter(!is.na(date)) %>% 
  filter(!is.na(paper))

salience$date_new <- NA_Date_
salience$date_new[salience$paper == "bild"] <- salience$date[salience$paper == "bild"] %>% as.Date()
salience$date_new[salience$paper == "faz"] <- salience$date[salience$paper=="faz"] %>% as.numeric() %>% as.Date(origin = "1970-01-01")
salience$date_new[salience$paper == "spon"] <- salience$date[salience$paper=="spon"] %>% as.Date(format = "%d.%m.%Y")
salience$date_new[salience$paper == "sz"] <- salience$date[salience$paper=="sz"] %>% as.integer() %>% as.Date(origin = "1970-01-01")
salience$date_new[salience$paper == "taz"] <- salience$date[salience$paper=="taz"] %>% as.integer() %>% as.Date(origin = "1970-01-01")
salience$date_new[salience$paper == "welt"] <- salience$date[salience$paper=="welt"] %>% as.integer() %>% as.Date(origin = "1970-01-01")
salience <- salience %>% 
  filter(date_new >= as.Date('2013-01-01')) %>% 
  filter(!is.na(date_new))

## transform & select variables  
salience <- salience %>% 
  mutate(
    estimate = as.numeric(est),
    mig = label == "True"
  ) %>% 
  select(estimate, mig, paper, date_new)

salience_raw <- salience;rm(salience)

save(salience_raw, file = here('data/salience_cleaned.csv'))



# 2. daily media attention ####

# 2.1 summarise daily attention ####
salience_sum <- 
  salience_raw %>%
  group_by(paper, date_new) %>% 
  summarise(
    n_mig = sum(mig),
    n_tot = n()
  )

framing_sum <- 
  framing_raw %>% 
  group_by(date_new, paper) %>% 
  summarise(
    crime = sum(crime),
    medit = sum(medit),
    deport = sum(deport),
    refnums = sum(refnums),
    camps = sum(camps),
    labmar = sum(labmar),
    capcrime = sum(capcrime)
  )

media_attention <- left_join(salience_sum, framing_sum, by = c('paper', 'date_new'))

rm(framing_raw, framing_sum, salience_raw, salience_sum)


# 2.2 add cumulative sum of lags (1w, 1m, 6m, max) ####

media_attention <- 
  media_attention %>% 
  mutate(
       crime_7d = roll_sum(crime,    7, fill=NA, align = 'right', na.rm = T),
    capcrime_7d = roll_sum(capcrime, 7, fill=NA, align = 'right', na.rm = T),
       medit_7d = roll_sum(medit,    7, fill=NA, align = 'right', na.rm = T),
      deport_7d = roll_sum(deport,   7, fill=NA, align = 'right', na.rm = T),
     refnums_7d = roll_sum(refnums,  7, fill=NA, align = 'right', na.rm = T),
       camps_7d = roll_sum(camps,    7, fill=NA, align = 'right', na.rm = T),
      labmar_7d = roll_sum(labmar,   7, fill=NA, align = 'right', na.rm = T),
       n_mig_7d = roll_sum(n_mig,    7, fill=NA, align = 'right', na.rm = T),
       n_tot_7d = roll_sum(n_tot,    7, fill=NA, align = 'right', na.rm = T),
    
       crime_30d = roll_sum(crime,    30, fill=NA, align = 'right', na.rm = T),
    capcrime_30d = roll_sum(capcrime, 30, fill=NA, align = 'right', na.rm = T),
       medit_30d = roll_sum(medit,    30, fill=NA, align = 'right', na.rm = T),
      deport_30d = roll_sum(deport,   30, fill=NA, align = 'right', na.rm = T),
     refnums_30d = roll_sum(refnums,  30, fill=NA, align = 'right', na.rm = T),
       camps_30d = roll_sum(camps,    30, fill=NA, align = 'right', na.rm = T),
      labmar_30d = roll_sum(labmar,   30, fill=NA, align = 'right', na.rm = T),
       n_mig_30d = roll_sum(n_mig,    30, fill=NA, align = 'right', na.rm = T),
       n_tot_30d = roll_sum(n_tot,    30, fill=NA, align = 'right', na.rm = T)
    
  )


# 2.3. save ####
save(media_attention, file = here('data/media_daily_wlags.Rdata'))
