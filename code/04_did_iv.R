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

## add relevant treatment waves for did
framing_raw$pre_wave <- NA # upcoming survey wave
framing_raw$pre_wave_1d <- NA # upcoming survey wave
framing_raw$pre_wave_1w <- NA # upcoming survey wave
framing_raw$pre_wave_1m <- NA # upcoming survey wave
framing_raw$pre_wave_6m <- NA # upcoming survey wave

## calculate topic prevalence in between survey waves
for (wave in sort(unique(attitude_dids$wave))){
  
  if (wave == min(unique(attitude_dids$wave))){
    lower <- min(framing_raw$date_new)
  }else{
    lower <- max(as.Date(attitude_dids$date[attitude_dids$wave == (wave-1)]))
  }
  
  upper <- max(as.Date(attitude_dids$date[attitude_dids$wave == wave]))
  
  lower_1d <- upper - days(1)
  lower_1w <- upper - weeks(1)
  lower_1m <- upper - months(1)
  lower_6m <- upper - months(6)
  
  
  framing_raw <- framing_raw %>% 
    mutate(is.wave = (date_new > lower & (date_new <= upper)),
           pre_wave = ifelse(is.wave, wave, pre_wave)) %>% 
    mutate(is.wave = (date_new > lower_1d & (date_new <= upper)),
           pre_wave_1d = ifelse(is.wave, wave, pre_wave_1d)) %>% 
    mutate(is.wave = (date_new > lower_1w & (date_new <= upper)),
           pre_wave_1w = ifelse(is.wave, wave, pre_wave_1w)) %>% 
    mutate(is.wave = (date_new > lower_1m & (date_new <= upper)),
           pre_wave_1m = ifelse(is.wave, wave, pre_wave_1m)) %>% 
    mutate(is.wave = (date_new > lower_6m & (date_new <= upper)),
           pre_wave_6m = ifelse(is.wave, wave, pre_wave_6m))
    
}


## generate did table
framing_temp <-
  framing_raw %>% 
  filter(!is.na(pre_wave)) %>% 
  select(paper, pre_wave) %>% 
  group_by(paper, pre_wave) %>% 
  summarise() %>%
  mutate(
    date = NA, 
    frame = NA,
    dv_lag = NA,
    coef = NA,
    p = NA
  )

framing_dids <- data.frame()

for (lag in c("g", "1d", "1w", "1m", "6m")){
  framing_temp$dv_lag <- lag
  for (topic in c(colnames(framing_raw)[7:13], "salience")){
  
    framing_temp$frame <- topic
    
    framing_dids <-
      rbind(
        framing_dids,
        framing_temp
      )
  }
}
rm(framing_temp)

for (lag in c("g", "1d", "1w", "1m", "6m")){
  for (paper_treat in unique(framing_dids$paper)){
    for (topic in unique(framing_dids$frame)[-8]){
      framing_raw["topic"] <- framing_raw[topic]
      for (wave_id in sort(unique(framing_raw$pre_wave))[-1]){
        
        ## add relevant treatment waves for did
        if (lag == "g"){
          framing_raw$wave <- framing_raw$pre_wave
        } else if (lag == "1d"){
          framing_raw$wave <- framing_raw$pre_wave_1d
        } else if (lag == "1w"){
          framing_raw$wave <- framing_raw$pre_wave_1w
        } else if (lag == "1m"){
          framing_raw$wave <- framing_raw$pre_wave_1m
        } else if (lag == "6m"){
          framing_raw$wave <- framing_raw$pre_wave_6m
        }
        
        
        framing_raw <- framing_raw %>%
          mutate(treat = ifelse(wave == wave_id, 1, NA)) %>%
          mutate(treat = ifelse(wave == (wave_id-1), 0, treat),
                 paper_reader = paper == paper_treat)
        
        framing_raw$topic <- scale(framing_raw[, topic], center = T, scale = T)
        
        # ensure all conditions are observable
        if (table(framing_raw$paper_reader, framing_raw$treat) %>% min() > 0 &
            (table(framing_raw$paper_reader, framing_raw$treat) %>% dim() %>% min() == 2)){
          
          ## estimate model
          sum_lm <- summary(lm(topic ~ treat*paper_reader, data = framing_raw))
          
          beta <- sum_lm$coefficients['treat:paper_readerTRUE', 1]
          pval <- sum_lm$coefficients['treat:paper_readerTRUE', 4]
          
          framing_dids <- 
            framing_dids %>% 
            mutate(
                change = 
                  (paper == paper_treat) & 
                  (pre_wave == wave_id) &
                  (frame == topic) &
                  (dv_lag == lag),
                coef = ifelse(change, beta, coef),
                p    = ifelse(change, pval, p),
                date = ifelse(change, max(framing_raw$date_new[framing_raw$wave == wave_id], na.rm = T), date)
              )
        }
        
      }
    }
  }
}

rm(framing_raw)




## 2.2 salience ####
## add relevant treatment waves for did
salience_raw$pre_wave <- NA # upcoming survey wave
salience_raw$pre_wave_1d <- NA # upcoming survey wave
salience_raw$pre_wave_1w <- NA # upcoming survey wave
salience_raw$pre_wave_1m <- NA # upcoming survey wave
salience_raw$pre_wave_6m <- NA # upcoming survey wave

## calculate topic prevalence in between survey waves
for (wave in sort(unique(attitude_dids$wave))){
  
  if (wave == min(unique(attitude_dids$wave))){
    lower <- min(salience_raw$date_new)
  }else{
    lower <- max(as.Date(attitude_dids$date[attitude_dids$wave == (wave-1)]))
  }
  
  upper <- max(as.Date(attitude_dids$date[attitude_dids$wave == wave]))
  
  lower_1d <- upper - days(1)
  lower_1w <- upper - weeks(1)
  lower_1m <- upper - months(1)
  lower_6m <- upper - months(6)
  
  
  salience_raw <- salience_raw %>% 
    mutate(is.wave = (date_new > lower & (date_new <= upper)),
           pre_wave = ifelse(is.wave, wave, pre_wave)) %>% 
    mutate(is.wave = (date_new > lower_1d & (date_new <= upper)),
           pre_wave_1d = ifelse(is.wave, wave, pre_wave_1d)) %>% 
    mutate(is.wave = (date_new > lower_1w & (date_new <= upper)),
           pre_wave_1w = ifelse(is.wave, wave, pre_wave_1w)) %>% 
    mutate(is.wave = (date_new > lower_1m & (date_new <= upper)),
           pre_wave_1m = ifelse(is.wave, wave, pre_wave_1m)) %>% 
    mutate(is.wave = (date_new > lower_6m & (date_new <= upper)),
           pre_wave_6m = ifelse(is.wave, wave, pre_wave_6m))
  
}

for (lag in c("g", "1d", "1w", "1m", "6m")){
  for (paper_treat in unique(framing_dids$paper)){
    for (wave_id in sort(unique(salience_raw$pre_wave))[-1]){
        
      ## add relevant treatment waves for did
      if (lag == "g"){
        salience_raw$wave <- salience_raw$pre_wave
      } else if (lag == "1d"){
        salience_raw$wave <- salience_raw$pre_wave_1d
      } else if (lag == "1w"){
        salience_raw$wave <- salience_raw$pre_wave_1w
      } else if (lag == "1m"){
        salience_raw$wave <- salience_raw$pre_wave_1m
      } else if (lag == "6m"){
        salience_raw$wave <- salience_raw$pre_wave_6m
      }
      
      salience_raw <- salience_raw %>%
        mutate(treat = ifelse(wave == wave_id, 1, NA)) %>%
        mutate(treat = ifelse(wave == (wave_id-1), 0, treat),
               paper_reader = paper == paper_treat)
      
      # ensure all conditions are observable
      if (table(salience_raw$paper_reader, salience_raw$treat) %>% min() > 0 &
          (table(salience_raw$paper_reader, salience_raw$treat) %>% dim() %>% min() == 2)){
        
        ## estimate model
        sum_lm <- summary(lm(mig ~ treat*paper_reader, data = salience_raw))
        
        beta <- sum_lm$coefficients['treat:paper_readerTRUE', 1]
        pval <- sum_lm$coefficients['treat:paper_readerTRUE', 4]
        
        sal_share <-
          salience_raw %>% 
          filter(paper == paper_treat) %>% 
          filter(wave == wave_id) %>% 
          summarise(attention = sum(mig)/n()) %>% 
          unlist()
        
        framing_dids <- 
          framing_dids %>% 
          mutate(
            change = 
              (paper == paper_treat) & 
              (pre_wave == wave_id) &
              (frame == "salience") &
              (dv_lag == lag)) %>% 
          mutate(
            coef = ifelse(change, beta, coef),
            p    = ifelse(change, pval, p),
            date = as.numeric(ifelse(change, max(salience_raw$date_new[salience_raw$wave == wave_id], na.rm = T), date)))
      }
    }
  }
}

rm(salience_raw)

# 3. save ####

save(framing_dids, file = here("data/coefs_media.Rdata"))




