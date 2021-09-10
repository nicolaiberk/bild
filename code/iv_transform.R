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

# 1. load and clean ####

## 1.1 survey data ####
attitude_dids <- read.csv(here('paper/ReportThomas/coeftable.csv'))

attitude_dids$paper_name <- ''
attitude_dids$paper_name[attitude_dids$paper == 'a'] <- 'bild'
attitude_dids$paper_name[attitude_dids$paper == 'b'] <- 'falg'
attitude_dids$paper_name[attitude_dids$paper == 'c'] <- 'faz'
attitude_dids$paper_name[attitude_dids$paper == 'd'] <- 'sz'
attitude_dids$paper_name[attitude_dids$paper == 'e'] <- 'taz'
attitude_dids$paper_name[attitude_dids$paper == 'f'] <- 'welt'

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

# 2. transform ####

## 2.1 framing ####

## add relevant treatment waves for did
framing_raw$pre_wave <- NA # upcoming survey wave
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
  
  lower_1w <- upper - weeks(1)
  lower_1m <- upper - months(1)
  lower_6m <- upper - months(6)
  
  
  framing_raw <- framing_raw %>% 
    mutate(is.wave = c(date_new > lower & (date_new < upper)),
           pre_wave = ifelse(is.wave, wave, pre_wave)) %>% 
    mutate(is.wave = c(date_new > lower_1w & (date_new < upper)),
           pre_wave_1w = ifelse(is.wave, wave, pre_wave_1w)) %>% 
    mutate(is.wave = c(date_new > lower_1m & (date_new < upper)),
           pre_wave_1m = ifelse(is.wave, wave, pre_wave_1m)) %>% 
    mutate(is.wave = c(date_new > lower_6m & (date_new < upper)),
           pre_wave_6m = ifelse(is.wave, wave, pre_wave_6m))
    
}


## generate did table
framing_temp <-
  framing_raw %>% 
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

for (lag in c("g", "1w", "1m", "6m")){
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

for (lag in c("g", "1w", "1m", "6m")){
  for (paper_treat in unique(framing_dids$paper)){
    for (topic in unique(framing_dids$frame)[-8]){
      framing_raw["topic"] <- framing_raw[topic]
      for (wave_id in sort(unique(framing_raw$pre_wave))[-1]){
        
        ## add relevant treatment waves for did
        if (lag == "g"){
          framing_raw$wave <- framing_raw$pre_wave
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
        
        # ensure all conditions are observable
        if (table(framing_raw$paper_reader, framing_raw$treat) %>% min() > 0){
          
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

# add general attention variable

framing_temp <- data.frame()

for (lag in c("g", "1w", "1m", "6m")){
  if (lag == "g"){
    framing_raw$wave <- framing_raw$pre_wave
  } else if (lag == "1w"){
    framing_raw$wave <- framing_raw$pre_wave_1w
  } else if (lag == "1m"){
    framing_raw$wave <- framing_raw$pre_wave_1m
  } else if (lag == "6m"){
    framing_raw$wave <- framing_raw$pre_wave_6m
  }
  
  framing_temp <- 
    framing_raw %>% 
    group_by(paper, wave) %>% 
    summarise_at(vars(6:12), sum) %>% 
    pivot_longer(!c(paper, wave), names_to = "frame", values_to = "attention") %>% 
    mutate(dv_lag = lag) %>% 
    mutate(pre_wave = wave) %>% 
    rbind(framing_temp)
}

framing_dids <- merge(framing_dids, framing_temp, 
                       by = c("paper", "pre_wave", "frame", "dv_lag"),
                       all.x = T) 
rm(framing_raw, framing_temp)





## 2.2 salience ####
## add relevant treatment waves for did
salience_raw$pre_wave <- NA # upcoming survey wave
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
  
  lower_1w <- upper - weeks(1)
  lower_1m <- upper - months(1)
  lower_6m <- upper - months(6)
  
  
  salience_raw <- salience_raw %>% 
    mutate(is.wave = c(date_new > lower & (date_new < upper)),
           pre_wave = ifelse(is.wave, wave, pre_wave)) %>% 
    mutate(is.wave = c(date_new > lower_1w & (date_new < upper)),
           pre_wave_1w = ifelse(is.wave, wave, pre_wave_1w)) %>% 
    mutate(is.wave = c(date_new > lower_1m & (date_new < upper)),
           pre_wave_1m = ifelse(is.wave, wave, pre_wave_1m)) %>% 
    mutate(is.wave = c(date_new > lower_6m & (date_new < upper)),
           pre_wave_6m = ifelse(is.wave, wave, pre_wave_6m))
  
}

for (lag in c("g", "1w", "1m", "6m")){
  for (paper_treat in unique(framing_dids$paper)){
    for (wave_id in sort(unique(salience_raw$pre_wave))[-1]){
        
      ## add relevant treatment waves for did
      if (lag == "g"){
        salience_raw$wave <- salience_raw$pre_wave
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
      if (table(salience_raw$paper_reader, salience_raw$treat) %>% min() > 0){
        
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
            date = as.numeric(ifelse(change, max(salience_raw$date_new[salience_raw$wave == wave_id], na.rm = T), date)),
            attention = ifelse(change, sal_share, attention))
      }
    }
  }
}

rm(salience_raw)

# 3. save ####

save(framing_dids, file = here("data/coefs_media.Rdata"))




