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

## get relevant waves and dates
gles_p_long <- 
  fread(here('data/gles/Panel/long_cleaned.csv')) %>% 
  as.data.frame()

wavelist <- 
  gles_p_long %>% 
  select(wave) %>% 
  filter(wave != 'a1', wave != 'a2') %>% 
  unique() %>% 
  unlist() %>% 
  as.numeric() %>% 
  sort()

datelist <- c()
for (wave_treat in wavelist){
  
  datelist <- 
    c(datelist,
      gles_p_long %>% 
        filter(wave == wave_treat) %>% 
        select(date_clean) %>% 
        unlist() %>% 
        max(na.rm = T)
    )
}

datelist <- 
  datelist %>% 
  as.Date(origin = "1970-01-01")

survey_dates <- 
  data.frame(
    wave = wavelist,
    date = datelist
  )

rm(gles_p_long)

## load media data
load(here('data/media_daily_2021-10-03.Rdata'))


# 2. transform ####

## 2.1 framing ####

## add relevant treatment waves for did
merged_media$pre_wave <- NA # upcoming survey wave
merged_media$pre_wave_1d <- NA # upcoming survey wave
merged_media$pre_wave_1w <- NA # upcoming survey wave
merged_media$pre_wave_1m <- NA # upcoming survey wave
merged_media$pre_wave_6m <- NA # upcoming survey wave

## calculate topic prevalence in between survey waves
for (wave_id in 1:nrow(survey_dates)){
  
  if (wave_id == 1){
    lower <- as.Date('2016-01-01')
  }else{
    lower <- survey_dates$date[wave_id-1]
  }
  
  upper <- survey_dates$date[wave_id]
  
  lower_1d <- max((upper - days(1)),   lower)
  lower_1w <- max((upper - weeks(1)),  lower)
  lower_1m <- max((upper - months(1)), lower)
  lower_6m <- max((upper - months(6)), lower)
  
  wave <- survey_dates$wave[wave_id]
    
  merged_media <- merged_media %>% 
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
  merged_media %>% 
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

for (lag in c("all", "1d", "1w", "1m", "6m")){
  framing_temp$dv_lag <- lag
  for (topic in (colnames(merged_media)[c(5:12, 14:20)])){
  
    framing_temp$frame <- topic
    
    framing_dids <-
      rbind(
        framing_dids,
        framing_temp
      )
  }
}
rm(framing_temp)

for (lag in c("all", "1d", "1w", "1m", "6m")){
  for (paper_treat in unique(framing_dids$paper)){
    for (topic in unique(framing_dids$frame)){
      merged_media["topic"] <- merged_media[topic]
      for (wave_id in sort(unique(merged_media$pre_wave))[-1]){
        
        ## add relevant treatment waves for did
        if (lag == "all"){
          merged_media$wave <- merged_media$pre_wave
        } else if (lag == "1d"){
          merged_media$wave <- merged_media$pre_wave_1d
        } else if (lag == "1w"){
          merged_media$wave <- merged_media$pre_wave_1w
        } else if (lag == "1m"){
          merged_media$wave <- merged_media$pre_wave_1m
        } else if (lag == "6m"){
          merged_media$wave <- merged_media$pre_wave_6m
        }
        
        
        merged_media <- merged_media %>%
          mutate(treat = ifelse(wave == wave_id, 1, NA)) %>%
          mutate(treat = ifelse(wave == (wave_id-1), 0, treat),
                 paper_reader = paper == paper_treat)
        
        merged_media$topic <- scale(merged_media[, topic], center = T, scale = T)
        
        # subset to get rid of missings
        reg_data <- 
          merged_media %>% 
          filter(!is.na(treat), !is.na(paper_reader), !is.na(topic))
        
        # ensure all conditions are observable
        if (table(reg_data$paper_reader, reg_data$treat) %>% min() > 0 &
            (table(reg_data$paper_reader, reg_data$treat) %>% dim() %>% min() == 2)){
          
          ## estimate model
          sum_lm <- summary(lm(topic ~ treat*paper_reader, data = reg_data))
          
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
                date = ifelse(change, max(merged_media$date_new[merged_media$wave == wave_id], na.rm = T), date)
              )
        }
        
      }
    }
  }
}

rm(merged_media, reg_data)

# 3. save ####
framing_dids <- 
  framing_dids %>% 
  select(-change)

datum <- Sys.Date()
save(framing_dids, file = here(paste0("data/coefs_media_", datum, ".Rdata")))




