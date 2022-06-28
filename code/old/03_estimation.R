# ______________________________________________
# Effect of framing on issue attitudes
# Goal: Estimate effects
# Procedure: load, estimate
# ______________________________________________
# Date:  Thu Sep 09 17:24:28 2021
# Author: Nicolai Berk
#  R version 4.0.3 (2020-10-10)
# ______________________________________________


# 0. setup ####
library(tidyverse)
library(dplyr)
library(here)
library(data.table)
library(here)
library(lubridate)
library(glue)
library(gridExtra)
library(stargazer)
library(interplot)
library(sjPlot)
library(RcppRoll)
  
# 1. load data ####
load(here("data/did_media_2021-12-15.Rdata"))
load(here("data/kink_media_2021-12-15.Rdata"))

framing_dids <- 
  did_estimates %>% 
  filter(!is.na(est)) %>% 
  mutate(
    frame = topic,
    paper = paper_treat,
    coef = est
  )

framing_kinks <- 
  kink_estimates %>% 
  filter(!is.na(est)) %>% 
  mutate(
    frame = topic,
    coef = est
  )


rm(kink_estimates, did_estimates)



attitude_dids <- read.csv(here('data/coeftable_gles_noimp.csv'))

attitude_dids$paper_name <- ''
attitude_dids$paper_name[attitude_dids$paper == 'a'] <- 'bild'
attitude_dids$paper_name[attitude_dids$paper == 'b'] <- 'falg'
attitude_dids$paper_name[attitude_dids$paper == 'c'] <- 'faz'
attitude_dids$paper_name[attitude_dids$paper == 'd'] <- 'sz'
attitude_dids$paper_name[attitude_dids$paper == 'e'] <- 'taz'
attitude_dids$paper_name[attitude_dids$paper == 'f'] <- 'welt'



# 2. estimation did ####


## 2.1 framing ####

## select relevant issues (immigration & integration)
imm_dids <- 
  attitude_dids %>% 
  filter(dv == "imm")

int_dids <- 
  attitude_dids %>% 
  filter(dv == "int")

afd_dids <- 
  attitude_dids %>%
  filter(dv == "afd")
  

## pivot frametable wide to get 1 var/frame
framing_dids <- 
  framing_dids %>% 
  dplyr::select(c(coef, wave_id, reference, paper, lag, frame)) %>% 
  pivot_wider(id_cols = c(paper, wave_id, lag, reference), names_from = 'frame', values_from = 'coef')

framing_kinks <- 
  framing_kinks %>% 
  dplyr::select(coef, frame, model, paper, cutoff) %>% 
  pivot_wider(id_cols = c(paper, cutoff, model), names_from = 'frame', values_from = 'coef')


## correlate with frames
efftable <- data.frame()
for (issue in c("immigration", "integration", "afd-support")){
  for (lag in c("all", "1d", "1w", "1m", "6m")){
    for (respons in unique(attitude_dids$respondents)){
    
      if (issue == "immigration"){
        dv_set <- 
          imm_dids %>% 
          mutate(delta_att = coef_fe,
                 paper = paper_name)
      }else if (issue == "integration"){
        dv_set <- 
          int_dids %>% 
          mutate(delta_att = coef_fe*(-1),
                 paper = paper_name)
      }else{
        dv_set <- 
          afd_dids %>% 
          mutate(delta_att = coef_fe,
                 paper = paper_name)
      }
      
      dv_set <- 
        dv_set %>% 
        filter((respondents == respons) & (medium == '1661'))
      
      iv_set <- framing_dids %>% 
        filter(dv_lag == lag) %>% 
        mutate(wave = pre_wave)
      
      
      full_set <- merge(dv_set, iv_set, by = c("paper", "wave"))
      
      if (nrow(full_set > 0)){
        
        rawmod <- lm(delta_att ~ 
                       crime + medit + deport + refnums + camps + labmar + capcrime, 
                     full_set)
        mod <- summary(rawmod)
        
        
        
        efftable <- efftable %>% 
          rbind(
            data.frame(
            
              dv = issue,
              dv_lag = lag,
              respondents = respons,
              
              beta_crime  = mod$coefficients['crime',1],
              lower_beta_crime = confint(rawmod)['crime',1],
              upper_beta_crime = confint(rawmod)['crime',2],
              pval_crime = mod$coefficients['crime',4],
              
              beta_capcrime = mod$coefficients['capcrime',1],
              lower_beta_capcrime = confint(rawmod)['capcrime',1],
              upper_beta_capcrime = confint(rawmod)['capcrime',2],
              pval_capcrime = mod$coefficients['capcrime',4],
              
              beta_medit = mod$coefficients['medit',1],
              lower_beta_medit = confint(rawmod)['medit',1],
              upper_beta_medit = confint(rawmod)['medit',2],
              pval_medit = mod$coefficients['medit',4],
              
              beta_deport = mod$coefficients['deport',1],
              lower_beta_deport = confint(rawmod)['deport',1],
              upper_beta_deport = confint(rawmod)['deport',2],
              pval_deport = mod$coefficients['deport',4],
              
              beta_refnums = mod$coefficients['refnums',1],
              lower_beta_refnums = confint(rawmod)['refnums',1],
              upper_beta_refnums = confint(rawmod)['refnums',2],
              pval_refnums = mod$coefficients['refnums',4],
              
              beta_camps = mod$coefficients['camps',1],
              lower_beta_camps = confint(rawmod)['camps',1],
              upper_beta_camps = confint(rawmod)['camps',2],
              pval_camps = mod$coefficients['camps',4],
              
              beta_labmar = mod$coefficients['labmar',1],
              lower_beta_labmar = confint(rawmod)['labmar',1],
              upper_beta_labmar = confint(rawmod)['labmar',2],
              pval_labmar = mod$coefficients['labmar',4]
              
          ))
      
      }
    }
  }
}

datum <- Sys.Date()
save(efftable, file = here(paste0('data/efftable_did_', datum, '.Rdata')))



