# ______________________________________________
# Bild media effects
# Goal: bootstrap did and individual model
# Procedure: load data, run bootstrap, save
# ______________________________________________
# Date:  Mon Mar 21 13:40:05 2022
# Author: Nicolai Berk
#  R version 4.1.1 (2021-08-10)
# ______________________________________________


# Setup ####
library(tidyverse)
library(here)
library(data.table)
library(fixest)
library(lubridate)
library(rsample)
library(purrr)
library(dplyr)

set.seed(42)
B <- 1000
run_inop <- F
run_mip_did <- F
run_mip_fe <- F
run_exp_fe <- F
run_cor_mip_crime <- T

# load data ####

## DiD ####

gles_p_long <- fread(here('data/raw/gles/Panel/long_cleaned.csv'))

gles_p_long$dv <- gles_p_long$`1130_clean`
gles_p_long$post <- gles_p_long$date_clean >= as.Date("2017-02-01")

## Indiv. Exp. ####

merged_data <- fread(file = here("data/processed/merged_bert.csv"))

merged_data <- 
  merged_data %>% 
  mutate(crime_share = crime_label_share,
         crime_share_lag = crime_label_share_lag,
         dv = `1130_clean`) %>% 
  filter(lag == 7)



# bootstrap ####

## DiD ####

if(run_inop){
  

### initial opinion ####
  
  modeldata <- 
    gles_p_long %>% 
    filter(!is.na(treat) & !is.na(dv) & !is.na(init_mig)) %>% 
    select(lfdn, dv, init_mig, treat, post) %>% 
    nest(-lfdn)
  
  bs <- bootstraps(modeldata, times = B)
  bs_did_ests <- map(bs$splits, ~as.tibble(.) %>% 
                       unnest %>% 
                       summarize(DiD = 
                                   feglm(dv ~ post * treat * init_mig, 
                                         cluster = c("lfdn"), 
                                         data = .) %>% 
                                   coef() %>% 
                                   .["postTRUE:treatTRUE"],
                                 IA = 
                                   feglm(dv ~ post * treat * init_mig, 
                                         cluster = c("lfdn"), 
                                         data = .) %>% 
                                   coef() %>% 
                                   .["postTRUE:treatTRUE:init_mig"],
                       )) %>% 
    bind_rows(.id = 'boots')
  
  
  save(bs_did_ests, file = here("data/processed/did_bootstraps.RData"))
  
  rm(modeldata, bs, bs_did_ests)

}

### MIP: Migrant Crime ####

if(run_mip_did){
  
  modeldata <- 
    gles_p_long %>% 
    filter(!is.na(treat) & !is.na(dv) & !is.na(mip_mig_crime_init)) %>% 
    select(lfdn, dv, mip_mig_crime_init, treat, post) %>% 
    nest(-lfdn)
  
  bs <- bootstraps(modeldata, times = B)
  bs_did_ests_mip <- map(bs$splits, ~as.tibble(.) %>% 
                       unnest %>% 
                       summarize(DiD = 
                                   feglm(dv ~ post * treat * mip_mig_crime_init, 
                                         cluster = c("lfdn"), 
                                         data = .) %>% 
                                   coef() %>% 
                                   .["postTRUE:treatTRUE"],
                                 IA = 
                                   feglm(dv ~ post * treat * mip_mig_crime_init, 
                                         cluster = c("lfdn"), 
                                         data = .) %>% 
                                   coef() %>% 
                                   .["postTRUE:treatTRUE:mip_mig_crime_initTRUE"],
                       )) %>% 
    bind_rows(.id = 'boots')
  
  
  save(bs_did_ests_mip, file = here("data/processed/did_bootstraps_mip.RData"))
  
  rm(modeldata, bs, bs_did_ests_mip)

}



gles_p_long$crime_att <- 
  gles_p_long$`2880h_clean`

if(run_cor_mip_crime){
  
  modeldata <- 
    gles_p_long %>% 
    filter(!is.na(treat) & !is.na(dv) & !is.na(crime_att)) %>% 
    select(lfdn, dv, crime_att, treat, post) %>% 
    nest(-lfdn)
  
  bs <- bootstraps(modeldata, times = B)
  bs_cor_mig_crime <- map(bs$splits, ~as.tibble(.) %>% 
                          unnest %>% 
                          group_by(treat, post) %>% 
                          summarise(mig_crime_cor = cor(dv, crime_att, use = "complete.obs"))
                                       
                           ) %>% 
    bind_rows(.id = 'boots')
  
  
  save(bs_cor_mig_crime, file = here("data/processed/did_bootstraps_cor.RData"))
  
  rm(modeldata, bs, bs_cor_mig_crime)
  
}

## correlations of crime and migration attitudes



## Individual Exposure ####

### Initial Opinion ####

if(run_inop){
  
  modeldata <- 
    merged_data %>% 
    filter(!is.na(crime_share) & !is.na(dv) & !is.na(init_mig) & !is.na(lfdn)) %>% 
    select(lfdn, dv, init_mig, crime_share, wave) %>% 
    nest(-lfdn)
  
  bs <- bootstraps(modeldata, times = B)
  bs_ind_ests <- map(bs$splits, ~as.tibble(.) %>% 
                       unnest %>% 
                       summarize(direct = 
                                   feglm(dv ~ crime_share:init_mig + crime_share | wave + lfdn, 
                                         cluster = c("lfdn"), 
                                         data = .) %>% 
                                   coef() %>% 
                                   .["crime_share"],
                                 IA = 
                                   feglm(dv ~ crime_share:init_mig + crime_share | wave + lfdn, 
                                         cluster = c("lfdn"), 
                                         data = .) %>% 
                                   coef() %>% 
                                   .["crime_share:init_mig"],
                       )) %>% 
    bind_rows(.id = 'boots')
  
  save(bs_ind_ests, file = here("data/processed/ind_bootstraps.RData"))
  
  rm(modeldata, bs, bs_ind_ests)
  
}




### MIP ####

if(run_mip_fe){
  
  modeldata <- 
    merged_data %>% 
    filter(!is.na(crime_share) & !is.na(dv) & !is.na(mip_mig_crime_init) & !is.na(lfdn)) %>% 
    select(lfdn, dv, mip_mig_crime_init, crime_share, wave) %>% 
    nest(-lfdn)
  
  bs <- bootstraps(modeldata, times = B)
  bs_ind_ests_mip <- map(bs$splits, ~as.tibble(.) %>% 
                       unnest %>% 
                       summarize(direct = 
                                   feglm(dv ~ crime_share:mip_mig_crime_init + crime_share | wave + lfdn, 
                                         cluster = c("lfdn"), 
                                         data = .) %>% 
                                   coef() %>% 
                                   .["crime_share"],
                                 IA = 
                                   feglm(dv ~ crime_share:mip_mig_crime_init + crime_share | wave + lfdn, 
                                         cluster = c("lfdn"), 
                                         data = .) %>% 
                                   coef() %>% 
                                   .["crime_share:mip_mig_crime_initTRUE"],
                       )) %>% 
    bind_rows(.id = 'boots')
  
  
  
  save(bs_ind_ests_mip, file = here("data/processed/ind_bootstraps_mip.RData"))
  
}


if (run_exp_fe){
  
  modeldata <- 
    merged_data %>% 
    filter(!is.na(crime_share) & !is.na(dv) & !is.na(crime_share_lag) & !is.na(lfdn)) %>% 
    select(lfdn, dv, crime_share_lag, crime_share, wave) %>% 
    nest(-lfdn)
  
  bs <- bootstraps(modeldata, times = B)
  bs_ind_ests_mip <- map(bs$splits, ~as.tibble(.) %>% 
                           unnest %>% 
                           summarize(direct = 
                                       feglm(dv ~ crime_share:crime_share_lag + crime_share | wave + lfdn, 
                                             cluster = c("lfdn"), 
                                             data = .) %>% 
                                       coef() %>% 
                                       .["crime_share"],
                                     IA = 
                                       feglm(dv ~ crime_share:crime_share_lag + crime_share | wave + lfdn, 
                                             cluster = c("lfdn"), 
                                             data = .) %>% 
                                       coef() %>% 
                                       .["crime_share:crime_share_lag"],
                           )) %>% 
    bind_rows(.id = 'boots')
  
  
  
  save(bs_ind_ests_mip, file = here("data/processed/ind_bootstraps_exp.RData"))
  
}


