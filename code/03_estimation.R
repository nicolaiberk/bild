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
load(here("data/coefs_media_2021-10-03.Rdata"))
framing_dids <- 
  framing_dids %>% 
  filter(!is.na(p))

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
  filter(issue == "1130")

int_dids <- 
  attitude_dids %>% 
  filter(issue == "1210")

afd_dids <- 
  attitude_dids %>%
  filter(issue == "430i")
  

## pivot frametable wide to get 1 var/frame
framing_dids <- 
  framing_dids %>% 
  pivot_wider(id_cols = c(paper, pre_wave, dv_lag), names_from = 'frame', values_from = 'coef')


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




# 3. full fe model with media controls ####

## 3.1 estimate individual media diet ####
rm(list = ls())
gles_p_long <- fread(here('data/gles/Panel/long_cleaned.csv'))

## generate lagged dependent
gles_p_long <- 
  gles_p_long %>% 
  filter(!is.na(date_clean)) %>% 
  group_by(lfdn) %>% 
  dplyr::arrange(date_clean, .by_group = T) %>% 
  mutate(
    `1130_lag` = dplyr::lag(`1130_clean`),
    `1210_lag` = dplyr::lag(`1210_clean`),
    `1140_lag` = dplyr::lag(`1140_clean`),
    `1220_lag` = dplyr::lag(`1220_clean`),
    `430i_lag` = dplyr::lag(`430i_clean`),
    date_str = as.character(date_clean)
  )

## prepare media estimates
load(here("data/media_daily_2021-10-03.Rdata"))

# calculate salience in relevant lag
merged_media <- 
  merged_media %>% 
  mutate(
    across(n_mig_sal:capcrime, ~roll_sum(.x,    7, fill=NA, align = 'right', na.rm = T), .names = "{.col}_7d"),
    across(n_mig_sal:capcrime, ~roll_sum(.x,    30, fill=NA, align = 'right', na.rm = T), .names = "{.col}_30d")
    
  )

merged_media <- 
  merged_media %>% 
  pivot_wider(names_from = paper, values_from = 3:ncol(merged_media)) %>% 
  mutate(date_str = as.character(date_new))

## merge
merged_data <- 
  gles_p_long %>% 
  left_join(merged_media, by = c("date_str"))


## replace missings
merged_data <- 
  merged_data %>% 
  mutate(across(n_mig_sal_7d_bild:capcrime_7d_welt, ~ifelse(is.na(.x), 0, .x))) %>% 
  mutate(
    n_read = # count n of newspapers consumed
        `1661a_bin`+
        `1661c_bin`+
        `1661d_bin`+
        `1661e_bin`+
        `1661f_bin`
    )

merged_data <- 
  merged_data %>% 
  # 7-day lag absolute
  mutate(
    n_mig_tot_7d_wtd = 
      n_mig_sal_7d_bild*`1661a_clean`/n_read + # take sum of consumed newspapers, divided by n of newspapers consumed
      n_mig_sal_7d_faz*`1661c_clean`/n_read +
      n_mig_sal_7d_sz*`1661d_clean`/n_read +
      n_mig_sal_7d_taz*`1661e_clean`/n_read +
      n_mig_sal_7d_welt*`1661f_clean`/n_read,
    crime_tot_7d_wtd = 
      crime_7d_bild*`1661a_clean`/n_read +
      crime_7d_faz*`1661c_clean`/n_read +
      crime_7d_sz*`1661d_clean`/n_read +
      crime_7d_taz*`1661e_clean`/n_read +
      crime_7d_welt*`1661f_clean`/n_read,
    capcrime_tot_7d_wtd = 
      capcrime_7d_bild*`1661a_clean`/n_read +
      capcrime_7d_faz*`1661c_clean`/n_read +
      capcrime_7d_sz*`1661d_clean`/n_read +
      capcrime_7d_taz*`1661e_clean`/n_read +
      capcrime_7d_welt*`1661f_clean`/n_read,
    refnums_tot_7d_wtd = 
      refnums_7d_bild*`1661a_clean`/n_read +
      refnums_7d_faz*`1661c_clean`/n_read +
      refnums_7d_sz*`1661d_clean`/n_read +
      refnums_7d_taz*`1661e_clean`/n_read +
      refnums_7d_welt*`1661f_clean`/n_read,
    camps_tot_7d_wtd = 
      camps_7d_bild*`1661a_clean`/n_read +
      camps_7d_faz*`1661c_clean`/n_read +
      camps_7d_sz*`1661d_clean`/n_read +
      camps_7d_taz*`1661e_clean`/n_read +
      camps_7d_welt*`1661f_clean`/n_read,
    medit_tot_7d_wtd = 
      medit_7d_bild*`1661a_clean`/n_read +
      medit_7d_faz*`1661c_clean`/n_read +
      medit_7d_sz*`1661d_clean`/n_read +
      medit_7d_taz*`1661e_clean`/n_read +
      medit_7d_welt*`1661f_clean`/n_read,
    labmar_tot_7d_wtd = 
      labmar_7d_bild*`1661a_clean`/n_read +
      labmar_7d_faz*`1661c_clean`/n_read +
      labmar_7d_sz*`1661d_clean`/n_read +
      labmar_7d_taz*`1661e_clean`/n_read +
      labmar_7d_welt*`1661f_clean`/n_read,
    deport_tot_7d_wtd = 
      deport_7d_bild*`1661a_clean`/n_read +
      deport_7d_faz*`1661c_clean`/n_read +
      deport_7d_sz*`1661d_clean`/n_read +
      deport_7d_taz*`1661e_clean`/n_read +
      deport_7d_welt*`1661f_clean`/n_read
  ) %>% 
  # 30-day lag absolute
  mutate(
    n_mig_tot_30d_wtd = 
      n_mig_sal_30d_bild*`1661a_clean`/n_read + # take sum of consumed newspapers, divided by n of newspapers consumed
      n_mig_sal_30d_faz*`1661c_clean`/n_read +
      n_mig_sal_30d_sz*`1661d_clean`/n_read +
      n_mig_sal_30d_taz*`1661e_clean`/n_read +
      n_mig_sal_30d_welt*`1661f_clean`/n_read,
    crime_tot_30d_wtd = 
      crime_30d_bild*`1661a_clean`/n_read +
      crime_30d_faz*`1661c_clean`/n_read +
      crime_30d_sz*`1661d_clean`/n_read +
      crime_30d_taz*`1661e_clean`/n_read +
      crime_30d_welt*`1661f_clean`/n_read,
    capcrime_tot_30d_wtd = 
      capcrime_30d_bild*`1661a_clean`/n_read +
      capcrime_30d_faz*`1661c_clean`/n_read +
      capcrime_30d_sz*`1661d_clean`/n_read +
      capcrime_30d_taz*`1661e_clean`/n_read +
      capcrime_30d_welt*`1661f_clean`/n_read,
    refnums_tot_30d_wtd = 
      refnums_30d_bild*`1661a_clean`/n_read +
      refnums_30d_faz*`1661c_clean`/n_read +
      refnums_30d_sz*`1661d_clean`/n_read +
      refnums_30d_taz*`1661e_clean`/n_read +
      refnums_30d_welt*`1661f_clean`/n_read,
    camps_tot_30d_wtd = 
      camps_30d_bild*`1661a_clean`/n_read +
      camps_30d_faz*`1661c_clean`/n_read +
      camps_30d_sz*`1661d_clean`/n_read +
      camps_30d_taz*`1661e_clean`/n_read +
      camps_30d_welt*`1661f_clean`/n_read,
    medit_tot_30d_wtd = 
      medit_30d_bild*`1661a_clean`/n_read +
      medit_30d_faz*`1661c_clean`/n_read +
      medit_30d_sz*`1661d_clean`/n_read +
      medit_30d_taz*`1661e_clean`/n_read +
      medit_30d_welt*`1661f_clean`/n_read,
    labmar_tot_30d_wtd = 
      labmar_30d_bild*`1661a_clean`/n_read +
      labmar_30d_faz*`1661c_clean`/n_read +
      labmar_30d_sz*`1661d_clean`/n_read +
      labmar_30d_taz*`1661e_clean`/n_read +
      labmar_30d_welt*`1661f_clean`/n_read,
    deport_tot_30d_wtd = 
      deport_30d_bild*`1661a_clean`/n_read +
      deport_30d_faz*`1661c_clean`/n_read +
      deport_30d_sz*`1661d_clean`/n_read +
      deport_30d_taz*`1661e_clean`/n_read +
      deport_30d_welt*`1661f_clean`/n_read
  ) %>% 
  # 7day estimate relative attention
  mutate(
    n_tot_tot_7d_unw = 
      n_tot_7d_bild + 
      n_tot_7d_faz  +
      n_tot_7d_sz   +
      n_tot_7d_taz  +
      n_tot_7d_welt,
    n_mig_tot_7d_unw = 
      n_mig_sal_7d_bild + 
      n_mig_sal_7d_faz  +
      n_mig_sal_7d_sz   +
      n_mig_sal_7d_taz  +
      n_mig_sal_7d_welt,
    crime_tot_7d_unw = 
      crime_7d_bild +
      crime_7d_faz  +
      crime_7d_sz   +
      crime_7d_taz  +
      crime_7d_welt,
    capcrime_tot_7d_unw = 
      capcrime_7d_bild +
      capcrime_7d_faz  +
      capcrime_7d_sz   +
      capcrime_7d_taz  +
      capcrime_7d_welt,
    refnums_tot_7d_unw = 
      refnums_7d_bild +
      refnums_7d_faz+
      refnums_7d_sz+
      refnums_7d_taz+
      refnums_7d_welt,
    camps_tot_7d_unw = 
      camps_7d_bild +
      camps_7d_faz +
      camps_7d_sz +
      camps_7d_taz +
      camps_7d_welt,
    medit_tot_7d_unw = 
      medit_7d_bild +
      medit_7d_faz +
      medit_7d_sz +
      medit_7d_taz +
      medit_7d_welt,
    labmar_tot_7d_unw = 
      labmar_7d_bild +
      labmar_7d_faz +
      labmar_7d_sz +
      labmar_7d_taz +
      labmar_7d_welt,
    deport_tot_7d_unw = 
      deport_7d_bild +
      deport_7d_faz +
      deport_7d_sz +
      deport_7d_taz +
      deport_7d_welt
  ) %>% 
  mutate(
    crime_share_7d_wtd = crime_tot_7d_wtd/n_mig_tot_7d_wtd,
    capcrime_share_7d_wtd = capcrime_tot_7d_wtd/n_mig_tot_7d_wtd,
    refnums_share_7d_wtd = refnums_tot_7d_wtd/n_mig_tot_7d_wtd,
    camps_share_7d_wtd = camps_tot_7d_wtd/n_mig_tot_7d_wtd,
    medit_share_7d_wtd = medit_tot_7d_wtd/n_mig_tot_7d_wtd,
    labmar_share_7d_wtd = labmar_tot_7d_wtd/n_mig_tot_7d_wtd,
    deport_share_7d_wtd = deport_tot_7d_wtd/n_mig_tot_7d_wtd
  ) %>% 
  mutate(
    crime_share_7d_unw = crime_tot_7d_unw/n_mig_tot_7d_unw,
    capcrime_share_7d_unw = capcrime_tot_7d_unw/n_mig_tot_7d_unw,
    refnums_share_7d_unw = refnums_tot_7d_unw/n_mig_tot_7d_unw,
    camps_share_7d_unw = camps_tot_7d_unw/n_mig_tot_7d_unw,
    medit_share_7d_unw = medit_tot_7d_unw/n_mig_tot_7d_unw,
    labmar_share_7d_unw = labmar_tot_7d_unw/n_mig_tot_7d_unw,
    deport_share_7d_unw = deport_tot_7d_unw/n_mig_tot_7d_unw
  ) %>% 
  # 30-day estimate relative attention
  mutate(
    n_tot_tot_30d_unw = 
      n_tot_30d_bild + 
      n_tot_30d_faz  +
      n_tot_30d_sz   +
      n_tot_30d_taz  +
      n_tot_30d_welt,
    n_mig_tot_30d_unw = 
      n_mig_sal_30d_bild + 
      n_mig_sal_30d_faz  +
      n_mig_sal_30d_sz   +
      n_mig_sal_30d_taz  +
      n_mig_sal_30d_welt,
    crime_tot_30d_unw = 
      crime_30d_bild +
      crime_30d_faz  +
      crime_30d_sz   +
      crime_30d_taz  +
      crime_30d_welt,
    capcrime_tot_30d_unw = 
      capcrime_30d_bild +
      capcrime_30d_faz  +
      capcrime_30d_sz   +
      capcrime_30d_taz  +
      capcrime_30d_welt,
    refnums_tot_30d_unw = 
      refnums_30d_bild +
      refnums_30d_faz+
      refnums_30d_sz+
      refnums_30d_taz+
      refnums_30d_welt,
    camps_tot_30d_unw = 
      camps_30d_bild +
      camps_30d_faz +
      camps_30d_sz +
      camps_30d_taz +
      camps_30d_welt,
    medit_tot_30d_unw = 
      medit_30d_bild +
      medit_30d_faz +
      medit_30d_sz +
      medit_30d_taz +
      medit_30d_welt,
    labmar_tot_30d_unw = 
      labmar_30d_bild +
      labmar_30d_faz +
      labmar_30d_sz +
      labmar_30d_taz +
      labmar_30d_welt,
    deport_tot_30d_unw = 
      deport_30d_bild +
      deport_30d_faz +
      deport_30d_sz +
      deport_30d_taz +
      deport_30d_welt
  ) %>% 
  mutate(
    crime_share_30d_wtd = crime_tot_30d_wtd/n_mig_tot_30d_wtd,
    capcrime_share_30d_wtd = capcrime_tot_30d_wtd/n_mig_tot_30d_wtd,
    refnums_share_30d_wtd = refnums_tot_30d_wtd/n_mig_tot_30d_wtd,
    camps_share_30d_wtd = camps_tot_30d_wtd/n_mig_tot_30d_wtd,
    medit_share_30d_wtd = medit_tot_30d_wtd/n_mig_tot_30d_wtd,
    labmar_share_30d_wtd = labmar_tot_30d_wtd/n_mig_tot_30d_wtd,
    deport_share_30d_wtd = deport_tot_30d_wtd/n_mig_tot_30d_wtd
  ) %>% 
  mutate(
    crime_share_30d_unw = crime_tot_30d_unw/n_mig_tot_30d_unw,
    capcrime_share_30d_unw = capcrime_tot_30d_unw/n_mig_tot_30d_unw,
    refnums_share_30d_unw = refnums_tot_30d_unw/n_mig_tot_30d_unw,
    camps_share_30d_unw = camps_tot_30d_unw/n_mig_tot_30d_unw,
    medit_share_30d_unw = medit_tot_30d_unw/n_mig_tot_30d_unw,
    labmar_share_30d_unw = labmar_tot_30d_unw/n_mig_tot_30d_unw,
    deport_share_30d_unw = deport_tot_30d_unw/n_mig_tot_30d_unw
  )



## 3.2 individual-level model ####

## visualise simple correlations frame exposure X opinion

### immigration attitude
  
merged_data %>% 
  filter(!is.na(`1130_clean`), wave %in% c(1, 3:8)) %>% 
  ggplot(aes(as.factor(`1130_clean`), crime_share_7d_wtd)) +
  stat_summary(geom = "bar", fun = mean) +
  stat_summary(geom = "errorbar", fun.data = mean_se) +
  facet_wrap(~wave) +
  theme_minimal() +
  xlab('less restrictive <- migration opinion -> more restrictive') +
  ylab('Share of crime frame in migration coverage in consumed newspapers') +
  ggtitle('Exposure to crime frames on immigration and migration opinions across waves')
ggsave(here('paper/vis/crime_imm_att.png'))

merged_data %>% 
  filter(!is.na(`1130_clean`), wave %in% c(1, 3:8)) %>% 
  ggplot(aes(as.factor(`1130_clean`), n_mig_tot_7d_unw/n_tot_tot_7d_unw)) +
  stat_summary(geom = "bar", fun = mean) +
  stat_summary(geom = "errorbar", fun.data = mean_se) +
  facet_wrap(~wave) +
  theme_minimal() +
  xlab('less restrictive <- migration opinion -> more restrictive') +
  ylab('Migration content exposure') +
  ggtitle('Exposure to migration content and migration opinions across waves')
ggsave(here('paper/vis/salience_imm_att.png'))


### integration attitude
merged_data %>% 
  filter(!is.na(`1210_clean`), wave %in% c(1, 4:8)) %>% 
  ggplot(aes(as.factor(`1210_clean`), crime_share_7d_wtd)) +
  stat_summary(geom = "bar", fun = mean) +
  stat_summary(geom = "errorbar", fun.data = mean_se) +
  facet_wrap(~wave) +
  theme_minimal() +
  xlab('less restrictive <- integration opinion -> more restrictive') +
  ylab('Share of crime frame in migration coverage in consumed newspapers') +
  ggtitle('Exposure to crime frames on immigration and integration opinions across waves')
ggsave(here('paper/vis/crime_int_att.png'))

merged_data %>% 
  filter(!is.na(`1210_clean`), wave %in% c(1, 4:8)) %>% 
  ggplot(aes(as.factor(`1210_clean`), n_mig_tot_7d_unw/n_tot_tot_7d_unw)) +
  stat_summary(geom = "bar", fun = mean) +
  stat_summary(geom = "errorbar", fun.data = mean_se) +
  facet_wrap(~wave) +
  theme_minimal() +
  xlab('less restrictive <- integration opinion -> more restrictive') +
  ylab('Migration content exposure') +
  ggtitle('Exposure to migration content and integration opinions across waves')
ggsave(here('paper/vis/salience_int_att.png'))

### afd support
merged_data %>% 
  filter(!is.na(`1130_clean`), wave %in% c(1, 3:8)) %>% 
  ggplot(aes(as.factor(`1130_clean`), crime_share_7d_wtd)) +
  stat_summary(geom = "bar", fun = mean) +
  stat_summary(geom = "errorbar", fun.data = mean_se) +
  facet_wrap(~wave) +
  theme_minimal() +
  xlab('less <- AfD support -> more') +
  ylab('Share of crime frame in migration coverage in consumed newspapers') +
  ggtitle('Exposure to crime frames on immigration and AfD support across waves')
ggsave(here('paper/vis/crime_afd_att.png'))

merged_data %>% 
  filter(!is.na(`430i_clean`), wave %in% c(1, 3:8)) %>% 
  ggplot(aes(as.factor(`430i_clean`), n_mig_tot_7d_unw/n_tot_tot_7d_unw)) +
  stat_summary(geom = "bar", fun = mean) +
  stat_summary(geom = "errorbar", fun.data = mean_se) +
  facet_wrap(~wave) +
  theme_minimal() +
  xlab('less <- AfD support -> more') +
  ylab('Migration content exposure') +
  ggtitle('Exposure to migration content and AfD support across waves')
ggsave(here('paper/vis/salience_afd_att.png'))


## estimation

library(fixest)

efftable_absolute <- data.frame()
efftable_share <- data.frame()

# could add one more specification for estimate vs treatment interaction (w paper_bin)
for (spec in c('absolute', 'share')){
  for (lag in c('7d', '30d')){
    for (issue in c('integration', 'immigration', 'afd-support')){
      for (respons in c('only readers', 'only exclusive readers')){
        
        if (respons == 'only exclusive readers'){
          tempdta <- 
            merged_data %>% 
            filter(n_read == 1)
        }else{
          tempdta <- 
            merged_data %>% 
            filter(n_read > 0)
        }
        
        if (issue == "immigration"){
          tempdta$dv <- tempdta$`1130_clean` %>% scale()
        }else if(issue == 'integration'){
          tempdta$dv <- tempdta$`1210_clean` %>% scale()
        }else{
          tempdta$dv <- tempdta$`430i_clean` %>% scale()
        }
        
        
        if (spec == 'absolute'){
          
          tempdta$crime    <- tempdta[[paste0('crime_tot_', lag, '_wtd')]] %>% scale()
          tempdta$capcrime <- tempdta[[paste0('capcrime_tot_', lag, '_wtd')]] %>% scale()
          tempdta$medit    <- tempdta[[paste0('medit_tot_', lag, '_wtd')]] %>% scale()
          tempdta$deport   <- tempdta[[paste0('deport_tot_', lag, '_wtd')]] %>% scale()
          tempdta$refnums  <- tempdta[[paste0('refnums_tot_', lag, '_wtd')]] %>% scale()
          tempdta$camps    <- tempdta[[paste0('camps_tot_', lag, '_wtd')]] %>% scale()
          tempdta$labmar   <- tempdta[[paste0('labmar_tot_', lag, '_wtd')]] %>% scale()
          
          
          rawmod <- 
            feglm(
            dv ~ 
              crime + capcrime + medit + deport + refnums + camps + labmar |
              wave + lfdn,
            data = tempdta
              )
          mod <- summary(rawmod)
          
          
          efftable_absolute <- 
            efftable_absolute %>% 
            rbind(
              data.frame(
                
                dv = issue,
                dv_lag = lag,
                respondents = respons,
                specification = spec,
                
                beta_crime  = mod$coefficients['crime'],
                lower_beta_crime = confint(rawmod)['crime',1],
                upper_beta_crime = confint(rawmod)['crime',2],
                
                beta_capcrime = mod$coefficients['capcrime'],
                lower_beta_capcrime = confint(rawmod)['capcrime',1],
                upper_beta_capcrime = confint(rawmod)['capcrime',2],
                
                beta_medit = mod$coefficients['medit'],
                lower_beta_medit = confint(rawmod)['medit',1],
                upper_beta_medit = confint(rawmod)['medit',2],
                
                beta_deport = mod$coefficients['deport'],
                lower_beta_deport = confint(rawmod)['deport',1],
                upper_beta_deport = confint(rawmod)['deport',2],
                
                beta_refnums = mod$coefficients['refnums'],
                lower_beta_refnums = confint(rawmod)['refnums',1],
                upper_beta_refnums = confint(rawmod)['refnums',2],
                
                beta_camps = mod$coefficients['camps'],
                lower_beta_camps = confint(rawmod)['camps',1],
                upper_beta_camps = confint(rawmod)['camps',2],
                
                beta_labmar = mod$coefficients['labmar'],
                lower_beta_labmar = confint(rawmod)['labmar',1],
                upper_beta_labmar = confint(rawmod)['labmar',2]
                
              ))
          
        }else{
          
          tempdta$salience <- 
            tempdta[[paste0('n_mig_tot_', lag, '_unw')]] /
            tempdta[[paste0('n_tot_tot_', lag, '_unw')]] %>% scale()
          tempdta$crime    <- tempdta[[paste0('crime_share_', lag, '_unw')]] %>% scale()
          tempdta$capcrime <- tempdta[[paste0('capcrime_share_', lag, '_unw')]] %>% scale()
          tempdta$medit    <- tempdta[[paste0('medit_share_', lag, '_unw')]] %>% scale()
          tempdta$deport   <- tempdta[[paste0('deport_share_', lag, '_unw')]] %>% scale()
          tempdta$refnums  <- tempdta[[paste0('refnums_share_', lag, '_unw')]] %>% scale()
          tempdta$camps    <- tempdta[[paste0('camps_share_', lag, '_unw')]] %>% scale()
          tempdta$labmar   <- tempdta[[paste0('labmar_share_', lag, '_unw')]] %>% scale()
          
          
          rawmod <- 
            feglm(
              dv ~ 
                salience +
                crime*salience + 
                capcrime*salience +
                medit*salience + 
                deport*salience +
                refnums*salience + 
                camps*salience +
                labmar*salience |
                wave + lfdn,
              data = tempdta
            )
          
          mod <- summary(rawmod)
          
          
          efftable_share <- 
            efftable_share %>% 
            rbind(
              data.frame(
                
                dv = issue,
                dv_lag = lag,
                respondents = respons,
                specification = spec,
                
                beta_salience  = mod$coefficients['salience'],
                lower_beta_salience = confint(rawmod)['salience',1],
                upper_beta_salience = confint(rawmod)['salience',2],
                
                beta_crime  = mod$coefficients['salience:crime'],
                lower_beta_crime = confint(rawmod)['salience:crime',1],
                upper_beta_crime = confint(rawmod)['salience:crime',2],
                
                beta_capcrime = mod$coefficients['salience:capcrime'],
                lower_beta_capcrime = confint(rawmod)['salience:capcrime',1],
                upper_beta_capcrime = confint(rawmod)['salience:capcrime',2],
                
                beta_medit = mod$coefficients['salience:medit'],
                lower_beta_medit = confint(rawmod)['salience:medit',1],
                upper_beta_medit = confint(rawmod)['salience:medit',2],
                
                beta_deport = mod$coefficients['salience:deport'],
                lower_beta_deport = confint(rawmod)['salience:deport',1],
                upper_beta_deport = confint(rawmod)['salience:deport',2],
                
                beta_refnums = mod$coefficients['salience:refnums'],
                lower_beta_refnums = confint(rawmod)['salience:refnums',1],
                upper_beta_refnums = confint(rawmod)['salience:refnums',2],
                
                beta_camps = mod$coefficients['salience:camps'],
                lower_beta_camps = confint(rawmod)['salience:camps',1],
                upper_beta_camps = confint(rawmod)['salience:camps',2],
                
                beta_labmar = mod$coefficients['salience:labmar'],
                lower_beta_labmar = confint(rawmod)['salience:labmar',1],
                upper_beta_labmar = confint(rawmod)['salience:labmar',2]
                
              ))
          
        
        }
      }
    }
  }
}

## merge
efftable_absolute$beta_salience <- NA
efftable_absolute$upper_beta_salience <- NA
efftable_absolute$lower_beta_salience <- NA
efftable <- rbind(efftable_absolute, efftable_share)


## 3.3 save fe model ####
datum <- Sys.Date()
save(efftable, file = here(paste0('data/efftable_fe_', datum, '.Rdata')))



