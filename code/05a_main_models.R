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
library(sjPlot)

# 1. load data ####
load(here("data/coefs_media.Rdata"))
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



# 2. estimation and vis ####


## 2.1 framing ####

## select relevant issues (immigration & integration)
imm_dids <- 
  attitude_dids %>% 
  filter(issue == "1130")

int_dids <- 
  attitude_dids %>% 
  filter(issue == "1210")

## pivot frametable wide to get 1 var/frame
framing_dids <- 
  framing_dids %>% 
  select(-change) %>% 
  pivot_wider(id_cols = c(paper, pre_wave, dv_lag), names_from = 'frame', values_from = 'coef')


## correlate with frames
efftable <- data.frame()
for (issue in c("immigration", "integration")){
  for (lag in c("g", "1d", "1w", "1m", "6m")){
    for (respons in unique(attitude_dids$respondents)){
      
      if (issue == "immigration"){
        dv_set <- 
          imm_dids %>% 
          mutate(delta_att = coef_fe,
                 paper = paper_name)
      }else{
        dv_set <- 
          int_dids %>% 
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
              
              beta_crime  = mod$coefficients['crime',1] %>% round(3),
              lower_beta_crime = confint(rawmod)['crime',1],
              upper_beta_crime = confint(rawmod)['crime',2],
              pval_crime = mod$coefficients['crime',4] %>% round(3),
              
              beta_capcrime = mod$coefficients['capcrime',1] %>% round(3),
              lower_beta_capcrime = confint(rawmod)['capcrime',1],
              upper_beta_capcrime = confint(rawmod)['capcrime',2],
              pval_capcrime = mod$coefficients['capcrime',4] %>% round(3),
              
              beta_medit = mod$coefficients['medit',1] %>% round(3),
              lower_beta_medit = confint(rawmod)['medit',1],
              upper_beta_medit = confint(rawmod)['medit',2],
              pval_medit = mod$coefficients['medit',4] %>% round(3),
              
              beta_deport = mod$coefficients['deport',1] %>% round(3),
              lower_beta_deport = confint(rawmod)['deport',1],
              upper_beta_deport = confint(rawmod)['deport',2],
              pval_deport = mod$coefficients['deport',4] %>% round(3),
              
              beta_refnums = mod$coefficients['refnums',1] %>% round(3),
              lower_beta_refnums = confint(rawmod)['refnums',1],
              upper_beta_refnums = confint(rawmod)['refnums',2],
              pval_refnums = mod$coefficients['refnums',4] %>% round(3),
              
              beta_camps = mod$coefficients['camps',1] %>% round(3),
              lower_beta_camps = confint(rawmod)['camps',1],
              upper_beta_camps = confint(rawmod)['camps',2],
              pval_camps = mod$coefficients['camps',4] %>% round(3),
              
              beta_labmar = mod$coefficients['labmar',1] %>% round(3),
              lower_beta_labmar = confint(rawmod)['labmar',1],
              upper_beta_labmar = confint(rawmod)['labmar',2],
              pval_labmar = mod$coefficients['labmar',4] %>% round(3)
              
            ))
        
      }
    }
  }
}

save(efftable, file = here('data/efftable_noimp.Rdata'))










## 3.4 full fe model with media controls ####

## 3.4.1 prepare individual estimates dependent variable ####
rm(list = ls())
gles_p_long <- fread(here('data/gles/Panel/long_cleaned.csv'))

## generate lagged dependent
gles_p_long <- 
  gles_p_long %>% 
  select(lfdn, wave, date_clean, 
         `1130_clean`, `1210_clean`, `1140_clean`, `1220_clean`, 
         contains("1661")) %>% 
  filter(!is.na(date_clean)) %>% 
  group_by(lfdn) %>% 
  dplyr::arrange(date_clean, .by_group = T) %>% 
  mutate(
    `1130_lag` = dplyr::lag(`1130_clean`),
    `1210_lag` = dplyr::lag(`1210_clean`),
    `1140_lag` = dplyr::lag(`1140_clean`),
    `1220_lag` = dplyr::lag(`1220_clean`),
    date_str = as.character(date_clean)
  )


## 3.4.2 prepare daily media estimates ####
load(here("data/media_daily_wlags.Rdata"))

media_attention <- 
  media_attention %>% 
  pivot_wider(names_from = paper, values_from = 3:ncol(media_attention)) %>% 
  mutate(date_str = as.character(date_new))


## merge
merged_data <- 
  gles_p_long %>% 
  left_join(media_attention, by = c("date_str"))

## estimate frame exposure
merged_data <- 
  merged_data %>% 
  mutate(
    n_mig_7d_bild = ifelse(is.na(n_mig_7d_bild), 0, n_mig_7d_bild),
    n_mig_7d_faz  = ifelse(is.na(n_mig_7d_faz),  0, n_mig_7d_faz),
    n_mig_7d_sz   = ifelse(is.na(n_mig_7d_sz),   0, n_mig_7d_sz),
    n_mig_7d_taz  = ifelse(is.na(n_mig_7d_taz),  0, n_mig_7d_taz),
    n_mig_7d_welt = ifelse(is.na(n_mig_7d_welt), 0, n_mig_7d_welt),
    
    crime_7d_bild = ifelse(is.na(crime_7d_bild), 0, crime_7d_bild),
    crime_7d_faz  = ifelse(is.na(crime_7d_faz),  0, crime_7d_faz),
    crime_7d_sz   = ifelse(is.na(crime_7d_sz),   0, crime_7d_sz),
    crime_7d_taz  = ifelse(is.na(crime_7d_taz),  0, crime_7d_taz),
    crime_7d_welt = ifelse(is.na(crime_7d_welt), 0, crime_7d_welt),
    
    capcrime_7d_bild = ifelse(is.na(capcrime_7d_bild), 0, capcrime_7d_bild),
    capcrime_7d_faz  = ifelse(is.na(capcrime_7d_faz),  0, capcrime_7d_faz),
    capcrime_7d_sz   = ifelse(is.na(capcrime_7d_sz),   0, capcrime_7d_sz),
    capcrime_7d_taz  = ifelse(is.na(capcrime_7d_taz),  0, capcrime_7d_taz),
    capcrime_7d_welt = ifelse(is.na(capcrime_7d_welt), 0, capcrime_7d_welt),
    
    refnums_7d_bild = ifelse(is.na(refnums_7d_bild), 0, refnums_7d_bild),
    refnums_7d_faz  = ifelse(is.na(refnums_7d_faz),  0, refnums_7d_faz),
    refnums_7d_sz   = ifelse(is.na(refnums_7d_sz),   0, refnums_7d_sz),
    refnums_7d_taz  = ifelse(is.na(refnums_7d_taz),  0, refnums_7d_taz),
    refnums_7d_welt = ifelse(is.na(refnums_7d_welt), 0, refnums_7d_welt),
    
    camps_7d_bild = ifelse(is.na(camps_7d_bild), 0, camps_7d_bild),
    camps_7d_faz  = ifelse(is.na(camps_7d_faz),  0, camps_7d_faz),
    camps_7d_sz   = ifelse(is.na(camps_7d_sz),   0, camps_7d_sz),
    camps_7d_taz  = ifelse(is.na(camps_7d_taz),  0, camps_7d_taz),
    camps_7d_welt = ifelse(is.na(camps_7d_welt), 0, camps_7d_welt),
    
    medit_7d_bild = ifelse(is.na(medit_7d_bild), 0, medit_7d_bild),
    medit_7d_faz  = ifelse(is.na(medit_7d_faz),  0, medit_7d_faz),
    medit_7d_sz   = ifelse(is.na(medit_7d_sz),   0, medit_7d_sz),
    medit_7d_taz  = ifelse(is.na(medit_7d_taz),  0, medit_7d_taz),
    medit_7d_welt = ifelse(is.na(medit_7d_welt), 0, medit_7d_welt),
    
    deport_7d_bild = ifelse(is.na(deport_7d_bild), 0, deport_7d_bild),
    deport_7d_faz  = ifelse(is.na(deport_7d_faz),  0, deport_7d_faz),
    deport_7d_sz   = ifelse(is.na(deport_7d_sz),   0, deport_7d_sz),
    deport_7d_taz  = ifelse(is.na(deport_7d_taz),  0, deport_7d_taz),
    deport_7d_welt = ifelse(is.na(deport_7d_welt), 0, deport_7d_welt),
    
    labmar_7d_bild = ifelse(is.na(labmar_7d_bild), 0, labmar_7d_bild),
    labmar_7d_faz  = ifelse(is.na(labmar_7d_faz),  0, labmar_7d_faz),
    labmar_7d_sz   = ifelse(is.na(labmar_7d_sz),   0, labmar_7d_sz),
    labmar_7d_taz  = ifelse(is.na(labmar_7d_taz),  0, labmar_7d_taz),
    labmar_7d_welt = ifelse(is.na(labmar_7d_welt), 0, labmar_7d_welt)
    
  ) %>% 
  mutate(
    n_read = 
      sum(
        (`1661a_clean` > 0),
        (`1661c_clean` > 0),
        (`1661d_clean` > 0),
        (`1661e_clean` > 0),
        (`1661f_clean` > 0)
      )
  )

merged_data <- 
  merged_data %>% 
  mutate(
    n_mig_tot_7d_unw = 
      (n_mig_7d_bild *`1661a_bin` + # take sum of consumed newspapers, divided by total papers read
      n_mig_7d_faz  *`1661c_bin` +
      n_mig_7d_sz   *`1661d_bin` +
      n_mig_7d_taz  *`1661e_bin` +
      n_mig_7d_welt *`1661f_bin`)/n_read,
    crime_tot_7d_unw = 
      (crime_7d_bild *`1661a_bin` +
      crime_7d_faz  *`1661c_bin` +
      crime_7d_sz   *`1661d_bin` +
      crime_7d_taz  *`1661e_bin` +
      crime_7d_welt *`1661f_bin`)/n_read,
    capcrime_tot_7d_unw = 
      (capcrime_7d_bild *`1661a_bin` +
      capcrime_7d_faz  *`1661c_bin` +
      capcrime_7d_sz   *`1661d_bin` +
      capcrime_7d_taz  *`1661e_bin` +
      capcrime_7d_welt *`1661f_bin`)/n_read,
    refnums_tot_7d_unw = 
      (refnums_7d_bild *`1661a_bin` +
      refnums_7d_faz  *`1661c_bin` +
      refnums_7d_sz   *`1661d_bin` +
      refnums_7d_taz  *`1661e_bin` +
      refnums_7d_welt *`1661f_bin`)/n_read,
    camps_tot_7d_unw = 
      (camps_7d_bild *`1661a_bin` +
      camps_7d_faz  *`1661c_bin` +
      camps_7d_sz   *`1661d_bin` +
      camps_7d_taz  *`1661e_bin` +
      camps_7d_welt *`1661f_bin`)/n_read,
    medit_tot_7d_unw = 
      (medit_7d_bild *`1661a_bin` +
      medit_7d_faz  *`1661c_bin` +
      medit_7d_sz   *`1661d_bin` +
      medit_7d_taz  *`1661e_bin` +
      medit_7d_welt *`1661f_bin`)/n_read,
    labmar_tot_7d_unw = 
      (labmar_7d_bild *`1661a_bin` +
      labmar_7d_faz  *`1661c_bin` +
      labmar_7d_sz   *`1661d_bin` +
      labmar_7d_taz  *`1661e_bin` +
      labmar_7d_welt *`1661f_bin`)/n_read,
    deport_tot_7d_unw = 
      (deport_7d_bild *`1661a_bin` +
      deport_7d_faz  *`1661c_bin` +
      deport_7d_sz   *`1661d_bin` +
      deport_7d_taz  *`1661e_bin` +
      deport_7d_welt *`1661f_bin`)/n_read
  )



# 3.4.3 individual-level model ####

## unweighted independent
imm_mod_unw_all_nosal <- 
  merged_data %>% 
  lm(data = .,
     formula = 
       `1130_clean` ~ 
       `1130_lag` + 
       crime_tot_7d_unw + 
       capcrime_tot_7d_unw + 
       refnums_tot_7d_unw + 
       camps_tot_7d_unw + 
       medit_tot_7d_unw + 
       labmar_tot_7d_unw + 
       deport_tot_7d_unw )

summary(imm_mod_unw_all_nosal) # only camps very restrictive effect - weird, generally large effects


longnames <- c('(Intercept)', 'Lagged DV', 'Petty Crime', 'Capital Crime', 'Refugee Numbers', 'Camps', 'Mediterranean Rescue', 'Labour Market', 'Deportations')

imm_model_plot <- 
  plot_model(imm_mod_unw_all_nosal, 
           sort.est = T,
           title = 'Effect of different frames on Immigration attitude', 
           show.p = T)
ggsave(imm_model_plot,
       filename = here('paper/vis/individual_effects_immigration.png'),
        width = 8, height = 6)



int_mod_unw_all_nosal <- 
  merged_data %>% 
  lm(data = .,
     formula = 
       `1210_clean` ~ 
       `1210_lag` + 
       crime_tot_7d_unw + 
       capcrime_tot_7d_unw + 
       refnums_tot_7d_unw + 
       camps_tot_7d_unw + 
       medit_tot_7d_unw + 
       labmar_tot_7d_unw + 
       deport_tot_7d_unw )

summary(int_mod_unw_all_nosal)

stargazer(imm_mod_unw_all_nosal, 
          int_mod_unw_all_nosal,
          title = 'Individual-level Model', 
          out = here('paper/imm_unw.tex'))

int_model_plot <- 
  plot_model(int_mod_unw_all_nosal, 
           sort.est = T,
           title = 'Effect of different frames on Integration attitude', 
           show.p = T)

ggsave(int_model_plot, 
       filename = here('paper/vis/individual_effects_integration.png'),
         width = 8, height = 6)

grid.arrange(imm_model_plot, int_model_plot) %>% 
  ggsave(filename = here('paper/vis/individual_effects_combined.png'),
                         width = 8, height = 8)

