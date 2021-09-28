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

#############################
# calculate media diet for all respondents, multiplying treatment by days read
# run fe models once with weighted frame exposure both for all and exclusive r, 
#   once unweighted and with exclusive readers

## merge
merged_data <- 
  gles_p_long %>% 
  left_join(media_attention, by = c("date_str"))

## estimate frame exposure by days read in last week
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
    n_mig_tot_7d_wtd = 
      n_mig_7d_bild*`1661a_clean`/n_read + # take sum of consumed newspapers weighted by days read, divided by n of newspapers consumed
      n_mig_7d_faz*`1661c_clean`/n_read +
      n_mig_7d_sz*`1661d_clean`/n_read +
      n_mig_7d_taz*`1661e_clean`/n_read +
      n_mig_7d_welt*`1661f_clean`/n_read,
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
  mutate(
    n_mig_tot_7d_unw = 
      n_mig_7d_bild + # take sum of consumed newspapers weighted by days read, divided by n of newspapers consumed
      n_mig_7d_faz  +
      n_mig_7d_sz   +
      n_mig_7d_taz  +
      n_mig_7d_welt,
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
    crime_share_7d_unw = crime_tot_7d_unw/n_mig_tot_7d_unw,
    capcrime_share_7d_unw = capcrime_tot_7d_unw/n_mig_tot_7d_unw,
    refnums_share_7d_unw = refnums_tot_7d_unw/n_mig_tot_7d_unw,
    camps_share_7d_unw = camps_tot_7d_unw/n_mig_tot_7d_unw,
    medit_share_7d_unw = medit_tot_7d_unw/n_mig_tot_7d_unw,
    labmar_share_7d_unw = labmar_tot_7d_unw/n_mig_tot_7d_unw,
    deport_share_7d_unw = deport_tot_7d_unw/n_mig_tot_7d_unw
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

mod_tbl <- summary(imm_mod_unw_all_nosal) # all but crime significant, refnums and deport negative


longnames <- c('(Intercept)', 'Lagged DV', 'Petty Crime', 'Capital Crime', 'Refugee Numbers', 'Camps', 'Mediterranean Rescue', 'Labour Market', 'Deportations')
arm::coefplot(imm_mod_unw_all_nosal, longnames) %>% 
  ggsave(filename = here('paper/vis/individual_effects_immigration.png'),
                         width = 8, height = 6)


# integration
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

summary(int_mod_unw_all_nosal) # capcrime negative (expected), deport positive (also expected), but surprisingly labmar no effect

stargazer(imm_mod_unw_all_nosal, 
          int_mod_unw_all_nosal,
          title = 'Individual-level Model', 
          out = here('paper/imm_unw.tex'))

arm::coefplot(int_mod_unw_all_nosal) %>% 
  ggsave(filename = here('paper/vis/individual_effects_immigration.png'),
         width = 8, height = 6)


## holds controlling for salience:
imm_mod_unw_all_sal <- 
  merged_data %>% 
  lm(data = .,
     formula = 
       `1130_clean` ~ 
       `1130_lag` + 
       n_mig_tot_7d_unw +
       crime_tot_7d_unw + 
       capcrime_tot_7d_unw + 
       refnums_tot_7d_unw + 
       camps_tot_7d_unw + 
       medit_tot_7d_unw + 
       labmar_tot_7d_unw + 
       deport_tot_7d_unw )

summary(imm_mod_unw_all_sal)
arm::coefplot(imm_mod_unw_all_sal)


int_mod_unw_all_sal <- 
  merged_data %>% 
  lm(data = .,
     formula = 
       `1210_clean` ~ 
       `1210_lag` + 
       n_mig_tot_7d_unw +
       crime_tot_7d_unw + 
       capcrime_tot_7d_unw + 
       refnums_tot_7d_unw + 
       camps_tot_7d_unw + 
       medit_tot_7d_unw + 
       labmar_tot_7d_unw + 
       deport_tot_7d_unw )

summary(int_mod_unw_all_sal) # capcrime negative (expected), labmar positive (unclear)
arm::coefplot(int_mod_unw_all_sal)


## interact salience with share

imm_mod_inter <- 
  merged_data %>% 
  lm(data = .,
     formula = 
       `1130_clean` ~ 
       `1130_lag` + 
       n_mig_tot_7d_unw +
       n_mig_tot_7d_unw * crime_share_7d_unw + 
       n_mig_tot_7d_unw * capcrime_share_7d_unw + 
       n_mig_tot_7d_unw * refnums_share_7d_unw + 
       n_mig_tot_7d_unw * camps_share_7d_unw + 
       n_mig_tot_7d_unw * medit_share_7d_unw + 
       n_mig_tot_7d_unw * labmar_share_7d_unw + 
       n_mig_tot_7d_unw * deport_share_7d_unw)

summary(imm_mod_inter) # refnums, medit, camps pos, salience only slightly but interaction hard to interpret (not sure if useful but might plot marginal effects)


int_mod_inter <- 
  merged_data %>% 
  lm(data = .,
     formula = 
       `1210_clean` ~ 
       `1210_lag` + 
       n_mig_tot_7d_unw +
       n_mig_tot_7d_unw * crime_share_7d_unw + 
       n_mig_tot_7d_unw * capcrime_share_7d_unw + 
       n_mig_tot_7d_unw * refnums_share_7d_unw + 
       n_mig_tot_7d_unw * camps_share_7d_unw + 
       n_mig_tot_7d_unw * medit_share_7d_unw + 
       n_mig_tot_7d_unw * labmar_share_7d_unw + 
       n_mig_tot_7d_unw * deport_share_7d_unw)

summary(int_mod_inter) # no effect salience, 


## weighted media consumption estimates
imm_mod_wtd_all <- 
  lm(`1130_clean` ~ 
     `1130_lag` + 
       # n_mig_tot_7d + 
       crime_tot_7d + 
       capcrime_tot_7d + 
       refnums_tot_7d + 
       camps_tot_7d + 
       medit_tot_7d + 
       labmar_tot_7d + 
       deport_tot_7d,
     data = merged_data)

summary(imm_mod_wtd_all) # only effect camps - but actually MORE restrictive!


int_mod_wtd_all <- 
  lm(`1210_clean` ~ 
       `1210_lag` + 
       # n_mig_tot_7d +
       crime_tot_7d + 
       capcrime_tot_7d + 
       refnums_tot_7d + 
       camps_tot_7d + 
       medit_tot_7d + 
       labmar_tot_7d + 
       deport_tot_7d,
     data = merged_data)

summary(int_mod_wtd_all) # no effects




## 3.5 explore individual-level moderators ####


### 3.5.1 existing migration opinion ####

## immigration
imm_mod_inter_opinion <- 
  merged_data %>% 
  mutate(
    imm_op = `1130_clean`,
    imm_op_lag = `1130_lag`
  ) %>% 
  lm(data = .,
     formula = 
       imm_op ~ 
       imm_op_lag + 
       crime_tot_7d_unw * imm_op_lag + 
       capcrime_tot_7d_unw * imm_op_lag + 
       refnums_tot_7d_unw * imm_op_lag + 
       camps_tot_7d_unw * imm_op_lag + 
       medit_tot_7d_unw * imm_op_lag + 
       labmar_tot_7d_unw * imm_op_lag + 
       deport_tot_7d_unw * imm_op_lag )

plot_model(imm_mod_inter_opinion) # camps, medit, labmar more restrictive, capcrime, refnums less - no effing sense. But capcrime most polarising, labmar de-polarising

### marginal effect plot
interplot(imm_mod_inter_opinion, var1 = 'crime_tot_7d_unw',    var2 = 'imm_op_lag', hist = T) # negative effect among pro-imm, positive effect among anti-imm (anti-polarisation?)
interplot(imm_mod_inter_opinion, var1 = 'capcrime_tot_7d_unw', var2 = 'imm_op_lag', hist = T) # capcrime makes those with less restrictive attitudes even less restrictive (and vice versa, as expected for more restrictive attitudes) - Polarisation&Reaktanz?
interplot(imm_mod_inter_opinion, var1 = 'refnums_tot_7d_unw',  var2 = 'imm_op_lag', hist = T) # always positive, less positive with more negative attitudes
interplot(imm_mod_inter_opinion, var1 = 'camps_tot_7d_unw',    var2 = 'imm_op_lag', hist = T) # always negative, but less so among more restrictive resps
interplot(imm_mod_inter_opinion, var1 = 'medit_tot_7d_unw',    var2 = 'imm_op_lag', hist = T) # always negative
interplot(imm_mod_inter_opinion, var1 = 'labmar_tot_7d_unw',   var2 = 'imm_op_lag', hist = T) # strongly anti-polarising
interplot(imm_mod_inter_opinion, var1 = 'deport_tot_7d_unw',   var2 = 'imm_op_lag', hist = T) # strongly anti-polarising



## integration

int_mod_inter_opinion <- 
  merged_data %>%   
  mutate(
    int_op = `1210_clean`,
    int_op_lag = `1210_lag`,
  ) %>% 
  lm(data = .,
     formula = 
       int_op ~ 
       int_op_lag + 
       crime_tot_7d_unw * int_op_lag + 
       capcrime_tot_7d_unw * int_op_lag + 
       refnums_tot_7d_unw * int_op_lag + 
       camps_tot_7d_unw * int_op_lag + 
       medit_tot_7d_unw * int_op_lag + 
       labmar_tot_7d_unw * int_op_lag + 
       deport_tot_7d_unw * int_op_lag )

plot_model(int_mod_inter_opinion) # capcrime more restrictive, camps more lenient, capcrime polarising, camps de-polarising

interplot(int_mod_inter_opinion, var1 = 'crime_tot_7d_unw',    var2 = 'int_op_lag', hist = T) #  not significant
interplot(int_mod_inter_opinion, var1 = 'capcrime_tot_7d_unw', var2 = 'int_op_lag', hist = T) # strongly polarising
interplot(int_mod_inter_opinion, var1 = 'refnums_tot_7d_unw',  var2 = 'int_op_lag', hist = T) # polarising
interplot(int_mod_inter_opinion, var1 = 'camps_tot_7d_unw',    var2 = 'int_op_lag', hist = T) # de-polarising
interplot(int_mod_inter_opinion, var1 = 'medit_tot_7d_unw',    var2 = 'int_op_lag', hist = T) # not significant
interplot(int_mod_inter_opinion, var1 = 'labmar_tot_7d_unw',   var2 = 'int_op_lag', hist = T) # not significant
interplot(int_mod_inter_opinion, var1 = 'deport_tot_7d_unw',   var2 = 'int_op_lag', hist = T) # strongly de-polarising
  


### 3.5.2 existing issue importance ####

## immigration
imm_mod_inter_imp <- 
  merged_data %>% 
  mutate(
    imm_op = `1130_clean`,
    imm_op_lag = `1130_lag`,
    imm_imp_lag = `1140_lag`
  ) %>% 
  lm(data = .,
     formula = 
       imm_op ~ 
       imm_op_lag + 
       imm_imp_lag + 
       crime_tot_7d_unw * imm_imp_lag + 
       capcrime_tot_7d_unw * imm_imp_lag + 
       refnums_tot_7d_unw * imm_imp_lag + 
       camps_tot_7d_unw * imm_imp_lag + 
       medit_tot_7d_unw * imm_imp_lag + 
       labmar_tot_7d_unw * imm_imp_lag + 
       deport_tot_7d_unw * imm_imp_lag )

plot_model(imm_mod_inter_imp) # only labmar decreases restrictiveness and depolarises, camps increase restrictiveness

### marginal effect plot
interplot(imm_mod_inter_imp, var1 = 'crime_tot_7d_unw',    var2 = 'imm_imp_lag')
interplot(imm_mod_inter_imp, var1 = 'capcrime_tot_7d_unw', var2 = 'imm_imp_lag')
interplot(imm_mod_inter_imp, var1 = 'refnums_tot_7d_unw',  var2 = 'imm_imp_lag')
interplot(imm_mod_inter_imp, var1 = 'camps_tot_7d_unw',    var2 = 'imm_imp_lag')
interplot(imm_mod_inter_imp, var1 = 'medit_tot_7d_unw',    var2 = 'imm_imp_lag')
interplot(imm_mod_inter_imp, var1 = 'labmar_tot_7d_unw',   var2 = 'imm_imp_lag')
interplot(imm_mod_inter_imp, var1 = 'deport_tot_7d_unw',   var2 = 'imm_imp_lag')


## integration

int_mod_inter_imp <- 
  merged_data %>%   
  mutate(
    int_op = `1210_clean`,
    int_op_lag = `1210_lag`,
    int_imp_lag = `1220_lag`
  ) %>% 
lm(data = .,
   formula = 
     int_op ~ 
     int_op_lag + 
     int_imp_lag + 
     int_imp_lag *  crime_tot_7d_unw    + 
     int_imp_lag * capcrime_tot_7d_unw + 
     int_imp_lag * refnums_tot_7d_unw  + 
     int_imp_lag *  camps_tot_7d_unw    + 
     int_imp_lag *  medit_tot_7d_unw    + 
     int_imp_lag *  labmar_tot_7d_unw   + 
     int_imp_lag *  deport_tot_7d_unw   )

plot_model(int_mod_inter_imp) # capcrime, refnums, camps more restrictive, no interaction effects

interplot(int_mod_inter_imp, var1 = 'crime_tot_7d_unw',    var2 = 'int_imp_lag', hist = T)
interplot(int_mod_inter_imp, var1 = 'capcrime_tot_7d_unw', var2 = 'int_imp_lag', hist = T)
interplot(int_mod_inter_imp, var1 = 'refnums_tot_7d_unw',  var2 = 'int_imp_lag', hist = T)
interplot(int_mod_inter_imp, var1 = 'camps_tot_7d_unw',    var2 = 'int_imp_lag', hist = T)
interplot(int_mod_inter_imp, var1 = 'medit_tot_7d_unw',    var2 = 'int_imp_lag', hist = T)
interplot(int_mod_inter_imp, var1 = 'labmar_tot_7d_unw',   var2 = 'int_imp_lag', hist = T)
interplot(int_mod_inter_imp, var1 = 'deport_tot_7d_unw',   var2 = 'int_imp_lag', hist = T)




# 3.5.3 party vote (190b) ####

merged_data$`190b_clean` <- relevel(as.factor(merged_data$`190b_clean`), ref = 'CDU/CSU')  

## immigration
imm_mod_inter_party <- 
  merged_data %>% 
  mutate(
    imm_op = `1130_clean`,
    party = `190b_clean`,
    imm_op_lag = `1130_lag`
  ) %>% 
  filter(party != '') %>% 
  lm(data = .,
     formula = 
       imm_op ~ 
       imm_op_lag + 
       party + 
       crime_tot_7d_unw * party + 
       capcrime_tot_7d_unw * party + 
       refnums_tot_7d_unw * party + 
       camps_tot_7d_unw * party + 
       medit_tot_7d_unw * party + 
       labmar_tot_7d_unw * party + 
       deport_tot_7d_unw * party )

plot_model(imm_mod_inter_party, group.terms = c(1, 2:6, rep(7, 7), rep(2:6, 7))) # 

### marginal effect plot
interplot(imm_mod_inter_party, var1 = 'crime_tot_7d_unw',    var2 = 'party')
interplot(imm_mod_inter_party, var1 = 'capcrime_tot_7d_unw', var2 = 'party') # baseline effect (CDU/CSU) positive(!), but becomes insignificant/neg for all other parties
interplot(imm_mod_inter_party, var1 = 'refnums_tot_7d_unw',  var2 = 'party') # leads to more positive att among left
interplot(imm_mod_inter_party, var1 = 'camps_tot_7d_unw',    var2 = 'party') # neg effect disappears for SPD and AfD, increases for Greens
interplot(imm_mod_inter_party, var1 = 'medit_tot_7d_unw',    var2 = 'party') # becomes negative among left ???
interplot(imm_mod_inter_party, var1 = 'labmar_tot_7d_unw',   var2 = 'party') # neg effect disappears for SPD & AfD, increases for Left
interplot(imm_mod_inter_party, var1 = 'deport_tot_7d_unw',   var2 = 'party') # neg effect among CDU/CSU disappears for all, even pos among greens


## integration

int_mod_inter_party <- 
  merged_data %>%   
  mutate(
    int_op = `1210_clean`,
    party = `190b_clean`,
    int_op_lag = `1210_lag`,
  ) %>% 
  lm(data = .,
     formula = 
       int_op ~ 
       int_op_lag +
       party + 
       party *  crime_tot_7d_unw    + 
       party * capcrime_tot_7d_unw + 
       party * refnums_tot_7d_unw  + 
       party *  camps_tot_7d_unw    + 
       party *  medit_tot_7d_unw    + 
       party *  labmar_tot_7d_unw   + 
       party *  deport_tot_7d_unw   )

plot_model(imm_mod_inter_party, group.terms = c(1, 2:6, rep(7, 7), rep(2:6, 7))) # 

### marginal effect plot
interplot(imm_mod_inter_party, var1 = 'crime_tot_7d_unw',    var2 = 'party') # no effect
interplot(imm_mod_inter_party, var1 = 'capcrime_tot_7d_unw', var2 = 'party') # neg for CDU/CSU, insig for others
interplot(imm_mod_inter_party, var1 = 'refnums_tot_7d_unw',  var2 = 'party') # neg for Left, no eff for all others
interplot(imm_mod_inter_party, var1 = 'camps_tot_7d_unw',    var2 = 'party') # pos for CDU/CSU, FDP, greens, Left
interplot(imm_mod_inter_party, var1 = 'medit_tot_7d_unw',    var2 = 'party') # pos for all, esp. left
interplot(imm_mod_inter_party, var1 = 'labmar_tot_7d_unw',   var2 = 'party') # pos for all except AfD & SPD
interplot(imm_mod_inter_party, var1 = 'deport_tot_7d_unw',   var2 = 'party') # pos for CDU, neg for Greens


# 3.5.4 ideology (1500) ####

# 3.5.5 strength of pre-existing att on migration ####

# 3.5.6 number of papers read/diversity of media diet ####

# 3.5.7 political knowledge ####







# 3.6 effects on issue importance ####


## unweighted independent
imm_imp_unw_all_nosal <- 
  merged_data %>% 
  lm(data = .,
     formula = 
       `1140_clean` ~ 
       `1140_lag` + 
       n_mig_tot_7d_unw +
       crime_tot_7d_unw + 
       capcrime_tot_7d_unw + 
       refnums_tot_7d_unw + 
       camps_tot_7d_unw + 
       medit_tot_7d_unw + 
       labmar_tot_7d_unw + 
       deport_tot_7d_unw )

summary(imm_imp_unw_all_nosal) # not significant


arm::coefplot(imm_imp_unw_all_nosal)


# integration
int_imp_unw_all_nosal <- 
  merged_data %>% 
  lm(data = .,
     formula = 
       `1220_clean` ~ 
       `1220_lag` + 
       n_mig_tot_7d_unw +
       crime_tot_7d_unw + 
       capcrime_tot_7d_unw + 
       refnums_tot_7d_unw + 
       camps_tot_7d_unw + 
       medit_tot_7d_unw + 
       labmar_tot_7d_unw + 
       deport_tot_7d_unw )

summary(int_imp_unw_all_nosal) # again only crime

arm::coefplot(int_imp_unw_all_nosal)
