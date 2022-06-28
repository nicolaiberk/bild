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
load(here("data/media_daily_wlags.Rdata"))
gles_p_long <- fread(here('data/gles/Panel/long_cleaned.csv'))


# 2. estimation and vis ####

## 2.1 prepare individual estimates dependent variable ####


## 2.2 prepare daily media estimates ####

media_attention <- 
  media_attention %>% 
  pivot_wider(names_from = paper, values_from = 3:ncol(media_attention)) %>% 
  mutate(date_str = as.character(date_new)) %>% 
  select(contains('7d'), contains('30d'), date_str)

gles_p_long <- 
  gles_p_long %>% 
  select(contains('1661'), lfdn, wave, date_clean, 
         `1130_clean`, `1210_clean`, `1140_clean`, `1220_clean`) %>% 
  filter(wave > 2, wave < 9)


## merge
merged_data <- 
  gles_p_long %>% 
  mutate(date_str = as.character(date_clean)) %>% 
  inner_join(media_attention, by = c("date_str"))



## estimate frame exposure
# merged_data <- 
#   merged_data %>% 
#   mutate(
#     n_tot_7d_bild = ifelse(is.na(n_tot_7d_bild), 0, n_tot_7d_bild),
#     n_tot_7d_faz  = ifelse(is.na(n_tot_7d_faz),  0, n_tot_7d_faz),
#     n_tot_7d_sz   = ifelse(is.na(n_tot_7d_sz),   0, n_tot_7d_sz),
#     n_tot_7d_taz  = ifelse(is.na(n_tot_7d_taz),  0, n_tot_7d_taz),
#     n_tot_7d_welt = ifelse(is.na(n_tot_7d_welt), 0, n_tot_7d_welt),
#     
#     n_mig_7d_bild = ifelse(is.na(n_mig_7d_bild), 0, n_mig_7d_bild),
#     n_mig_7d_faz  = ifelse(is.na(n_mig_7d_faz),  0, n_mig_7d_faz),
#     n_mig_7d_sz   = ifelse(is.na(n_mig_7d_sz),   0, n_mig_7d_sz),
#     n_mig_7d_taz  = ifelse(is.na(n_mig_7d_taz),  0, n_mig_7d_taz),
#     n_mig_7d_welt = ifelse(is.na(n_mig_7d_welt), 0, n_mig_7d_welt),
#     
#     crime_7d_bild = ifelse(is.na(crime_7d_bild), 0, crime_7d_bild),
#     crime_7d_faz  = ifelse(is.na(crime_7d_faz),  0, crime_7d_faz),
#     crime_7d_sz   = ifelse(is.na(crime_7d_sz),   0, crime_7d_sz),
#     crime_7d_taz  = ifelse(is.na(crime_7d_taz),  0, crime_7d_taz),
#     crime_7d_welt = ifelse(is.na(crime_7d_welt), 0, crime_7d_welt),
#     
#     capcrime_7d_bild = ifelse(is.na(capcrime_7d_bild), 0, capcrime_7d_bild),
#     capcrime_7d_faz  = ifelse(is.na(capcrime_7d_faz),  0, capcrime_7d_faz),
#     capcrime_7d_sz   = ifelse(is.na(capcrime_7d_sz),   0, capcrime_7d_sz),
#     capcrime_7d_taz  = ifelse(is.na(capcrime_7d_taz),  0, capcrime_7d_taz),
#     capcrime_7d_welt = ifelse(is.na(capcrime_7d_welt), 0, capcrime_7d_welt),
#     
#     refnums_7d_bild = ifelse(is.na(refnums_7d_bild), 0, refnums_7d_bild),
#     refnums_7d_faz  = ifelse(is.na(refnums_7d_faz),  0, refnums_7d_faz),
#     refnums_7d_sz   = ifelse(is.na(refnums_7d_sz),   0, refnums_7d_sz),
#     refnums_7d_taz  = ifelse(is.na(refnums_7d_taz),  0, refnums_7d_taz),
#     refnums_7d_welt = ifelse(is.na(refnums_7d_welt), 0, refnums_7d_welt),
#     
#     camps_7d_bild = ifelse(is.na(camps_7d_bild), 0, camps_7d_bild),
#     camps_7d_faz  = ifelse(is.na(camps_7d_faz),  0, camps_7d_faz),
#     camps_7d_sz   = ifelse(is.na(camps_7d_sz),   0, camps_7d_sz),
#     camps_7d_taz  = ifelse(is.na(camps_7d_taz),  0, camps_7d_taz),
#     camps_7d_welt = ifelse(is.na(camps_7d_welt), 0, camps_7d_welt),
#     
#     medit_7d_bild = ifelse(is.na(medit_7d_bild), 0, medit_7d_bild),
#     medit_7d_faz  = ifelse(is.na(medit_7d_faz),  0, medit_7d_faz),
#     medit_7d_sz   = ifelse(is.na(medit_7d_sz),   0, medit_7d_sz),
#     medit_7d_taz  = ifelse(is.na(medit_7d_taz),  0, medit_7d_taz),
#     medit_7d_welt = ifelse(is.na(medit_7d_welt), 0, medit_7d_welt),
#     
#     deport_7d_bild = ifelse(is.na(deport_7d_bild), 0, deport_7d_bild),
#     deport_7d_faz  = ifelse(is.na(deport_7d_faz),  0, deport_7d_faz),
#     deport_7d_sz   = ifelse(is.na(deport_7d_sz),   0, deport_7d_sz),
#     deport_7d_taz  = ifelse(is.na(deport_7d_taz),  0, deport_7d_taz),
#     deport_7d_welt = ifelse(is.na(deport_7d_welt), 0, deport_7d_welt),
#     
#     labmar_7d_bild = ifelse(is.na(labmar_7d_bild), 0, labmar_7d_bild),
#     labmar_7d_faz  = ifelse(is.na(labmar_7d_faz),  0, labmar_7d_faz),
#     labmar_7d_sz   = ifelse(is.na(labmar_7d_sz),   0, labmar_7d_sz),
#     labmar_7d_taz  = ifelse(is.na(labmar_7d_taz),  0, labmar_7d_taz),
#     labmar_7d_welt = ifelse(is.na(labmar_7d_welt), 0, labmar_7d_welt)
#     
#   ) %>%  
#   mutate(
#     n_tot_30d_bild = ifelse(is.na(n_tot_30d_bild), 0, n_tot_30d_bild),
#     n_tot_30d_faz  = ifelse(is.na(n_tot_30d_faz),  0, n_tot_30d_faz),
#     n_tot_30d_sz   = ifelse(is.na(n_tot_30d_sz),   0, n_tot_30d_sz),
#     n_tot_30d_taz  = ifelse(is.na(n_tot_30d_taz),  0, n_tot_30d_taz),
#     n_tot_30d_welt = ifelse(is.na(n_tot_30d_welt), 0, n_tot_30d_welt),
#     
#     n_mig_30d_bild = ifelse(is.na(n_mig_30d_bild), 0, n_mig_30d_bild),
#     n_mig_30d_faz  = ifelse(is.na(n_mig_30d_faz),  0, n_mig_30d_faz),
#     n_mig_30d_sz   = ifelse(is.na(n_mig_30d_sz),   0, n_mig_30d_sz),
#     n_mig_30d_taz  = ifelse(is.na(n_mig_30d_taz),  0, n_mig_30d_taz),
#     n_mig_30d_welt = ifelse(is.na(n_mig_30d_welt), 0, n_mig_30d_welt),
#     
#     crime_30d_bild = ifelse(is.na(crime_30d_bild), 0, crime_30d_bild),
#     crime_30d_faz  = ifelse(is.na(crime_30d_faz),  0, crime_30d_faz),
#     crime_30d_sz   = ifelse(is.na(crime_30d_sz),   0, crime_30d_sz),
#     crime_30d_taz  = ifelse(is.na(crime_30d_taz),  0, crime_30d_taz),
#     crime_30d_welt = ifelse(is.na(crime_30d_welt), 0, crime_30d_welt),
#     
#     capcrime_30d_bild = ifelse(is.na(capcrime_30d_bild), 0, capcrime_30d_bild),
#     capcrime_30d_faz  = ifelse(is.na(capcrime_30d_faz),  0, capcrime_30d_faz),
#     capcrime_30d_sz   = ifelse(is.na(capcrime_30d_sz),   0, capcrime_30d_sz),
#     capcrime_30d_taz  = ifelse(is.na(capcrime_30d_taz),  0, capcrime_30d_taz),
#     capcrime_30d_welt = ifelse(is.na(capcrime_30d_welt), 0, capcrime_30d_welt),
#     
#     refnums_30d_bild = ifelse(is.na(refnums_30d_bild), 0, refnums_30d_bild),
#     refnums_30d_faz  = ifelse(is.na(refnums_30d_faz),  0, refnums_30d_faz),
#     refnums_30d_sz   = ifelse(is.na(refnums_30d_sz),   0, refnums_30d_sz),
#     refnums_30d_taz  = ifelse(is.na(refnums_30d_taz),  0, refnums_30d_taz),
#     refnums_30d_welt = ifelse(is.na(refnums_30d_welt), 0, refnums_30d_welt),
#     
#     camps_30d_bild = ifelse(is.na(camps_30d_bild), 0, camps_30d_bild),
#     camps_30d_faz  = ifelse(is.na(camps_30d_faz),  0, camps_30d_faz),
#     camps_30d_sz   = ifelse(is.na(camps_30d_sz),   0, camps_30d_sz),
#     camps_30d_taz  = ifelse(is.na(camps_30d_taz),  0, camps_30d_taz),
#     camps_30d_welt = ifelse(is.na(camps_30d_welt), 0, camps_30d_welt),
#     
#     medit_30d_bild = ifelse(is.na(medit_30d_bild), 0, medit_30d_bild),
#     medit_30d_faz  = ifelse(is.na(medit_30d_faz),  0, medit_30d_faz),
#     medit_30d_sz   = ifelse(is.na(medit_30d_sz),   0, medit_30d_sz),
#     medit_30d_taz  = ifelse(is.na(medit_30d_taz),  0, medit_30d_taz),
#     medit_30d_welt = ifelse(is.na(medit_30d_welt), 0, medit_30d_welt),
#     
#     deport_30d_bild = ifelse(is.na(deport_30d_bild), 0, deport_30d_bild),
#     deport_30d_faz  = ifelse(is.na(deport_30d_faz),  0, deport_30d_faz),
#     deport_30d_sz   = ifelse(is.na(deport_30d_sz),   0, deport_30d_sz),
#     deport_30d_taz  = ifelse(is.na(deport_30d_taz),  0, deport_30d_taz),
#     deport_30d_welt = ifelse(is.na(deport_30d_welt), 0, deport_30d_welt),
#     
#     labmar_30d_bild = ifelse(is.na(labmar_30d_bild), 0, labmar_30d_bild),
#     labmar_30d_faz  = ifelse(is.na(labmar_30d_faz),  0, labmar_30d_faz),
#     labmar_30d_sz   = ifelse(is.na(labmar_30d_sz),   0, labmar_30d_sz),
#     labmar_30d_taz  = ifelse(is.na(labmar_30d_taz),  0, labmar_30d_taz),
#     labmar_30d_welt = ifelse(is.na(labmar_30d_welt), 0, labmar_30d_welt)
#     
#   )

merged_data$n_read  <-  
  merged_data %>% 
  ungroup() %>% 
  select(contains("1661")) %>% 
  select(contains("bin"), -`1661g_bin`, -`1661b_bin`) %>% 
  rowSums(na.rm = T)


merged_data <- 
  merged_data %>%
  filter(n_read != 0) %>% 
  mutate(
    mig_share_7d = 
      (n_mig_7d_bild *`1661a_bin`/n_tot_7d_bild + # take average of share of migration articles per paper
       n_mig_7d_faz  *`1661c_bin`/n_tot_7d_faz  +
       n_mig_7d_sz   *`1661d_bin`/n_tot_7d_sz   +
       n_mig_7d_taz  *`1661e_bin`/n_tot_7d_taz  +
       n_mig_7d_welt *`1661f_bin`/n_tot_7d_welt)/n_read,
    crime_share_7d = 
      (crime_7d_bild *`1661a_bin`/n_mig_7d_bild + # take average of share of frame per migration article
      crime_7d_faz   *`1661c_bin`/n_mig_7d_faz  +
      crime_7d_sz    *`1661d_bin`/n_mig_7d_sz   +
      crime_7d_taz   *`1661e_bin`/n_mig_7d_taz  +
      crime_7d_welt  *`1661f_bin`/n_mig_7d_welt)/n_read,
    capcrime_share_7d = 
      (capcrime_7d_bild *`1661a_bin`/n_mig_7d_bild +
       capcrime_7d_faz  *`1661c_bin`/n_mig_7d_faz  +
       capcrime_7d_sz   *`1661d_bin`/n_mig_7d_sz   +
       capcrime_7d_taz  *`1661e_bin`/n_mig_7d_taz  +
       capcrime_7d_welt *`1661f_bin`/n_mig_7d_welt)/n_read,
    refnums_share_7d = 
      (refnums_7d_bild *`1661a_bin`/n_mig_7d_bild +
       refnums_7d_faz  *`1661c_bin`/n_mig_7d_faz  +
       refnums_7d_sz   *`1661d_bin`/n_mig_7d_sz   +
       refnums_7d_taz  *`1661e_bin`/n_mig_7d_taz  +
       refnums_7d_welt *`1661f_bin`/n_mig_7d_welt)/n_read,
    camps_share_7d = 
      (camps_7d_bild *`1661a_bin`/n_mig_7d_bild +
       camps_7d_faz  *`1661c_bin`/n_mig_7d_faz  +
       camps_7d_sz   *`1661d_bin`/n_mig_7d_sz   +
       camps_7d_taz  *`1661e_bin`/n_mig_7d_taz  +
       camps_7d_welt *`1661f_bin`/n_mig_7d_welt)/n_read,
    medit_share_7d = 
      (medit_7d_bild *`1661a_bin`/n_mig_7d_bild +
       medit_7d_faz  *`1661c_bin`/n_mig_7d_faz  +
       medit_7d_sz   *`1661d_bin`/n_mig_7d_sz   +
       medit_7d_taz  *`1661e_bin`/n_mig_7d_taz  +
       medit_7d_welt *`1661f_bin`/n_mig_7d_welt)/n_read,
    labmar_share_7d = 
      (labmar_7d_bild *`1661a_bin`/n_mig_7d_bild +
       labmar_7d_faz  *`1661c_bin`/n_mig_7d_faz  +
       labmar_7d_sz   *`1661d_bin`/n_mig_7d_sz   +
       labmar_7d_taz  *`1661e_bin`/n_mig_7d_taz  +
       labmar_7d_welt *`1661f_bin`/n_mig_7d_welt)/n_read,
    deport_share_7d = 
      (deport_7d_bild *`1661a_bin`/n_mig_7d_bild +
       deport_7d_faz  *`1661c_bin`/n_mig_7d_faz  +
       deport_7d_sz   *`1661d_bin`/n_mig_7d_sz   +
       deport_7d_taz  *`1661e_bin`/n_mig_7d_taz  +
       deport_7d_welt *`1661f_bin`/n_mig_7d_welt)/n_read
  ) %>% 
  mutate(
    mig_share_30d = 
      (n_mig_30d_bild *`1661a_bin`/n_tot_30d_bild + # take average of share of migration articles per paper
         n_mig_30d_faz  *`1661c_bin`/n_tot_30d_faz  +
         n_mig_30d_sz   *`1661d_bin`/n_tot_30d_sz   +
         n_mig_30d_taz  *`1661e_bin`/n_tot_30d_taz  +
         n_mig_30d_welt *`1661f_bin`/n_tot_30d_welt)/n_read,
    crime_share_30d = 
      (crime_30d_bild *`1661a_bin`/n_mig_30d_bild + # take average of share of frame per migration article
         crime_30d_faz   *`1661c_bin`/n_mig_30d_faz  +
         crime_30d_sz    *`1661d_bin`/n_mig_30d_sz   +
         crime_30d_taz   *`1661e_bin`/n_mig_30d_taz  +
         crime_30d_welt  *`1661f_bin`/n_mig_30d_welt)/n_read,
    capcrime_share_30d = 
      (capcrime_30d_bild *`1661a_bin`/n_mig_30d_bild +
         capcrime_30d_faz  *`1661c_bin`/n_mig_30d_faz  +
         capcrime_30d_sz   *`1661d_bin`/n_mig_30d_sz   +
         capcrime_30d_taz  *`1661e_bin`/n_mig_30d_taz  +
         capcrime_30d_welt *`1661f_bin`/n_mig_30d_welt)/n_read,
    refnums_share_30d = 
      (refnums_30d_bild *`1661a_bin`/n_mig_30d_bild +
         refnums_30d_faz  *`1661c_bin`/n_mig_30d_faz  +
         refnums_30d_sz   *`1661d_bin`/n_mig_30d_sz   +
         refnums_30d_taz  *`1661e_bin`/n_mig_30d_taz  +
         refnums_30d_welt *`1661f_bin`/n_mig_30d_welt)/n_read,
    camps_share_30d = 
      (camps_30d_bild *`1661a_bin`/n_mig_30d_bild +
         camps_30d_faz  *`1661c_bin`/n_mig_30d_faz  +
         camps_30d_sz   *`1661d_bin`/n_mig_30d_sz   +
         camps_30d_taz  *`1661e_bin`/n_mig_30d_taz  +
         camps_30d_welt *`1661f_bin`/n_mig_30d_welt)/n_read,
    medit_share_30d = 
      (medit_30d_bild *`1661a_bin`/n_mig_30d_bild +
         medit_30d_faz  *`1661c_bin`/n_mig_30d_faz  +
         medit_30d_sz   *`1661d_bin`/n_mig_30d_sz   +
         medit_30d_taz  *`1661e_bin`/n_mig_30d_taz  +
         medit_30d_welt *`1661f_bin`/n_mig_30d_welt)/n_read,
    labmar_share_30d = 
      (labmar_30d_bild *`1661a_bin`/n_mig_30d_bild +
         labmar_30d_faz  *`1661c_bin`/n_mig_30d_faz  +
         labmar_30d_sz   *`1661d_bin`/n_mig_30d_sz   +
         labmar_30d_taz  *`1661e_bin`/n_mig_30d_taz  +
         labmar_30d_welt *`1661f_bin`/n_mig_30d_welt)/n_read,
    deport_share_30d = 
      (deport_30d_bild *`1661a_bin`/n_mig_30d_bild +
         deport_30d_faz  *`1661c_bin`/n_mig_30d_faz  +
         deport_30d_sz   *`1661d_bin`/n_mig_30d_sz   +
         deport_30d_taz  *`1661e_bin`/n_mig_30d_taz  +
         deport_30d_welt *`1661f_bin`/n_mig_30d_welt)/n_read
  )





# 3 individual-level models ####
library(fixest)
merged_data$lfdn <- as.factor(merged_data$lfdn)

# vis function
intervis <- function(model, term1, term2) {
  
  model_table <- broom::tidy(model, conf.int = T)
  
  
  x1 <- c(rep(min(merged_data[[term1]], na.rm = T), 10), 
          rep(summary(merged_data[[term1]])[3], 10), 
          rep(max(merged_data[[term1]], na.rm = T), 10))
  x2_1q <- min(merged_data[[term2]], na.rm = T)
  x2_3q <- max(merged_data[[term2]], na.rm = T)
  
  x2 <- rep(seq(x2_1q, x2_3q, length.out = 10), 3)
  
  y <-
    x1 * model_table$estimate[model_table$term == term1]+
    x2 * model_table$estimate[model_table$term == term2] +
    x1 * x2 * model_table$estimate[model_table$term == paste0(term1, ':', term2)]
  
  y_min <-
    x1 * model_table$conf.low[model_table$term == term1]+
    x2 * model_table$conf.low[model_table$term == term2] +
    x1 * x2 * model_table$conf.low[model_table$term == paste0(term1, ':', term2)]
  
  y_max <-
    x1 * model_table$conf.high[model_table$term == term1]+
    x2 * model_table$conf.high[model_table$term == term2] +
    x1 * x2 * model_table$conf.high[model_table$term == paste0(term1, ':', term2)]
  
  dfplot <- data.frame(x1, x2, y, y_min, y_max)
  
  ggplot(dfplot, aes(x = x2, y = y, 
                     col = x1, fill = x1, 
                     ymin = y_min, 
                     ymax = y_max)) +
    # geom_freqpoly(data = merged_data, aes_string(x = term2), 
    #                freq = T, inherit.aes = F) +
    geom_pointrange() +
    xlab(term2) + ylab('Predicted value')
  
}


# 2-way fe's

## immigration
imm_mod_2fe_7d <- 
    merged_data %>%
    mutate(imm = `1130_clean`) %>%
  feglm(data = .,
        fml = 
           imm  ~ 
          mig_share_7d +
          crime_share_7d*mig_share_7d+
          capcrime_share_7d*mig_share_7d+
          refnums_share_7d*mig_share_7d+
          camps_share_7d*mig_share_7d+
          medit_share_7d*mig_share_7d+
          labmar_share_7d*mig_share_7d+
          deport_share_7d*mig_share_7d 
        | wave + lfdn # 2-way-fes
          
     )

summary(imm_mod_2fe_7d) # only very negative impact of refnums, capcrime rather positive

intervis(imm_mod_2fe_7d, 'mig_share_7d', 'refnums_share_7d')
intervis(imm_mod_2fe_7d, 'mig_share_7d', 'camps_share_7d')
intervis(imm_mod_2fe_7d, 'mig_share_7d', 'capcrime_share_7d')
intervis(imm_mod_2fe_7d, 'mig_share_7d', 'crime_share_7d')
intervis(imm_mod_2fe_7d, 'mig_share_7d', 'deport_share_7d')


### 30d
imm_mod_2fe_30d <- 
  merged_data %>%
  mutate(imm = `1130_clean`) %>%
  feglm(data = .,
        fml = 
          imm  ~ 
          mig_share_30d +
          crime_share_30d*mig_share_30d+
          capcrime_share_30d*mig_share_30d+
          refnums_share_30d*mig_share_30d+
          camps_share_30d*mig_share_30d+
          medit_share_30d*mig_share_30d+
          labmar_share_30d*mig_share_30d+
          deport_share_30d*mig_share_30d 
        | wave + lfdn # 2-way-fes
        
  )

summary(imm_mod_2fe_30d)
intervis(imm_mod_2fe_30d, 'mig_share_30d', 'camps_share_30d')


## integration
int_mod_2fe <- 
  merged_data %>%
  mutate(int = `1210_clean`*-1) %>%
  feglm(data = .,
        fml = 
          int ~ 
          mig_share_7d +
          crime_share_7d*mig_share_7d+
          capcrime_share_7d*mig_share_7d+
          refnums_share_7d*mig_share_7d+
          camps_share_7d*mig_share_7d+
          medit_share_7d*mig_share_7d+
          labmar_share_7d*mig_share_7d+
          deport_share_7d*mig_share_7d 
        | wave + lfdn # 2-way-fes
        
  )

summary(int_mod_2fe) # nothing remotely significant




# only exclusive readers
restricted_data <- 
  merged_data %>% 
  filter(n_read == 1)

## immigration
imm_mod_2fe_ex <- 
  restricted_data %>%
  mutate(imm = `1130_clean`) %>%
  feglm(data = .,
        fml = 
          imm  ~ 
          mig_share_7d +
          crime_share_7d*mig_share_7d+
          capcrime_share_7d*mig_share_7d+
          refnums_share_7d*mig_share_7d+
          camps_share_7d*mig_share_7d+
          medit_share_7d*mig_share_7d+
          labmar_share_7d*mig_share_7d+
          deport_share_7d*mig_share_7d 
        | lfdn + wave # 2-way-fes
        
  )

summary(imm_mod_2fe_ex) # only very negative impact of refnums, salience positive, capcrime rather positive



## integration
int_mod_2fe_ex <- 
  restricted_data %>%
  mutate(int = `1210_clean`*-1) %>%
  feglm(data = .,
        fml = 
          int ~ 
          mig_share_7d +
          crime_share_7d*mig_share_7d+
          capcrime_share_7d*mig_share_7d+
          refnums_share_7d*mig_share_7d+
          camps_share_7d*mig_share_7d+
          medit_share_7d*mig_share_7d+
          labmar_share_7d*mig_share_7d+
          deport_share_7d*mig_share_7d 
        | wave + lfdn # 2-way-fes
        
  )

summary(int_mod_2fe) # nothing remotely significant



# agenda-setting model

## immigration
agenda_model_imm <- 
  merged_data %>%
  mutate(imm = `1140_clean`) %>%
  feglm(data = .,
        fml = 
          imm  ~ 
          mig_share_7d
        | wave + lfdn # 2-way-fes
        
  )

summary(agenda_model_imm) # lol nichts

## no wave fes
agenda_model_imm <- 
  merged_data %>%
  mutate(imm = `1140_clean`) %>%
  feglm(data = .,
        fml = 
          imm  ~ 
          mig_share_7d
        | lfdn # 2-way-fes
        
  )

summary(agenda_model_imm) # wtf

agenda_model_int <- 
  merged_data %>%
  mutate(int = `1220_clean`) %>%
  feglm(data = .,
        fml = 
          int  ~ 
          mig_share_7d
        | wave + lfdn # 2-way-fes
        
  )

summary(agenda_model_int) # lol nichts

# 30d
## immigration
agenda_model_imm <- 
  merged_data %>%
  mutate(imm = `1140_clean`) %>%
  feglm(data = .,
        fml = 
          imm  ~ 
          mig_share_30d
        | wave + lfdn # 2-way-fes
        
  )

summary(agenda_model_imm) # lol nichts

### no wave fes

agenda_model_imm <- 
  merged_data %>%
  mutate(imm = `1140_clean`) %>%
  feglm(data = .,
        fml = 
          imm  ~ 
          mig_share_30d
        | lfdn
        
  )

summary(agenda_model_imm) # nein



agenda_model_int <- 
  merged_data %>%
  mutate(int = `1220_clean`) %>%
  feglm(data = .,
        fml = 
          int  ~ 
          mig_share_30d
        | wave + lfdn # 2-way-fes
        
  )

summary(agenda_model_int) # lol nichts


