# ______________________________________________
# Bild
# Goal: Fixed effect estimates
# ______________________________________________
# Date:  Wed Dec 15 14:12:14 2021
# Author: Nicolai Berk
#  R version 4.1.1 (2021-08-10)
# ______________________________________________


# Setup ####
library(tidyverse)
library(dplyr)
library(here)
library(data.table)


#################################
#### CODE LIKELY BROKEN HERE #### 
#################################

# (working with old dataset)

# 1. individual-level model ####

fread("data/merged_2022-02-15.csv")

## visualise simple correlations frame exposure X opinion

## immigration attitude

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


# 3. save fe model ####
datum <- Sys.Date()
save(efftable, file = here(paste0('data/efftable_fe_', datum, '.Rdata')))
