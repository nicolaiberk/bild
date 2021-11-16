# ______________________________________________
# Media effects on issue definitions
# Goal: estimate DiD opinion shifts
# Procedure: load data, broad estimation, only paper estimates
# ______________________________________________
# Date:  Mon Sep 13 08:36:30 2021
# Author: Nicolai Berk
#  R version 4.0.3 (2020-10-10)
# ______________________________________________


# Setup ####
library(here)
library(dplyr)
library(tidyverse)
library(data.table)
library(haven)
library(lubridate)
library(glue)
library(fixest)

# load data ####
gles_p_long <- fread(here('data/gles/Panel/long_cleaned.csv')) %>% as.data.frame()

# broad estimates (all outlets) ####
coeftable <- data.frame()
mediumlist <- c('1681', '1661')
ego_issues <- 
  str_match(
    string = colnames(gles_p_long), 
    pattern = "2880(.*)_clean"
  )[,2] %>% 
  unique()
ego_issues <- paste0("2880", ego_issues[!is.na(ego_issues)])



for (medium in mediumlist){
  print(paste('Medium:', medium))
  if (medium == '1661'){
    papers_list <- letters[1:7]
    non_reader_var <- letters[8]
  } else if (medium == '1681'){
    papers_list <- letters[1:5]
    non_reader_var <- letters[6]
  }
  for (sample_restriction in c('only readers', 'only exclusive readers')){
    print(paste('Sample restriction:', sample_restriction))
    for (paper in papers_list){
      print(paste('Paper:', paper))
      for (issue in c(ego_issues, "1500", "1090", "1130", 
                      "1290", "1210", "1140", "1220", "1300", 
                      "1411", "1250", "430i")){
        print(paste('Issue:', issue))
        
        
        if (sample_restriction == 'only exclusive readers'){
          
          # filter only exclusive readers
          tempdta <- 
            gles_p_long %>% 
            mutate(n_papers = rowSums(gles_p_long %>% 
                                        select(contains(medium)) %>% 
                                        select(!contains(paste0(medium, non_reader_var))) %>% 
                                        select(contains('bin')))) %>% 
            filter(n_papers == 1)
          
        }else{
          
          # additional estimate with relaxed treatment restriction
          gles_p_long$non_reader <- gles_p_long[, c(paste0(medium, non_reader_var, '_clean'))] == 1
            
          tempdta <- 
            gles_p_long %>% 
            filter(non_reader != T)
          
        }
        
        tempdta$paper_reader <- tempdta[, paste0(medium, paper, '_bin')]
        
        tempdta$issue <- scale(tempdta[, paste0(issue, '_clean')], center = T, scale = T)
        
        tempdta <- 
          tempdta %>% 
          filter(!is.na(issue))
        
        wavelist <- sort(unique(as.integer(tempdta$wave)))
        
        if (length(wavelist) >= 2){
          
          # estimate DiD ####
          for (wave_id in 2:length(wavelist)){
            tempdta_model <- 
              tempdta %>%
              mutate(treat = ifelse(wave == as.character(wavelist[wave_id]), 1, NA)) %>%
              mutate(treat = ifelse(wave == as.character(wavelist[(wave_id-1)]), 0, treat)) %>% 
              filter(!is.na(treat))
            
            if ((length(unique(tempdta_model$paper_reader)) == 2) &
              (length(unique(tempdta_model$treat)) == 2)){
  
              ## fes dropped bc takes too long
              sum_lm <- summary(lm(issue ~ treat*paper_reader, data = tempdta_model))
              sum_fe <- summary(feglm(issue ~ treat*paper_reader | lfdn, data = tempdta_model))
              
              coeftable <- 
                data.frame(
                medium   = medium,
                paper    = paper,
                issue    = issue,
                wave     = wavelist[wave_id],
                date     = max(tempdta_model$date_new[tempdta_model$wave == wavelist[wave_id]]),
                coef_fe  = sum_fe$coeftable['treat:paper_readerTRUE', 1],
                p_fe     = sum_fe$coeftable['treat:paper_readerTRUE', 4],
                coef_lm  = sum_lm$coefficients['treat:paper_readerTRUE',][1],
                p_lm     = sum_lm$coefficients['treat:paper_readerTRUE', ][4],
                respondents = sample_restriction,
                sample_n = nrow(tempdta_model)
                ) %>% 
                rbind(coeftable)
              
              
                
            }
          }
        }
      }
    }
  }
}

  
write.csv(coeftable, here('data/coeftable_gles_noimp.csv'))
