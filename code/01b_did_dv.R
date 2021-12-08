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


# DiD with treatment X readership var

coeftable <- data.frame()
sample_set <- c("exclusive readers", "all readers", "all respondents")

for (sample_restriction in sample_set){

  if (sample_restriction == 'exclusive readers'){
    
    # filter only exclusive readers
    tempdta_res <- 
      gles_p_long %>% 
      mutate(n_papers = rowSums(gles_p_long %>% 
                                  select(contains("1661")) %>% 
                                  select(!contains("1661h")) %>% 
                                  select(contains('bin')))) %>% 
      filter(n_papers == 1)
    
  }else if (sample_restriction == "all readers"){
    
    tempdta_res <- 
      gles_p_long %>% 
      filter(!is.na(readership) & (readership != "")) %>% 
      filter(readership != "None")
    
  }else if (sample_restriction == "all respondents"){
    
    tempdta_res <- 
      gles_p_long %>% 
      filter(!is.na(readership) & (readership != ""))
    
  }else{
    stop(paste0("Unknown sample restriction: ", sample_restriction))
  }
  
  if (nrow(tempdta_res) == 0){next}
  
  post_wave_set <- 
    tempdta_res %>% 
    filter(wave != "1") %>% 
    select(wave) %>% 
    unique() %>% 
    unlist() %>% 
    as.character()
  
  

  for (post_wave in post_wave_set){
  
    
    min_date_post <- 
      tempdta_res %>% 
      filter(wave == post_wave) %>% 
      filter(!is.na(date_clean)) %>% 
      select(date_clean) %>% 
      unlist() %>% 
      min() %>% 
      as.Date(origin = "1970-01-01")
  
    if (!is.finite(min_date_post) | is.na(min_date_post)){
      
      stop(cat("No available date for post-wave:",
               "\n\t- Sample: ", sample_restriction,
               "\n\t- Post-wave: ", post_wave
               ))
    
    }
  
    tempdta_res$post <- tempdta_res$date_clean >= min_date_post
  
    for (reference in c("Other", "Pooled", "Welt", "None")){
      
      # Skip duplicate reference categories 
      
      ## "Other" and "Welt" same for readers only and full
      if (sample_restriction == "All" & 
          ((reference == "Pooled") | (reference == "Welt"))){next}
      
      ## "None" undefined for readers only samples
      if (sample_restriction != "All" & (reference == "None")){next}
    
      for (dv in c("imm", "int", "afd")){
        
        if (dv == "imm"){
          tempdta_res$dv <- tempdta_res$`1130_clean`
        }else if (dv == "int"){
          tempdta_res$dv <- tempdta_res$`1210_clean`*(-1)
        }else if (dv == "afd"){
          tempdta_res$dv <- tempdta_res$`430i_clean`
        }else{
          stop(paste0("Error: Unexpected dependent '", dv, "'."))
        }
        
        tempdta_dv <- 
          tempdta_res %>% 
          filter(!is.na(dv))
        
        if (nrow(tempdta_dv) == 0){next}
        
        for (sample_period in c("all", "pre-post")){
          
          if (sample_period == "all"){
            
            tempdta_per <- tempdta_dv
              
          }else if (sample_period == "pre-post"){
            
            max_pre_date <- 
              tempdta_dv %>% 
              filter(post == F) %>% 
              select(date_clean) %>% 
              unlist() %>% 
              max() %>% 
              as.Date(origin = "1970-01-01")
            
            pre_wave <- 
              tempdta_dv %>% 
              filter(date_clean == max_pre_date) %>% 
              select(wave) %>% unlist %>% unique()
            
            tempdta_per <- 
              tempdta_dv %>% 
              filter(wave %in% c(pre_wave, post_wave))
            
          }else{
            stop(paste0("Error: Unexpected period: '", sample_period, "'."))
          }
          
          for (paper in unique(tempdta_per$readership)){
            
            if (paper != reference){
              
              tempdta_paper <- 
                tempdta_per %>% 
                filter(readership %in% 
                         if(reference == "Pooled"){
                           unique(gles_p_long$readership)
                         }else{
                           c(paper, reference) 
                         }) %>% 
                mutate(paper_reader = readership == paper)
            
              if (nrow(tempdta_paper) == 0){
                
                cat("No observations:",
                      "\n\t- Paper:", paper,
                      "\n\t- DV:", dv, 
                      "\n\t- Sample:", sample_restriction,
                      "\n\t- Wave (post):", post_wave,
                      "\n\t- Reference category:", reference,
                      "\n\t- Sample period:", sample_period,
                      "\n\n Skipping iteration. \n\n")
                
                next
                
              }
              
              if (table(tempdta_paper$post, tempdta_paper$paper_reader) %>% 
                  dim() %>% min() != 2 |
                  (table(tempdta_paper$post, tempdta_paper$paper_reader) %>% 
                   min() == 0)){
                cat("Empty cell in exposure-post-table:",
                    "\n\t- Paper:", paper,
                    "\n\t- DV:", dv, 
                    "\n\t- Sample:", sample_restriction,
                    "\n\t- Wave (post):", post_wave,
                    "\n\t- Reference category:", reference,
                    "\n\t- Sample period:", sample_period,
                    "\n\n Skipping iteration. \n\n")
                next
              }
                  
              model_did <- 
                tempdta_paper %>% 
                lm(formula = dv ~ post * paper_reader, data = .)
          
              
              coeftable <- 
                data.frame(
                  est   = model_did$coefficients[["postTRUE:paper_readerTRUE"]],
                  lower = confint(model_did)["postTRUE:paper_readerTRUE", 1],
                  upper = confint(model_did)["postTRUE:paper_readerTRUE", 2],
                  paper = paper,
                  period = sample_period,
                  reference = reference,
                  dv = dv,
                  sample = sample_restriction,
                  post_wave = post_wave
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

