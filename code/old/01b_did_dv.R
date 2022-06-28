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

## vis relevant waves
gles_p_long %>% 
  filter(readership != "", !is.na(`1130_clean`)) %>% 
  ggplot(aes(date_clean, wave)) + 
  geom_bin2d()

# simple DiD for Reichelt takeover
gles_p_long$post <- 
  gles_p_long$date_clean > as.Date("2017-01-01")

gles_p_long_nona$post <- 
  gles_p_long_nona$date_clean > as.Date("2017-01-01")

gles_p_long$treat <- 
  gles_p_long$readership == "Bild"

gles_p_long_nona$treat <- 
  gles_p_long_nona$readership == "Bild"

gles_p_long <- 
  gles_p_long %>% 
  filter(wave == 1) %>% 
  mutate(imm_att = cut(`1130_clean`, breaks = c(-Inf, -1.5, 1.5, Inf), labels = c("liberal", "moderate", "conservative"))) %>% 
  select(imm_att, lfdn) %>% 
  right_join(gles_p_long, by = "lfdn")

gles_p_long_nona <- 
  gles_p_long_nona %>% 
  filter(wave == 1) %>% 
  mutate(imm_att = cut(`1130_clean`, breaks = c(-Inf, -1.5, 1.5, Inf), labels = c("liberal", "moderate", "conservative"))) %>% 
  select(imm_att, lfdn) %>% 
  right_join(gles_p_long_nona, by = "lfdn")


## vis
gles_p_long %>% 
  filter(readership != "") %>% 
  filter(!is.na(date_clean), wave != 5) %>% 
  select(wave, treat, `1130_clean`, date_clean) %>% 
  group_by(wave, treat) %>% 
  summarise(date_clean = min(date_clean, na.rm = T),
            mig_att_mean = mean(`1130_clean`, na.rm = T),
            mig_att_sd   = sd(`1130_clean`, na.rm = T),
            resps = n()) %>%
  mutate(
    mig_lower = mig_att_mean + qt(0.025, resps-1)*mig_att_sd/sqrt(resps),
    mig_upper = mig_att_mean + qt(0.975, resps-1)*mig_att_sd/sqrt(resps)
  ) %>% 
  ggplot(aes(x = date_clean, y = mig_att_mean, ymin = mig_lower, ymax = mig_upper, 
             col = treat, fill = treat, group = treat)) +
  geom_line() + 
  geom_ribbon(alpha = 0.3) +
  geom_vline(xintercept = as.Date("2017-01-01"), col = 'red', lty = 2)

gles_p_long_nona %>% 
  select(wave, treat, `1130_clean`, date_clean) %>% 
  group_by(wave, treat) %>% 
  summarise(date_clean = min(date_clean, na.rm = T),
            mig_att_mean = mean(`1130_clean`, na.rm = T),
            mig_att_sd   = sd(`1130_clean`, na.rm = T),
            resps = n()) %>%
  mutate(
    mig_lower = mig_att_mean + qt(0.025, resps-1)*mig_att_sd/sqrt(resps),
    mig_upper = mig_att_mean + qt(0.975, resps-1)*mig_att_sd/sqrt(resps)
  ) %>% 
  ggplot(aes(x = date_clean, y = mig_att_mean, ymin = mig_lower, ymax = mig_upper, 
             col = treat, fill = treat, group = treat)) +
  geom_line() + 
  geom_ribbon(alpha = 0.3) +
  geom_vline(xintercept = as.Date("2017-01-01"), col = 'red', lty = 2)




gles_p_long %>% 
  filter(readership != "") %>% 
  filter(!is.na(date_clean), wave != 5,!is.na(imm_att)) %>% 
  select(wave, `1130_clean`, date_clean, treat, imm_att) %>% 
  mutate(treat_immatt = paste(as.character(treat), as.character(imm_att))) %>% 
  group_by(wave, treat_immatt, treat) %>% 
  summarise(date_clean = min(date_clean, na.rm = T),
            mig_att_mean = mean(`1130_clean`, na.rm = T),
            mig_att_sd   = sd(`1130_clean`, na.rm = T),
            resps = n()) %>%
  mutate(
    mig_lower = mig_att_mean + qt(0.025, resps-1)*mig_att_sd/sqrt(resps),
    mig_upper = mig_att_mean + qt(0.975, resps-1)*mig_att_sd/sqrt(resps)
  ) %>% 
  ggplot(aes(x = date_clean, y = mig_att_mean, ymin = mig_lower, ymax = mig_upper, 
             col = treat, fill = treat, group = treat_immatt)) +
  geom_line() + 
  geom_ribbon(alpha = 0.3) +
  geom_vline(xintercept = as.Date("2017-01-01"), col = 'red', lty = 2)


gles_p_long_nona %>% 
  select(wave, `1130_clean`, date_clean, treat, imm_att) %>% 
  mutate(treat_immatt = paste(as.character(treat), as.character(imm_att))) %>% 
  group_by(wave, treat_immatt, treat) %>% 
  summarise(date_clean = min(date_clean, na.rm = T),
            mig_att_mean = mean(`1130_clean`, na.rm = T),
            mig_att_sd   = sd(`1130_clean`, na.rm = T),
            resps = n()) %>%
  mutate(
    mig_lower = mig_att_mean + qt(0.025, resps-1)*mig_att_sd/sqrt(resps),
    mig_upper = mig_att_mean + qt(0.975, resps-1)*mig_att_sd/sqrt(resps)
  ) %>% 
  ggplot(aes(x = date_clean, y = mig_att_mean, ymin = mig_lower, ymax = mig_upper, 
             col = treat, fill = treat, group = treat_immatt)) +
  geom_line() + 
  geom_ribbon(alpha = 0.3) +
  geom_vline(xintercept = as.Date("2017-01-01"), col = 'red', lty = 2)


## Salience
gles_p_long %>% 
  filter(readership %in% c("Bild", "Welt")) %>% 
  filter(!is.na(date_clean), wave != 5) %>% 
  select(wave, readership, `840_clean`, date_clean) %>% 
  group_by(wave, readership) %>% 
  summarise(date_clean = min(date_clean, na.rm = T),
            mig_att_mean = mean(`840_clean` %in% c(3411:3413, 3750:3759), na.rm = T),
            mig_att_sd   = sd(`840_clean` %in% c(3411:3413, 3750:3759), na.rm = T),
            resps = n()) %>%
  mutate(
    mig_lower = mig_att_mean + qt(0.025, resps-1)*mig_att_sd/sqrt(resps),
    mig_upper = mig_att_mean + qt(0.975, resps-1)*mig_att_sd/sqrt(resps)
  ) %>% 
  ggplot(aes(x = date_clean, y = mig_att_mean, ymin = mig_lower, ymax = mig_upper, 
             col = readership, fill = readership, group = readership)) +
  geom_line() + 
  geom_ribbon(alpha = 0.3) +
  geom_vline(xintercept = as.Date("2017-01-01"), col = 'red', lty = 2)


# gles_p_long %>% 
#   filter(readership != "") %>% 
#   filter(!is.na(date_clean), wave != 5) %>% 
#   select(wave, readership, `1130_clean`, date_clean) %>% 
#   group_by(wave, readership) %>% 
#   summarise(date_clean = min(date_clean, na.rm = T),
#             mig_att_mean = mean(`1130_clean`, na.rm = T),
#             mig_att_sd   = sd(`1130_clean`, na.rm = T),
#             resps = n()) %>%
#   mutate(
#     mig_lower = mig_att_mean + qt(0.025, resps-1)*mig_att_sd/sqrt(resps),
#     mig_upper = mig_att_mean + qt(0.975, resps-1)*mig_att_sd/sqrt(resps)
#   ) %>% 
#   ggplot(aes(x = date_clean, y = mig_att_mean, ymin = mig_lower, ymax = mig_upper, 
#              col = readership, fill = readership, group = readership)) +
#   geom_line() + 
#   geom_ribbon(alpha = 0.3) +
#   geom_vline(xintercept = as.Date("2017-01-01"), col = 'red', lty = 2)


# ## full time frame
# full_model <- 
#   lm(`1130_clean` ~ treat*post, gles_p_long)
# summary(full_model) # insignificant
# 
# ## pre-post wave
# prepost_model <- 
#   lm(`1130_clean` ~ treat*post, gles_p_long %>% filter(wave %in% c(1,3)))
# summary(prepost_model) # negative - more LIBERAL attitudes!!
# 
# 
# # Bild vs Welt
# 
# 
# 
# ## full time frame
# full_model <- 
#   lm(`1130_clean` ~ treat*post, gles_p_long %>% 
#        filter(readership %in% c("Bild", "Welt")))
# summary(full_model) # insignificant negative
# 
# ## pre-post wave
# prepost_model <- 
#   lm(`1130_clean` ~ treat*post, gles_p_long %>% filter(wave %in% c(1,3)))
# summary(prepost_model) # negative - more LIBERAL attitudes!!
# 

# loop for case DiD with different dependents vs Welt vs non-Bild


gles_p_long <- # who are these people?   
  gles_p_long %>% 
  filter(readership != "")

gles_p_long$readership <- 
  factor(gles_p_long$readership, levels = c("Welt", "Bild", "FAZ", "FR", "None", "SZ", "taz"))


reichelt_ests <- data.frame()

for (ref in c("non-Bild", "Welt")){
  
  if (ref == "non-Bild"){
    gles_p_long$iv <- gles_p_long$treat
  }else if (ref == "Welt"){
    gles_p_long$iv <- gles_p_long$readership
  }else{
    stop(paste('Unknown reference:', ref))
  }
  
  for (dv in c("Immigration Attitude", "Integration Attitude", "MIP: Migration",
               "AfD thermometer", "CDU thermometer", "CSU thermometer", "SPD thermometer", 
               "Greens thermometer", "FDP thermometer", "Linke thermometer")){
    
    if(dv == "Immigration Attitude"){
      gles_p_long$dv <- gles_p_long$`1130_clean`
    }else if (dv == "Integration Attitude"){
      gles_p_long$dv <- gles_p_long$`1210_clean`
    }else if (dv == "Immigration salience"){
      gles_p_long$dv <- gles_p_long$`1140_clean`
    }else if (dv == "MIP: Migration"){
      gles_p_long$dv <- 
        gles_p_long$`840_clean` %in% c(3411:3413, 3750:3759) 
    }else if (dv == "SMIP: Migration"){
      gles_p_long$dv <- 
        gles_p_long$`860_clean` %in% c(3411:3413, 3750:3759) 
    }else if (dv == "AfD thermometer"){
      gles_p_long$dv <- gles_p_long$`430i_clean` 
    }else if (dv == "CDU thermometer"){
      gles_p_long$dv <- gles_p_long$`430a_clean` 
    }else if (dv == "CSU thermometer"){
      gles_p_long$dv <- gles_p_long$`430b_clean` 
    }else if (dv == "SPD thermometer"){
      gles_p_long$dv <- gles_p_long$`430c_clean` 
    }else if (dv == "FDP thermometer"){
      gles_p_long$dv <- gles_p_long$`430d_clean` 
    }else if (dv == "Greens thermometer"){
      gles_p_long$dv <- gles_p_long$`430e_clean` 
    }else if (dv == "Linke thermometer"){
      gles_p_long$dv <- gles_p_long$`430f_clean` 
    }else{
      stop(paste(dv, "not found!"))
    }
    
    for (model in c("lm", "fe")){
      
        if (model == "lm"){
          
            
            did_model <- feglm(dv ~ post*iv, cluster = c("lfdn"),  data = gles_p_long)
            
          
        }else if (model == "fe"){
          
            did_model <- feglm(dv ~ post*iv | lfdn, cluster = c("lfdn"), data = gles_p_long)
          
        }
      
      reichelt_ests <- 
        reichelt_ests %>% 
        rbind(
          data.frame(
            dv = dv,
            ref = ref,
            model = model,
            est = did_model$coefficients[paste0("postTRUE:iv", 
                                                ifelse(ref == "non-Bild", "TRUE", "Bild"))],
            lower = confint(did_model)[paste0("postTRUE:iv", 
                                              ifelse(ref == "non-Bild", "TRUE", "Bild")), 1],
            upper = confint(did_model)[paste0("postTRUE:iv", 
                                              ifelse(ref == "non-Bild", "TRUE", "Bild")), 2]
          )
        )
    }
  }
}

## vis
reichelt_ests %>% 
  filter(ref == "non-Bild") %>% 
  # filter(dv %in% c("AfD thermometer", "Immigration Attitude", "MIP: Migration", "Greens thermometer")) %>% 
  ggplot(aes(x = est, xmin = lower, xmax = upper, y = model))+
  geom_pointrange(position = position_dodge2(width = 0.5)) +
  geom_vline(xintercept = 0) +
  facet_wrap(~dv)

ggsave(here("paper/vis/case_multiple_dvs.png"), width = 10, height = 3)




## condition on pre-treatment immigration attitude
library(boot)
set.seed(42)
B <- 10

reichelt_ests <- data.frame()

effs1 <- function(dat, inds){
  
  # sample observations, then wide to long
  dta_model <- 
    dat[inds,] %>% 
    pivot_longer(cols = contains("treat"), names_to = "wave", values_to = "treat", names_prefix = "(.*)_") %>% 
    select(lfdn, wave, treat)
  
  dta_model <- 
    dat[inds,] %>% 
    pivot_longer(cols = contains("dv"), names_to = "wave", values_to = "dv", names_prefix = "(.*)_") %>% 
    select(c(contains("dv"), wave, lfdn)) %>% 
    left_join(., dta_model, by = c("wave", "lfdn"))
  
  dta_model <- 
    dat[inds,] %>% 
    pivot_longer(cols = contains("imm_att"), names_to = "wave", values_to = "imm_att", names_prefix = "(.*)_") %>% 
    select(c(contains("imm_att"), wave, lfdn)) %>% 
    left_join(., dta_model, by = c("wave", "lfdn"))
  
  
  dta_model <- 
    dat[inds,] %>% 
    pivot_longer(cols = contains("post"), names_to = "wave", values_to = "post", names_prefix = "(.*)_") %>% 
    select(c(contains("post"), wave, lfdn)) %>% 
    left_join(., dta_model, by = c("wave", "lfdn"))
  
  did_model <- lm(dv ~ post*treat*imm_att, data = dta_model)
  coe <- coef(did_model)
  return(
    c(
      liberal = coe[["postTRUE:treatTRUE"]],
      moderate =
        coe[["postTRUE:treatTRUE"]] +
        coe[["postTRUE:treatTRUE:imm_attmoderate"]],
      conservative =
        coe[["postTRUE:treatTRUE"]] +
        coe[["postTRUE:treatTRUE:imm_attconservative"]]
      )
    )
}




# for (ref in c("non-Bild", "Welt")){
#   
#   if (ref == "non-Bild"){
#     gles_p_long$iv <- gles_p_long$treat
#   }else if (ref == "Welt"){
#     gles_p_long$iv <- gles_p_long$readership
#   }else{
#     stop(paste('Unknown reference:', ref))
#   }


# bootstrap DiDiD - runs forever
for (dv in c("Immigration Attitude", "MIP: Migration",
             "AfD thermometer", "CDU thermometer", "CSU thermometer", "SPD thermometer", 
             "Greens thermometer", "FDP thermometer", "Linke thermometer")){
  
  print(paste("Estimating effect on", dv, "..."))
  
  if(dv == "Immigration Attitude"){
    gles_p_long$dv <- gles_p_long$`1130_clean`
  }else if (dv == "Integration Attitude"){
    gles_p_long$dv <- gles_p_long$`1210_clean`
  }else if (dv == "Immigration salience"){
    gles_p_long$dv <- gles_p_long$`1140_clean`
  }else if (dv == "MIP: Migration"){
    gles_p_long$dv <- 
      gles_p_long$`840_clean` %in% c(3411:3413, 3750:3759)
  }else if (dv == "SMIP: Migration"){
    gles_p_long$dv <- 
      gles_p_long$`860_clean` %in% c(3411:3413, 3750:3759) 
  }else if (dv == "AfD thermometer"){
    gles_p_long$dv <- gles_p_long$`430i_clean` 
  }else if (dv == "CDU thermometer"){
    gles_p_long$dv <- gles_p_long$`430a_clean` 
  }else if (dv == "CSU thermometer"){
    gles_p_long$dv <- gles_p_long$`430b_clean` 
  }else if (dv == "SPD thermometer"){
    gles_p_long$dv <- gles_p_long$`430c_clean` 
  }else if (dv == "FDP thermometer"){
    gles_p_long$dv <- gles_p_long$`430d_clean` 
  }else if (dv == "Greens thermometer"){
    gles_p_long$dv <- gles_p_long$`430e_clean` 
  }else if (dv == "Linke thermometer"){
    gles_p_long$dv <- gles_p_long$`430f_clean` 
  }


  # bootstrap estimate 
  dta_boot <- 
    gles_p_long %>% 
    select(lfdn, wave, treat, dv, post, imm_att) %>% 
    pivot_wider(id_cols = c(lfdn), values_from = c(treat, dv, imm_att, post), names_from = c(wave))
  
  
  res1 <- boot(dta_boot, effs1, R = B, parallel = "snow")
  
  for (estimate in c("liberal", "moderate", "conservative")){
    
    boot_ests <- 
      res1$t[,
        case_when(
        estimate == "liberal" ~ 1,
        estimate == "moderate" ~ 2,
        estimate == "conservative" ~ 3,
      )]
      
    est <- mean(boot_ests)
    lower <- quantile(boot_ests, 0.025)[[1]]
    upper <- quantile(boot_ests, 0.975)[[1]]
    
    reichelt_ests <- 
      reichelt_ests %>% 
      rbind(
        data.frame(
          dv = dv,
          ref = ref,
          estimate = estimate,
          est = est,
          lower = lower,
          upper = upper
        ))
  }
}
# }

reichelt_ests$estimate <- factor(reichelt_ests$estimate, levels = c("conservative", "moderate", "liberal"), ordered = T)

reichelt_ests %>% 
  ggplot(aes(x = est, xmin = lower, xmax = upper, y = estimate, col = estimate))+
  geom_pointrange(position = position_dodge2(width = 0.5)) +
  geom_vline(xintercept = 0) +
  facet_wrap(~dv, scales = "free")

ggsave(here("paper/vis/case_multiple_dvs_ia.png"), width = 10, height = 3)




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

