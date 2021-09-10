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

# 1. load data ####
load(here("data/coefs_media.Rdata"))
framing_dids <- 
  framing_dids %>% 
  filter(!is.na(p))

attitude_dids <- read.csv(here('data/coeftable.csv'))

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

## correlate with frames
plots <- list()
efftable <- data.frame()
for (issue in c("immigration", "integration")){
  for (topic in unique(framing_dids$frame)){
    for (ind in c("coef", "attention")){
      for (lag in c("g", "1w", "1m", "6m")){
          
        
        if (issue == "immigration"){
          dv_set <- imm_dids %>% mutate(delta_att = coef_fe,
                                        paper = paper_name)
        }else{
          dv_set <- int_dids %>% mutate(delta_att = coef_fe,
                                        paper = paper_name)
        }
        
        if (ind == 'coef'){
          iv_set <- framing_dids %>% 
            filter(frame == topic & (dv_lag == lag)) %>% 
            mutate(delta_frame = coef) %>% 
            mutate(wave = pre_wave)
        }else{
          iv_set <- framing_dids %>% 
            filter(frame == topic & (dv_lag == lag)) %>% 
            mutate(delta_frame = log(attention)) %>% 
            mutate(wave = pre_wave) %>% 
            filter(delta_frame != -Inf)
        }
        
        
        full_set <- merge(dv_set, iv_set, by = c("paper", "wave"))
          
        mod <- summary(lm(delta_att ~ delta_frame, full_set))
        
        beta <- mod$coefficients[2,1] %>% round(3)
        stde <- mod$coefficients[2,2] %>% round(3)
        tval <- mod$coefficients[2,3] %>% round(3)
        pval <- mod$coefficients[2,4] %>% round(3)
        
        
        plots[[issue]][[lag]][[ind]][[topic]] <- 
          ggplot(full_set, aes(delta_frame, delta_att)) +
          geom_point() +
          geom_smooth(method = "lm") +
          ggtitle(topic) + 
          xlab("DiD media framing") + ylab(paste('media', ind))
        
        efftable <- efftable %>% 
          rbind(data.frame(
            dv = issue,
            dv_lag = lag,
            iv = ind,
            frame = topic,
            coef = beta,
            std_err = stde,
            t_value = tval,
            p_value = pval
          ))
        
      }
    }
  }
}

save(efftable, file = here('data/efftable.Rdata'))

for (issue in c('immigration', 'integration')){
  for (lag in c("g", "1w", "1m", "6m")){
    for (ind in c('coef', 'attention')){
      grid.arrange(grobs = plots[[issue]][[lag]][[ind]],
                   top = paste(issue, lag, ind)) %>% 
        ggsave(filename = here(paste0("paper/vis/effectplots/did_", issue, '_', lag, '_', ind, ".png")))
    }
  }
}


## 3.4 full fe model with media controls ####
