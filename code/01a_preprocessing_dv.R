
# ______________________________________________
# Media effects on issue definitions
# Goal: Preprocess survey data
# Procedure: load, pivot, clean
# ______________________________________________
# Date:  Mon Sep 13 08:17:34 2021
# Author: Nicolai Berk
#  R version 4.0.3 (2020-10-10)
# ______________________________________________

# 0. load ####

library(here)
library(tidyverse)
library(data.table)
library(haven)
library(lubridate)
library(glue)
library(naniar)
library(dplyr)

gles_p <- read_dta(here('data/gles/Panel/GLESMerge/ZA6838_allwaves_sA_v4-0-0.dta'))

## id var
gles_p$lfdn <- as.factor(gles_p$lfdn)



# 1. Pivot wide -> long ####


## date
date_vars <- colnames(gles_p)[grep(pattern = 'datetime', colnames(gles_p))]

### date
gles_p_long <- gles_p %>%
  select(c(lfdn, contains("datetime"))) %>%
  pivot_longer(
    cols = c(contains("datetime"))) %>%
  mutate(
    wave = str_match(name, "kp(.*)_")[,2],
    date_new = value
  ) %>%
  select(lfdn, wave, date_new)

### vis missings
vis_miss(gles_p_long[sample(1:nrow(gles_p_long), 10000),])


## media vars

mediumlist <- c('1681', '1661')

for (medium in mediumlist){
  
  if (medium == '1661'){
    papers_list <- letters[1:8]
    # papers_label_list <- c()
  }else{
    papers_list <- letters[1:6]
    # papers_label_list <- c('Tagesschau', 'Heute_Journal', 'RTL_Aktuell', 'Sat.1_Nachrichten', 'Andere_Sender', 'Keine_Fernsehnachrichten')
  }
  
  for (paper in papers_list){
      
      tempdta <- gles_p
      
      ## papervar
      papervar_vars <- colnames(tempdta)[grep(pattern = paste0(medium, paper), colnames(tempdta))]
      papervar_waves <- str_match(papervar_vars, 'kp(.*)_')[,2]
      
      ## wide -> long
      ### paper
      tempdta <- tempdta %>% 
        select(c(lfdn, contains(paste0(medium, paper)))) %>%
        select(!contains('flag')) %>% 
        pivot_longer(
          cols = contains(paste0(medium, paper))) %>% 
        mutate(
          wave = str_match(name, "kp(.*)_")[,2],
          !! str_match(papervar_vars[1], 'kp\\d+_(.*)')[2] := value
        ) %>% 
        select(lfdn, wave, contains(paste0(medium, paper)))
        
      
      ### merge, keeping all cases
      gles_p_long <- merge(gles_p_long, tempdta, by = c("lfdn", "wave"), all.x = T)
  }
  ### vis missings
  gles_p_long[sample(1:nrow(gles_p_long), 10000),] %>% select(contains(medium)) %>% vis_miss()
}






## same for news magazines (1701)
papers_list <- letters[1:4]

for (paper in papers_list){
  for (sub in letters[1:3]){
    
    
    tempdta <- gles_p
    
    ## papervar
    papervar_vars <- colnames(tempdta)[grep(pattern = paste0('1701', paper, sub), colnames(tempdta))]
    papervar_waves <- str_match(papervar_vars, 'kp(.*)_')[,2]
    
    
    ## wide -> long
    ### paper
    tempdta <- tempdta %>% 
      select(c(lfdn, contains(paste0('1701', paper, sub)))) %>% 
      pivot_longer(
        cols = contains(paste0('1701', paper, sub))) %>% 
      mutate(
        wave = str_match(name, "kp(.*)_")[,2],
        !! str_match(papervar_vars[1], 'kp\\d+_(.*)')[2] := value
      ) %>% 
      select(lfdn, wave, contains(str_match(papervar_vars[1], 'kp\\d+_(.*)')[2]))
    
    ### merge
    gles_p_long <- merge(gles_p_long, tempdta, by = c("lfdn", "wave"), all.x = T)
    
        
  }
}

gles_p_long[sample(1:nrow(gles_p_long), 10000),] %>% select(contains('1701')) %>% vis_miss()


## add spiegel online variable
tempdta <- gles_p

## papervar
papervar_vars <- gles_p %>% select(contains('1702')) %>% colnames()
papervar_waves <- str_match(papervar_vars, 'kp(.*)_')[,2]


## wide -> long
### paper
tempdta <- tempdta %>% 
  select(c(lfdn, contains('1702'))) %>% 
  pivot_longer(
    cols = contains('1702')) %>% 
  mutate(
    wave = str_match(name, "kp(.*)_")[,2],
    !! str_match(papervar_vars[1], 'kp\\d+_(.*)')[2] := value
  ) %>% 
  select(lfdn, wave, contains('1702'))

### merge
gles_p_long <- merge(gles_p_long, tempdta, by = c("lfdn", "wave"), all.x = T)

### vis missings
gles_p_long[sample(1:nrow(gles_p_long), 10000),] %>% select(contains('1702')) %>% vis_miss()



# issues

## extract ego-issues
ego_issues <- 
  str_match(
    string = colnames(gles_p), 
    pattern = "kp.*\\d+_2880(.*)"
  )[,2] %>% 
  unique()
ego_issues <- paste0("2880", ego_issues[!is.na(ego_issues)])


for (issue in c(ego_issues, "1500", "1090", "1130", 
                "1290", "1210", "1140", "1220", "1300", 
                "1411", "1250", "1260")){
  
  tempdta <- gles_p
  
  ## attitude
  issue_vars <- colnames(tempdta)[grep(pattern = paste0(issue, '$'), colnames(tempdta))]
  issue_waves <- str_match(issue_vars, 'kp(.*)_')[,2]
  
  
  ### issue
  tempdta <- tempdta %>% 
    select(c(lfdn, ends_with(issue))) %>% 
    pivot_longer(
      cols = c(ends_with(issue))) %>% 
    mutate(
      wave = str_match(name, "kp(.*)_")[,2],
      !! issue := value
    ) %>% 
    select(lfdn, wave, contains(issue))
  
  
  gles_p_long <- merge(gles_p_long, tempdta, by = c("lfdn", "wave"), all.x = T)
  
  ### vis missings
  gles_p_long[sample(1:nrow(gles_p_long), 10000),] %>% select(contains(issue)) %>% vis_miss()
  
  
}

# party preference
gles_p_long <- 
  gles_p %>% 
  select(c(lfdn, contains('190bb'))) %>% 
  pivot_longer(
    cols = c(ends_with('190bb'))) %>% 
  mutate(
    wave = str_match(name, "kp(.*)_")[,2],
    `190b` = value
  ) %>% 
  select(lfdn, wave, contains('190b')) %>% 
  merge(gles_p_long, ., by = c("lfdn", "wave"), all.x = T)


# afd scalometer
gles_p_long <- 
  gles_p %>% 
  select(c(lfdn, contains('430i'))) %>% 
  pivot_longer(
    cols = c(ends_with('430i'))) %>% 
  mutate(
    wave = str_match(name, "kp(.*)_")[,2],
    `430i` = value
  ) %>% 
  select(lfdn, wave, contains('430i')) %>% 
  merge(gles_p_long, ., by = c("lfdn", "wave"), all.x = T)



## save
fwrite(gles_p_long, file = here('data/gles/Panel/long.csv'))



# 2. Cleaning ####

# load raw data
rm(list = ls())
gles_p_long <- fread(file = here('data/gles/Panel/long.csv')) %>% as.data.frame()

# define parameters
media_vars <- gles_p_long %>% select(contains('1661'), contains('1681')) %>% select(!contains('1661h')) %>% select(!contains('1681f')) %>%  colnames()
newsm_vars <- gles_p_long %>% select(contains('1701')) %>% colnames()
non_vars   <- gles_p_long %>% select(contains('1661h'), contains('1681f')) %>%  colnames()
issue_vars <- gles_p_long %>% select(contains('2880'), "1500", "1090", "1130", 
                                     "1290", "1210", "1140", "1220", "1300", 
                                     "1411", "1250", "1260") %>% colnames()


# clean variables

## date
gles_p_long$date_clean <- gles_p_long$date_new
gles_p_long$date_clean[gles_p_long$date_clean == ''] <- NA
gles_p_long$date_clean[gles_p_long$date_clean == '-95'] <- NA
gles_p_long$date_clean[gles_p_long$date_clean == '-95 nicht teilgenommen'] <- NA
gles_p_long$date_clean <- as.Date(gles_p_long$date_clean)

## media vars
for (varname in media_vars){
  varname_clean <- paste0(varname, '_clean')
  varname_bin <- paste0(varname, '_bin')
  gles_p_long[,varname_clean] <- gles_p_long[varname] - 1
  gles_p_long[(gles_p_long[,varname_clean] < 0 | is.na(gles_p_long[,varname_clean])), varname_clean] <- NA
  gles_p_long[,varname_bin] <- (gles_p_long[,varname] > 1)
}

for (varname in c(newsm_vars, non_vars)){
  varname_clean <- paste0(varname, '_clean')
  gles_p_long[,varname_clean] <- gles_p_long[,varname] == 1
}

## spiegel online - how many days last week
gles_p_long[,'1702_clean'] <- gles_p_long[,'1702'] - 1
gles_p_long[gles_p_long$`1702`< 0 & !is.na(gles_p_long$`1702`),'1702_clean'] <- NA

## ideology
gles_p_long[,'1500_clean'] <- NA
gles_p_long[gles_p_long$`1500` > 0 & !is.na(gles_p_long$`1500`), '1500_clean'] <- 
  gles_p_long[gles_p_long$`1500` > 0 & !is.na(gles_p_long$`1500`),'1500'] - 1


## party preference
gles_p_long[,'190b_inter'] <- 
  ifelse(gles_p_long[,'190b'] > 0,
         gles_p_long[,'190b'],
         NA)

gles_p_long[,'190b_clean'] <- NA_character_
gles_p_long[gles_p_long$`190b_inter` == 1  & 
              !is.na(gles_p_long$`190b_inter`),'190b_clean'] <- 'CDU/CSU'
gles_p_long[gles_p_long$`190b_inter` == 4  & 
              !is.na(gles_p_long$`190b_inter`),'190b_clean'] <- 'SPD'
gles_p_long[gles_p_long$`190b_inter` == 5  & 
              !is.na(gles_p_long$`190b_inter`),'190b_clean'] <- 'FDP'
gles_p_long[gles_p_long$`190b_inter` == 6  & 
              !is.na(gles_p_long$`190b_inter`),'190b_clean'] <- 'Grüne'
gles_p_long[gles_p_long$`190b_inter` == 7  & 
              !is.na(gles_p_long$`190b_inter`),'190b_clean'] <- 'Linke'
gles_p_long[gles_p_long$`190b_inter` == 322  & 
              !is.na(gles_p_long$`190b_inter`),'190b_clean'] <- 'AfD'


## afd scalometer
gles_p_long <- 
  gles_p_long %>% 
  mutate(`430i_clean` = ifelse(`430i` > 0, `430i` - 6, NA))



## issues [2880** coding 1-5], [1130, 1290, 1210, 1411, 1250, 1500, 1090 coding 1-7]; 1140, 1220, 1300, 1-5 (issue importance)

### ego positions
ego_vars <- gles_p_long %>% select(contains("2880")) %>% colnames()

for (varname in ego_vars){
  varname_clean <- paste0(varname, '_clean')
  gles_p_long[,varname_clean] <- gles_p_long[, varname]
  gles_p_long[is.na(gles_p_long[,varname]), varname_clean] <- -1000
  gles_p_long[gles_p_long[,varname_clean] < 0, varname_clean] <- NA
  gles_p_long[,varname_clean] <- gles_p_long[,varname_clean] - 3
}

### general positions
issue_vars <- c(1130, 1290, 1210, 1411, 1250, 1500, 1090, 1260)

for (varname in issue_vars){
  varname <- as.character(varname)
  varname_clean <- paste0(varname, '_clean')
  gles_p_long[,varname_clean] <- gles_p_long[, varname]
  gles_p_long[is.na(gles_p_long[,varname]), varname_clean] <- -1000
  gles_p_long[gles_p_long[,varname_clean] < 0, varname_clean] <- NA
  gles_p_long[,varname_clean] <- gles_p_long[,varname_clean] - 4
}

salience_vars <- c(1140, 1220, 1300)

for (varname in salience_vars){
  varname <- as.character(varname)
  varname_clean <- paste0(varname, '_clean')
  gles_p_long[,varname_clean] <- gles_p_long[, varname]
  gles_p_long[is.na(gles_p_long[,varname]), varname_clean] <- -1000
  gles_p_long[gles_p_long[,varname_clean] < 0, varname_clean] <- NA
  gles_p_long[,varname_clean] <- gles_p_long[,varname_clean]*(-1) + 3
}


gles_p_long <- gles_p_long %>% 
  select(lfdn, wave, date_new, ends_with('_clean'), ends_with('_bin'))

## save
gles_p_long %>% 
  fwrite(., here('data/gles/Panel/long_cleaned.csv'))
