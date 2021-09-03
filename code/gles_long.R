
# wide -> long (gles panel)

library(here)
library(dplyr)
library(tidyverse)
library(data.table)
library(haven)
library(lubridate)
library(glue)
library(naniar)

gles_p <- read_dta(here('data/gles/Panel/GLESMerge/ZA6838_allwaves_sA_v4-0-0.dta'))

## id var
gles_p$lfdn <- as.factor(gles_p$lfdn)


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



## save
fwrite(gles_p_long, file = here('data/gles/Panel/long.csv'))
