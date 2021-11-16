# ______________________________________________
# Effect of framing on issue attitudes
# Goal: Show importance of newspapers 
# Procedure: load data, transform, visualise
# ______________________________________________
# Date:  Mon Nov 15 15:19:26 2021
# Author: Nicolai Berk
#  R version 4.1.1 (2021-08-10)
# ______________________________________________


# Setup ####
library(tidyverse)
library(dplyr)
library(here)
library(data.table)

# load and transform data ####

## print
for (file in list.files('data/auflage')){
  if(grepl('print', file)){
  temp <- fread(paste0('data/auflage/', file))
    if (exists('auflage')){
      auflage <- 
        temp %>% 
        rbind(auflage)
    }else{
      auflage <- temp
    }
  }
}

auflage <- 
  auflage %>% 
  filter(Zusatz == '') %>% 
  mutate(Paper = ifelse(Titel == 'BILD/B.Z. Deutschland-Gesamt', 'Bild', Titel)) %>% 
  mutate(Paper = ifelse(Titel == 'WELT Print Gesamt (WELT/WamS/WamS Kompakt)', 'Welt', Paper)) %>%
  mutate(Paper = ifelse(Titel == 'taz.die tageszeitung gesamt', 'taz', Paper)) %>% 
  mutate(Sales = Verkauf) %>% 
  mutate(Quarter = Quartal)

## online
online <- readxl::read_xlsx('data/auflage/online_merge.xlsx') %>% 
  filter(year == '2017') %>% 
  mutate(Quarter = as.numeric(paste0(year, round((month+1)/3)))) %>% 
  group_by(Quarter, paper) %>% 
  summarise(Impressions = mean(pis),
            news_pis = mean(news_pis)) %>% 
  mutate(Paper = paper) %>% 
  mutate(News = round(news_pis/Impressions, 2))


# visualise ####
auflage %>%  
  ggplot(aes(x = Quarter, col = Paper, y = Sales)) +
  geom_line()
ggsave(filename = 'paper/vis/auflage_print.png')

online %>% 
  ggplot(aes(x = Quarter, y = Impressions/1000000, col = Paper)) +
  geom_line()
ggsave(filename = 'paper/vis/auflage_online.png')

# merge and table ####
auflage_fin <- 
  online %>% 
  mutate(Paper = ifelse(Paper == 'bild', 'Bild', Paper)) %>% 
  full_join(auflage, by = c('Paper', 'Quarter'))

auflage_fin %>%  
  mutate(Year = str_sub(Quarter, 1, 4)) %>%
  group_by(Paper, Year) %>% 
  summarise(Sales = sum(Sales),
            Impressions = sum(Impressions),
            News = round(mean(News), 2)) %>% 
  select(Paper, Sales, Impressions, News) %>% 
  arrange(Paper) %>% 
  stargazer::stargazer(summary = F, rownames = F)
