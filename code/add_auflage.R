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
library(glue)

# load and transform data ####

rawlink <- "https://ivw.de/aw/print/qa/titel/{ivw_title}/export?quartal%5B20131%5D=20131&quartal%5B20132%5D=20132&quartal%5B20133%5D=20133&quartal%5B20134%5D=20134&quartal%5B20141%5D=20141&quartal%5B20142%5D=20142&quartal%5B20143%5D=20143&quartal%5B20144%5D=20144&quartal%5B20151%5D=20151&quartal%5B20152%5D=20152&quartal%5B20153%5D=20153&quartal%5B20154%5D=20154&quartal%5B20161%5D=20161&quartal%5B20162%5D=20162&quartal%5B20163%5D=20163&quartal%5B20164%5D=20164&quartal%5B20171%5D=20171&quartal%5B20172%5D=20172&quartal%5B20173%5D=20173&quartal%5B20174%5D=20174&quartal%5B20181%5D=20181&quartal%5B20182%5D=20182&quartal%5B20183%5D=20183&quartal%5B20184%5D=20184&quartal%5B20191%5D=20191&quartal%5B20192%5D=20192&quartal%5B20193%5D=20193&quartal%5B20194%5D=20194&quartal%5B20201%5D=20201&quartal%5B20202%5D=20202&quartal%5B20203%5D=20203&quartal%5B20204%5D=20204&quartal%5B20211%5D=20211&quartal%5B20212%5D=20212&quartal%5B20213%5D=20213"

## print
for (ivw_title in c('7110', '6751', '122', '1056', '1221', '1335')){
  temp <- fread(glue(rawlink))
  if (exists('auflage')){
    auflage <- 
      temp %>% 
      rbind(auflage, fill = T)
  }else{
    auflage <- temp
  }
}



auflage <- 
  auflage %>% 
  filter(Zusatz == '') %>% 
  filter(Korrektur_KZ != 'VK' | is.na(Korrektur_KZ)) %>%
  mutate(Paper = ifelse(Titel == 'BILD/B.Z. Deutschland-Gesamt', 'Bild', Titel)) %>% 
  mutate(Paper = ifelse(Titel == 'WELT Print Gesamt (WELT/WamS/WamS Kompakt)', 'Welt', Paper)) %>%
  mutate(Paper = ifelse(Titel == 'taz.die tageszeitung gesamt', 'taz', Paper)) %>% 
  mutate(Sales = Verkauf) %>% 
  mutate(Quarter = Quartal) %>% 
  mutate(Auflage = `Druckauflage gesamt`) %>% 
  mutate(Jahr = as.numeric(substr(Quartal, 1, 4))) %>% 
  mutate(Monat = as.numeric(substr(Quartal, 5, 5))*3-2) %>% 
  mutate(Date = as.Date(paste("01", Monat, Jahr), format = "%d %m %Y"))


save(auflage, file = "data/papers_dist.RDta")

## online
online <- readxl::read_xlsx('data/auflage/aggregate/online_merge.xlsx') %>% 
  mutate(Date = as.Date(paste(01, month, year), format = "%d %m %Y")) %>%
  group_by(Date, paper) %>% 
  summarise(Impressions = mean(pis),
            news_pis = mean(news_pis)) %>% 
  mutate(Paper = paper) %>% 
  mutate(News = round(news_pis/Impressions, 2))


# visualise ####
auflage %>%  
  group_by(Jahr, Paper) %>%
  summarise(Sales = mean(Sales)) %>%
  ggplot(aes(x = as.Date(paste(01, 01, Jahr), format = "%d %m %Y"), 
             col = Paper, y = Sales)) +
  geom_line()
ggsave(filename = 'paper/vis/auflage_print.png')

online %>% 
  ggplot(aes(x = Date, y = Impressions/1000000, col = Paper)) +
  geom_line()
ggsave(filename = 'paper/vis/auflage_online.png')

