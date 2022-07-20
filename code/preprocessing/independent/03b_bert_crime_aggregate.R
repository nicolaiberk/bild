
library(tidyverse)
library(dplyr)
library(here)
library(data.table)

# get number of news articles total from original data
load(here("data/salience_cleaned.csv"))

salience <- 
  salience_raw %>% 
  mutate(date_clean = as.IDate(date_new),
         paper = 
           case_when(
             paper == "bild" ~ "Bild",
             paper == "faz"  ~ "FAZ",
             paper == "spon" ~ "Spiegel",
             paper == "sz"   ~ "SZ",
             paper == "taz"  ~ "TAZ",
             paper == "welt" ~ "Welt",
           )) %>% 
  group_by(paper, date_clean) %>% 
  summarise(n_tot = n()) #; rm(salience_raw)

# aggregate BERT estimates to daily measure
fread(here("data/processed/bert_crime_clean.csv")) %>% 
  select(date_clean, paper, crime_label, crime_prob, dpa) %>% 
  group_by(date_clean, paper) %>% 
  mutate(n_mig = 1) %>% 
  summarise_all(sum) %>% 
  left_join(salience, by = c("paper", "date_clean")) %>% 
  mutate(mig_share = n_mig/n_tot) %>% 
  fwrite(here("data/processed/bert_crime_daily.csv"))

# aggregate BERT estimates to daily measure - exclude Bild-dpa
fread(here("data/processed/bert_crime_clean.csv")) %>% 
  filter(paper != "Bild" | !dpa) %>% 
  select(date_clean, paper, crime_label, crime_prob) %>% 
  group_by(date_clean, paper) %>% 
  mutate(n_mig = 1) %>% 
  summarise_all(sum) %>% 
  left_join(salience, by = c("paper", "date_clean")) %>% 
  mutate(mig_share = n_mig/n_tot) %>% 
  fwrite(here("data/processed/bert_crime_daily_nodpaBild.csv"))
