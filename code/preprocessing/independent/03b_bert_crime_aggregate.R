
library(tidyverse)
library(dplyr)
library(here)
library(data.table)

# aggregate BERT estimates to daily measure
prevalence <- fread(here("data/processed/bert_crime_clean.csv"))


prevalence <- 
  prevalence %>% 
  select(date_clean, paper, crime_label, crime_prob) %>% 
  group_by(date_clean, paper) %>% 
  mutate(n_mig = 1) %>% 
  summarise_all(sum)

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

prevalence <- 
  prevalence %>% 
  left_join(salience, by = c("paper", "date_clean")) %>% 
  mutate(mig_share = n_mig/n_tot)


fwrite(prevalence, here("data/processed/bert_crime_daily.csv"))
