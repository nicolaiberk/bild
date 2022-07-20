# ______________________________________________
# Quick assessment of BERT Crime Estimates
# ______________________________________________
# Date:  Mon May 09 11:49:57 2022
# Author: Nicolai Berk
#  R version 4.1.1 (2021-08-10)
# ______________________________________________


# Setup ####
library(tidyverse)
library(dplyr)
library(here)
library(data.table)

envlist <- c()

# load data, describe ####
articles <- fread(here('data/processed/bert_crime_raw.csv')); envlist <- c("articles")

articles$crime_label %>% 
  table() %>% 
  prop.table() %>% 
  round(2)

# summary(articles)

# clean ####

# fix probs var: IS label prob, SHOULD crime prob

articles <- 
  articles %>% 
  mutate(
    label_prob = crime_prob,
    crime_prob = 
      ifelse(
        crime_label == 1,
        crime_prob,
        1 - crime_prob),
    
    # add dpa indicator
    dpa = str_detect(text, "(dpa)")
  )


# date

articles %>% 
  group_by(paper) %>% 
  slice(1) %>% 
  select(paper, date)

## fix
articles <- 
  articles %>% 
    mutate(
      date_clean = 
        case_when(
          grepl("\\.", date) ~ as.Date(date, format = "%d.%m.%Y"),
          grepl("-", date) ~ as.Date(date, format = "%Y-%m-%d"),
          stringr::str_count(date) == 5 ~ as.Date(as.integer(date), origin = "1970-01-01")
        )
      ) %>% 
  mutate(paper = 
           case_when(paper %in% c("weltonline", "welt") ~ "Welt",
                     paper == "taz"  ~ "TAZ",
                     paper == "spon" ~ "Spiegel",
                     paper == "faz"  ~ "FAZ",
                     paper == "sz"   ~ "SZ",
                     paper == "bild" ~ "Bild")) %>% 
  filter(paper != "Spiegel") %>%  # as Spiegel not part of the analysis
  filter(date_clean < as.Date("2020-01-01")) # data only collected until end of 2019, some dates are off
  
  
envlist <- c(envlist, "bert_ests")

# check missings produced (fine, scraping wasn't fully perfect but small n of missings)
# articles %>% filter(is.na(date_clean)) %>% select(date) %>% unique()
# articles %>% filter(is.na(date_clean)) %>% select(paper, date) %>% table()


fwrite(articles, 'data/processed/bert_crime_clean.csv')

rm(envlist)
