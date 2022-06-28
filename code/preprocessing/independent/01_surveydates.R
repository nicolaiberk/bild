# ______________________________________________
# Function preparing survey dates for treatment plot
# ______________________________________________
# Date:  Mon May 09 20:45:26 2022
# Author: Nicolai Berk
#  R version 4.1.1 (2021-08-10)
# ______________________________________________

library(tidyverse)
library(dplyr)
library(data.table)


## define survey dates
gles_p_long <- 
  fread(here('data/raw/gles/Panel/long_cleaned.csv')); envlist <- c("gles_p_long")

survey_dates <- 
  gles_p_long %>% 
  filter(!is.na(date_clean)) %>% 
  filter(!is.na(`1130_clean`)) %>%  
  # filter(date_clean < as.Date("2018-01-01")) %>% 
  group_by(wave) %>% 
  summarise(date = max(date_clean)) %>% 
  ungroup() %>% 
  select(date); envlist <- c(envlist, "survey_dates")

fwrite(survey_dates, file = "data/processed/SurveyDates.csv")

rm(envlist)
