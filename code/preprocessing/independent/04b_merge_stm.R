# ______________________________________________
# Media effects Bild
# Goal: merge survey and stm data
# ______________________________________________
# Date:  Tue Feb 15 08:40:42 2022
# Author: Nicolai Berk
#  R version 4.1.1 (2021-08-10)
# ______________________________________________


# Setup ####
library(tidyverse)
library(dplyr)
library(here)
library(data.table)
library(RcppRoll)


gles_p_long <- fread(here('data/raw/gles/Panel/long_cleaned.csv'))

## generate lagged dependent
gles_p_long <- 
  gles_p_long %>% 
  filter(!is.na(date_clean)) %>% 
  select(-contains("2880")) %>% 
  group_by(lfdn) %>% 
  dplyr::arrange(date_clean, .by_group = T) %>% 
  mutate(
    `1130_lag` = dplyr::lag(`1130_clean`),
    `1210_lag` = dplyr::lag(`1210_clean`),
    `1140_lag` = dplyr::lag(`1140_clean`),
    `1220_lag` = dplyr::lag(`1220_clean`),
    `430i_lag` = dplyr::lag(`430i_clean`),
    date_str = as.character(date_clean)
  )

## prepare media estimates
load(here("data/processed/stm_daily.Rdata"))

# calculate salience in relevant lag
merged_media <- 
  merged_media %>% 
  select(date_new:share_mig, 
         sexual_assault, crime_schlepp,
         sexual_assault_share, crime_schlepp_share, n_mig_fra) %>% 
  group_by(paper) %>% 
  mutate(
    across(n_mig_sal:n_mig_fra, ~roll_sum(.x,    7, fill=NA, align = 'right', na.rm = T), .names = "{.col}_7d")
    # across(english:n_articles, ~roll_sum(.x,    30, fill=NA, align = 'right', na.rm = T), .names = "{.col}_30d")
    
  )

merged_media <- 
  merged_media %>% 
  pivot_wider(names_from = paper, values_from = 3:ncol(merged_media)) %>% 
  mutate(date_str = as.character(date_new))

## merge
merged_data <- 
  gles_p_long %>% 
  select(lfdn, wave, contains("1661"), date_str, date_clean) %>% 
  left_join(merged_media %>% 
              select(-date_new), by = c("date_str"))

rm(gles_p_long, merged_media)

## replace missings, add number of newspapers read
merged_data <- 
  merged_data %>% 
  mutate(across(n_mig_sal_7d_Bild:n_mig_fra_7d_SZ, ~ifelse(is.na(.x), 0, .x))) %>% 
  mutate(
    days_read = # count n of newspapers consumed
      `1661a_clean`+
      `1661c_clean`+
      `1661d_clean`+
      `1661e_clean`+
      `1661f_clean`
  )

# merged_data <-
#   merged_data %>%
#   ungroup %>%
#   slice_sample(n = 100)

merged_data <- 
  merged_data %>% 
  ## wide -> long
  select(lfdn, wave, contains("1661"), date_clean,
         n_mig_sal_7d_Bild:n_mig_fra_7d_Welt, 
         days_read) %>% 
  pivot_longer(cols = n_mig_sal_7d_Bild:n_mig_fra_7d_Welt, 
               names_to = c(".value", "paper"),
               names_pattern = "(.*_.*d)_(.*)"
  )

merged_data <- 
  merged_data %>% 
  pivot_longer(cols = n_mig_sal_7d:n_mig_fra_7d, 
               names_to = c(".value", "lag"),
               names_pattern = "(.*)_(.*)d"
  )

merged_data <- 
  merged_data %>% 
  # estimate individual frame exposure in past month and week
  group_by(lfdn, date_clean, lag) %>% 
  filter(paper != "Spiegel") %>% # spiegel data does not fit other question format (only asking for online)
  mutate(across(n_mig_sal:n_mig_fra,
                ~case_when(
                  paper == 'Bild' ~ .x * `1661a_clean`/days_read,
                  paper == 'FAZ'  ~ .x * `1661c_clean`/days_read,
                  paper == 'SZ'   ~ .x * `1661d_clean`/days_read,
                  paper == 'TAZ'  ~ .x * `1661e_clean`/days_read,
                  paper == 'Welt' ~ .x * `1661f_clean`/days_read
                ))) %>% 
  summarise(across(c(n_mig_sal:n_mig_fra), sum, na.rm = T))  

# divide by number of migration articles to get share
merged_data <-
  merged_data %>% 
  mutate(across(n_mig_sal:crime_schlepp, ~.x/n_mig_fra, .names = "{.col}_share"))

# define waves
merged_data <- 
  merged_data %>% 
  mutate(wave = case_when(
    (date_clean >= as.Date("2016-10-06")) & (date_clean <=  as.Date("2016-11-10")) ~ "1",
    (date_clean >= as.Date("2017-02-16")) & (date_clean <=  as.Date("2017-03-03")) ~ "2",
    (date_clean >= as.Date("2017-05-11")) & (date_clean <=  as.Date("2017-05-23")) ~ "3",
    (date_clean >= as.Date("2017-07-06")) & (date_clean <=  as.Date("2017-07-17")) ~ "4",
    (date_clean >= as.Date("2017-07-20")) & (date_clean <=  as.Date("2017-08-09")) ~ "a1",
    (date_clean >= as.Date("2017-08-17")) & (date_clean <=  as.Date("2017-08-28")) ~ "5",
    (date_clean >= as.Date("2017-09-04")) & (date_clean <=  as.Date("2017-09-13")) ~ "6",
    (date_clean >= as.Date("2017-09-18")) & (date_clean <=  as.Date("2017-09-23")) ~ "7",
    (date_clean >= as.Date("2017-09-27")) & (date_clean <=  as.Date("2017-10-09")) ~ "8",
    (date_clean >= as.Date("2018-03-15")) & (date_clean <=  as.Date("2018-03-26")) ~ "9", 
    (date_clean >= as.Date("2018-11-06")) & (date_clean <=  as.Date("2019-01-31")) ~ "10",
    (date_clean >= as.Date("2019-05-28")) & (date_clean <=  as.Date("2019-07-08")) ~ "11",
    (date_clean >= as.Date("2019-11-05")) & (date_clean <=  as.Date("2019-12-17")) ~ "12",
    (date_clean >= as.Date("2020-04-21")) & (date_clean <=  as.Date("2020-06-01")) ~ "13",
    (date_clean >= as.Date("2020-09-21")) & (date_clean <=  as.Date("2020-11-02")) ~ "a2"
  )) 

gles_p_long <- 
  fread(here('data/raw/gles/Panel/long_cleaned.csv'))

merged_data <- 
  merged_data %>% 
  mutate(datetime = as.character(date_clean)) %>% 
  select(lfdn, wave, n_mig_sal:n_mig_fra_share) %>% 
  left_join(gles_p_long, by = c("lfdn", "wave"))

# define crime_label_share
merged_data <- 
  merged_data %>% 
  mutate(crime_label_share = sexual_assault_share + crime_schlepp_share)

# add lagged crime_label_share and dv
merged_data <- 
  merged_data %>%
  ungroup() %>% 
  arrange(lfdn, date_clean) %>% 
  group_by(lfdn) %>% 
  filter(!is.na(crime_label_share)) %>% 
  mutate(crime_label_share_lag = lag(crime_label_share),
         mig_att_lag = lag(`1130_clean`))

merged_data %>%  
  fwrite(., file = here("data/processed/merged_stm.csv"))
