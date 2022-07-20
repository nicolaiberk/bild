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



for(dpa_corrected in c(T, F)){
  
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
  
  dpa_label <- 
    ifelse(
      dpa_corrected,
      "_nodpaBild",
      ""
    )

  ## prepare media estimates
  prevalence_daily <- fread(here(paste0("data/processed/bert_crime_daily", dpa_label, ".csv")))
  
  # calculate salience in relevant lag
  prevalence_daily <- 
    prevalence_daily %>% 
    group_by(paper) %>% 
    mutate(
      across(crime_label:n_tot, ~roll_sum(.x,    7, fill=NA, align = 'right', na.rm = T), .names = "{.col}_7d")
      # across(english:n_tot, ~roll_sum(.x,    30, fill=NA, align = 'right', na.rm = T), .names = "{.col}_30d")
      
    )
  
  prevalence_daily <- 
    prevalence_daily %>% 
    pivot_wider(names_from = paper, values_from = 3:ncol(prevalence_daily)) %>% 
    mutate(date_str = as.character(date_clean))
  
  ## merge
  merged_data <- 
    gles_p_long %>% 
    left_join(prevalence_daily %>% 
                select(-date_clean), by = c("date_str"))
  
  rm(gles_p_long, prevalence_daily)
  
  ## replace missings, add number of newspapers read
  merged_data <- 
    merged_data %>% 
    mutate(across(crime_label_7d_Bild:n_tot_7d_SZ, ~ifelse(is.na(.x), 0, .x))) %>% 
    mutate(
      days_read = # count n of newspapers consumed
        `1661a_clean`+
        `1661c_clean`+
        `1661d_clean`+
        `1661e_clean`+
        `1661f_clean`
    )
  
  # subset for testing
  # merged_data <-
  #   merged_data %>%
  #   ungroup %>%
  #   slice_sample(n = 100)
  
  merged_data <- 
    merged_data %>% 
    ## wide -> long
    select(lfdn:`1220_lag`, crime_label_7d_Bild:n_tot_7d_SZ, -contains("2880"), days_read) %>% 
    pivot_longer(cols = crime_label_7d_Bild:n_tot_7d_SZ, 
                 names_to = c(".value", "paper"),
                 names_pattern = "(.*_.*d)_(.*)"
    )
  
  merged_data <- 
    merged_data %>% 
    pivot_longer(cols = crime_label_7d:n_tot_7d, 
                 names_to = c(".value", "lag"),
                 names_pattern = "(.*)_(.*)d"
    )
  
  merged_data <- 
    merged_data %>% 
    # estimate individual frame exposure in past month and week
    group_by(lfdn, date_clean, lag) %>% 
    mutate(across(crime_label:n_tot,
                  ~case_when(
                    paper == 'Bild' ~ .x * `1661a_clean`/days_read,
                    paper == 'FAZ'  ~ .x * `1661c_clean`/days_read,
                    paper == 'SZ'   ~ .x * `1661d_clean`/days_read,
                    paper == 'TAZ'  ~ .x * `1661e_clean`/days_read,
                    paper == 'Welt' ~ .x * `1661f_clean`/days_read
                  ))) %>% 
    summarise(across(c(crime_label:n_tot), sum, na.rm = T))  
  
  merged_data <- 
    merged_data %>% 
    # divide by number of migration articles to get share
    mutate(across(crime_label:crime_prob, ~.x/n_mig, .names = "{.col}_share"))
  
  # define waves
  merged_data <- 
    merged_data %>% 
    # mutate(date_clean = date_clean.x) %>%
    # date_clean >= as.Date("2017-07-20") & date_clean <=  as.Date("2017-08-09)" ~ "RW",  #this date differs for sample B
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
    left_join(gles_p_long, by = c("lfdn", "wave"))
  
  
  merged_data <- 
    merged_data %>%
    ungroup() %>% 
    arrange(lfdn, date_clean.x) %>% 
    group_by(lfdn) %>% 
    filter(!is.na(crime_label_share)) %>% 
    mutate(crime_label_share_lag = lag(crime_label_share),
           mig_att_lag = lag(`1130_clean`))
  
  merged_data %>%  
    fwrite(., file = here(paste0("data/processed/merged_bert_", dpa_label, ".csv")))
  
}