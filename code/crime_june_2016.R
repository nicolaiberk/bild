# ______________________________________________
# Bild
# Goal: Explain peak in June
# ______________________________________________
# Date:  Fri May 13 18:09:21 2022
# Author: Nicolai Berk
#  R version 4.1.1 (2021-08-10)
# ______________________________________________


# Setup ####
library(tidyverse)
library(dplyr)
library(here)
library(data.table)

bert <- fread(here("data/processed/bert_crime_clean.csv"), encoding = "UTF-8")

bert %>%
  mutate(nberg = grepl(text, pattern = ".*nürnberg.*", ignore.case = T),
         tumult = grepl(text, pattern = ".*tumult.*", ignore.case = T),
         afghane = grepl(text, pattern = ".*afghane.*", ignore.case = T),
         arnschwang = grepl(text, pattern = ".*arnschwang.*", ignore.case = T)) -> bert

bert %>%
  mutate(date_clean = lubridate::floor_date(date_clean, "month")) %>%
  mutate(paper = paper == "Bild") %>% 
  group_by(paper, date_clean) %>%
  summarise(
    crime_share = mean(crime_label),
    afghane = mean(afghane),
    tumult = mean(tumult),
    nberg = mean(nberg),
    arnschwang = mean(arnschwang)
  ) -> bert_month

bert_month %>%
  filter(date_clean >= as.Date("2017-01-01"), date_clean < as.Date("2018-01-10")) %>%
  ggplot(aes(x = date_clean)) +
  geom_line(aes(y = arnschwang), col = "violet", lty = 5) +
  geom_line(aes(y = nberg), col = "black", lty = 4) +
  geom_line(aes(y = tumult), col = "green", lty = 3) +
  geom_line(aes(y = afghane), col = "red", lty = 2) +
  geom_line(aes(y = crime_share), col = "blue", lty = 1) +
  facet_wrap(~paper)

